#include "bjvm.h"
#include "classfile.h"

#include <math.h>
#include <tgmath.h>

// Interpreter 2 naming conventions:
//
// There are implementations for TOS types of int/long/reference, float, and double. Some instruction/TOS
// combinations are illegal and are thus omitted. The execution of stack polymorphic
// instructions like pop will consult the TOS stack type that the instruction is annotated with, before
// loading the correct type from the stack and using the appropriate jump table.
//
// The general signature is (frame, insns, pc, sd, tos). At appropriate points (whenever the frame might be
// read back, e.g. for GC purposes, or when interrupting), the TOS value and

#define ARGS bjvm_thread *thread, bjvm_plain_frame *frame, bjvm_bytecode_insn *insns, int pc, int sd
#define ARGS_TOS(tos_type) ARGS, tos_type tos

#define insn (&insns[pc])

#define MUSTTAIL __attribute((musttail))
#define MAX_INSN_KIND (bjvm_insn_putstatic_L + 1)

// Used when the TOS is int (i.e., the stack is empty)
static bjvm_stack_value (*jmp_table_void[MAX_INSN_KIND])(ARGS);
// Used when the TOS is int, long, or a reference (wasm signature: i64). In the int case, the result is sign-extended;
// in the reference case, the result is zero-extended.
static bjvm_stack_value (*jmp_table_int[MAX_INSN_KIND])(ARGS_TOS(int64_t));
// Used when the TOS is float (wasm signature: f32)
static bjvm_stack_value (*jmp_table_float[MAX_INSN_KIND])(ARGS_TOS(float));
// Used when the TOS is double (wasm signature: f64)
static bjvm_stack_value (*jmp_table_double[MAX_INSN_KIND])(ARGS_TOS(double));

#define SELECT_TABLE(tos) _Generic(tos, \
  bjvm_obj_header *: jmp_table_int, \
  int: jmp_table_int, \
  int64_t: jmp_table_int, \
  float: jmp_table_float, \
  double: jmp_table_double \
)

#define JMP(tos) MUSTTAIL return SELECT_TABLE(tos)[insns[pc].kind](thread, frame, insns, pc, sd, tos);
#define NEXT(tos) MUSTTAIL return SELECT_TABLE(tos)[insns[pc].kind](thread, frame, insns, pc + 1, sd, tos);
#define JMP_VOID MUSTTAIL return jmp_table_int[insns[pc].kind](thread, frame, insns, pc, sd);
#define NEXT_VOID MUSTTAIL return jmp_table_void[insns[pc].kind](thread, frame, insns, pc + 1, sd);

// Spill all the information currently in locals/registers to the frame (required at safepoints and when interrupting)
#define SPILL(tos) \
  frame->program_counter = pc; \
  frame->values[sd - 1] = _Generic(tos, \
    int64_t: (bjvm_stack_value) { .l = tos }, \
    float: (bjvm_stack_value) { .f = tos }, \
    double: (bjvm_stack_value) { .d = tos }, \
    bjvm_obj_header *: (bjvm_stack_value) { .obj = tos } \
  );

// Reload the top of stack type -- used after an instruction which may have instigated a GC
#define RELOAD(tos) \
  tos = _Generic(tos, \
    int64_t: frame->values[sd - 1].l, \
    float: frame->values[sd - 1].f, \
    double: frame->values[sd - 1].d, \
    bjvm_obj_header *: frame->values[sd - 1].obj \
  );

#define STACK_POLYMORPHIC_NEXT(tos) \
  switch (insn->tos_after) { \
    case TOS_VOID: NEXT_VOID \
    case TOS_INT: NEXT(tos.l) \
    case TOS_FLOAT: NEXT(tos.f) \
    case TOS_DOUBLE: NEXT(tos.d) \
    default: __builtin_unreachable(); \
}

// For a bytecode that takes no arguments, given an implementation for the int TOS type, generate adapter functions
// which push the current TOS value onto the stack and then call the int TOS implementation.
#define FORWARD_TO_NULLARY(which) \
static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
frame->values[sd - 1] = (bjvm_stack_value) { .l = tos }; \
MUSTTAIL return which##_impl_void(thread, frame, insns, pc, sd); \
} \
\
static bjvm_stack_value which##_impl_float(ARGS_TOS(float)) { \
frame->values[sd - 1] = (bjvm_stack_value) { .f = tos }; \
MUSTTAIL return which##_impl_void(thread, frame, insns, pc, sd); \
} \
\
static bjvm_stack_value which##_impl_double(ARGS_TOS(double)) {\
frame->values[sd - 1] = (bjvm_stack_value) { .d = tos };\
MUSTTAIL return which##_impl_void(thread, frame, insns, pc, sd);\
}


int32_t java_idiv(int32_t a, int32_t b) {
  assert(b != 0);
  if (a == INT_MIN && b == -1)
    return INT_MIN;
  return a / b;
}

int64_t java_irem(int32_t a, int32_t b) {
  assert(b != 0);
  if (a == INT_MIN && b == -1)
    return 0;
  return a % b;
}

int64_t java_ldiv(int64_t a, int64_t b) {
  assert(b != 0);
  if (a == LONG_MIN && b == -1)
    return LONG_MIN;
  return a / b;
}

int64_t java_lrem(int64_t a, int64_t b) {
  assert(b != 0);
  if (a == LONG_MIN && b == -1)
    return 0;
  return a % b;
}

/** BYTECODE IMPLEMENTATIONS */

/**
 * getstatic/putstatic/getfield/putfield
 *
 * getstatic:
 *   ... -> ..., value
 * putstatic:
 *   ..., value -> ...
 * getfield:
 *   ..., obj -> ..., value
 * putfield:
 *   ..., obj, value -> ...
 */

static bjvm_stack_value getstatic_L_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT(*(bjvm_obj_header **)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_L)

static bjvm_stack_value getstatic_F_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT(*(float *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_F)

static bjvm_stack_value getstatic_D_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT(*(double *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_D)

static bjvm_stack_value getstatic_J_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT(*(int64_t *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_J)

static bjvm_stack_value getstatic_I_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT(*(int *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_I)

static bjvm_stack_value getstatic_S_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT((int64_t)*(int16_t *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_S)

static bjvm_stack_value getstatic_C_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT((int64_t)*(uint16_t *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_C)

static bjvm_stack_value getstatic_B_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT((int64_t)*(int8_t *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_B)

static bjvm_stack_value getstatic_Z_impl_void(ARGS) {
  assert(insn->ic && "Static field location not found");
  sd++;
  NEXT((int64_t)*(int8_t *)insn->ic)
}
FORWARD_TO_NULLARY(getstatic_Z)

static bjvm_stack_value putstatic_B_impl_int(ARGS_TOS(int64_t)) {
  *(int8_t *)insn->ic = (int8_t)tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_S_impl_int(ARGS_TOS(int64_t)) {
  *(int16_t *)insn->ic = (int16_t)tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_I_impl_int(ARGS_TOS(int64_t)) {
  *(int *)insn->ic = (int)tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_J_impl_int(ARGS_TOS(int64_t)) {
  *(int64_t *)insn->ic = tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_F_impl_float(ARGS_TOS(float)) {
  *(float *)insn->ic = tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_D_impl_double(ARGS_TOS(double)) {
  *(double *)insn->ic = tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_L_impl_int(ARGS_TOS(int64_t)) {
  *(bjvm_obj_header **)insn->ic = (bjvm_obj_header *)tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value putstatic_Z_impl_int(ARGS_TOS(int64_t)) {
  *(int8_t *)insn->ic = (int8_t)tos;
  STACK_POLYMORPHIC_NEXT(frame->values[--sd]);
}

static bjvm_stack_value getfield_B_impl_int(ARGS_TOS(int64_t)) {
  int8_t *field = (int8_t *)((char *)tos + insn->index);
  NEXT((int64_t)*field)
}

static bjvm_stack_value getfield_S_impl_int(ARGS_TOS(int64_t)) {
  int16_t *field = (int16_t *)((char *)tos + insn->index);
  NEXT((int64_t)*field)
}

static bjvm_stack_value getfield_I_impl_int(ARGS_TOS(int64_t)) {
  int *field = (int *)((char *)tos + insn->index);
  NEXT((int64_t)*field)
}

static bjvm_stack_value getfield_J_impl_int(ARGS_TOS(int64_t)) {
  int64_t *field = (int64_t *)((char *)tos + insn->index);
  NEXT(*field)
}

static bjvm_stack_value getfield_F_impl_int(ARGS_TOS(int64_t)) {
  float *field = (float *)((char *)tos + insn->index);
  NEXT(*field)
}

static bjvm_stack_value getfield_D_impl_int(ARGS_TOS(int64_t)) {
  double *field = (double *)((char *)tos + insn->index);
  NEXT(*field)
}

static bjvm_stack_value getfield_L_impl_int(ARGS_TOS(int64_t)) {
  bjvm_obj_header **field = (bjvm_obj_header **)((char *)tos + insn->index);
  NEXT(*field)
}

static bjvm_stack_value getfield_Z_impl_int(ARGS_TOS(int64_t)) {
  int8_t *field = (int8_t *)((char *)tos + insn->index);
  NEXT((int64_t)*field)
}

static bjvm_stack_value putfield_B_impl_int(ARGS_TOS(int64_t)) {
  int8_t *field = (int8_t *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (int8_t)tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_S_impl_int(ARGS_TOS(int64_t)) {
  int16_t *field = (int16_t *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (int16_t)tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_I_impl_int(ARGS_TOS(int64_t)) {
  int *field = (int *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (int)tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_J_impl_int(ARGS_TOS(int64_t)) {
  int64_t *field = (int64_t *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_F_impl_float(ARGS_TOS(float)) {
  float *field = (float *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_D_impl_double(ARGS_TOS(double)) {
  double *field = (double *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

/** Arithmetic operations */

// Binary operation on two integers (ints or longs)
#define INTEGER_BIN_OP(which, eval) \
static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
  int64_t a = frame->values[sd - 2].i, b = tos; \
  int64_t result = eval; \
  sd--; \
  NEXT(result) \
}

INTEGER_BIN_OP(iadd, (int)((uint32_t)a + (uint32_t)b))
INTEGER_BIN_OP(ladd, (uint64_t)a + (uint64_t)b)
INTEGER_BIN_OP(isub, (int)((uint32_t)a - (uint32_t)b))
INTEGER_BIN_OP(lsub, (uint64_t)a - (uint64_t)b)
INTEGER_BIN_OP(imul, (int)((uint32_t)a * (uint32_t)b))
INTEGER_BIN_OP(lmul, (uint64_t)a * (uint64_t)b)
INTEGER_BIN_OP(iand, (int)(a & b))
INTEGER_BIN_OP(land, a & b)
INTEGER_BIN_OP(ior, (int)(a | b))
INTEGER_BIN_OP(lor, a | b)
INTEGER_BIN_OP(ixor, (int)(a ^ b))
INTEGER_BIN_OP(lxor, a ^ b)
INTEGER_BIN_OP(ishl, (int)((uint32_t)a << (b & 0x1f)))
INTEGER_BIN_OP(lshl, (uint64_t)a << (b & 0x3f))
INTEGER_BIN_OP(ishr, (int)a >> (b & 0x1f))
INTEGER_BIN_OP(lshr, a >> (b & 0x3f))
INTEGER_BIN_OP(iushr, (uint32_t)a >> (b & 0x1f))
INTEGER_BIN_OP(lushr, (uint64_t)a >> (b & 0x3f))

#define INTEGER_UN_OP(which, eval) \
static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
  int64_t a = tos; \
  NEXT(eval) \
}

INTEGER_UN_OP(ineg, (int)(-(uint32_t)a))
INTEGER_UN_OP(lneg, (int64_t)(-(uint64_t)a))
INTEGER_UN_OP(i2l, (int64_t)a)
INTEGER_UN_OP(i2s, (int64_t)(int16_t)a)
INTEGER_UN_OP(i2b, (int64_t)(int8_t)a)
INTEGER_UN_OP(i2c, (int64_t)(uint16_t)a)
INTEGER_UN_OP(i2f, (float)a)
INTEGER_UN_OP(i2d, (double)a)
INTEGER_UN_OP(l2i, (int)a)
INTEGER_UN_OP(l2f, (float)a)
INTEGER_UN_OP(l2d, (double)a)

#define FLOAT_BIN_OP(which, eval) \
  static bjvm_stack_value f##which##_impl_float(ARGS_TOS(float)) { \
    float a = frame->values[sd - 2].f, b = tos; \
    float result = eval; \
    sd--; \
    NEXT(result) \
} \
static bjvm_stack_value d##which##_impl_double(ARGS_TOS(double)) { \
double a = frame->values[sd - 2].f, b = tos; \
double result = eval; \
sd--; \
NEXT(result) \
}

#define FLOAT_UN_OP(which, eval) \
  static bjvm_stack_value which##_impl_float(ARGS_TOS(float)) { \
    float a = tos; \
    float result = eval; \
    NEXT(result) \
}

#define DOUBLE_UN_OP(which, eval) \
  static bjvm_stack_value which##_impl_double(ARGS_TOS(double)) { \
    double a = tos; \
    double result = eval; \
    NEXT(result) \
}

FLOAT_BIN_OP(add, a + b)
FLOAT_BIN_OP(sub, a - b)
FLOAT_BIN_OP(mul, a * b)
FLOAT_BIN_OP(div, a / b)
FLOAT_BIN_OP(rem, fmod(a, b))
FLOAT_BIN_OP(cmpg, a > b ? 1 : (a < b ? -1 : (a == b ? 0 : 1)))
FLOAT_BIN_OP(cmpl, a > b ? 1 : (a < b ? -1 : (a == b ? 0 : -1)))

FLOAT_UN_OP(fneg, -a)
FLOAT_UN_OP(f2i, (int)a)
FLOAT_UN_OP(f2l, (int64_t)a)
FLOAT_UN_OP(f2d, (double)a)

DOUBLE_UN_OP(dneg, -a)
DOUBLE_UN_OP(d2i, (int)a)
DOUBLE_UN_OP(d2l, (int64_t)a)
DOUBLE_UN_OP(d2f, (float)a)

static bjvm_stack_value idiv_impl_int(ARGS_TOS(int64_t)) {
  int a = frame->values[sd - 2].i, b = (int)tos; \
  if (unlikely(b == 0)) {
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_idiv(a, b));
}

static bjvm_stack_value ldiv_impl_int(ARGS_TOS(int64_t)) {
  int64_t a = frame->values[sd - 2].l, b = tos; \
  if (unlikely(b == 0)) {
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_ldiv(a, b));
}

static bjvm_stack_value irem_impl_int(ARGS_TOS(int64_t)) {
  int a = frame->values[sd - 2].i, b = (int)tos; \
  if (unlikely(b == 0)) {
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_irem(a, b));
}

static bjvm_stack_value lrem_impl_int(ARGS_TOS(int64_t)) {
  int64_t a = frame->values[sd - 2].l, b = tos; \
  if (unlikely(b == 0)) {
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_lrem(a, b));
}

/** Array instructions (arraylength, array loads, array stores) */

/** Constant-pushing instructions */

static bjvm_stack_value aconst_null_impl_void(ARGS) {
  sd++;
  NEXT((int64_t)0)
}
FORWARD_TO_NULLARY(aconst_null)

static bjvm_stack_value ldc_impl_void(ARGS) {
  sd++;
  bjvm_cp_entry *ent = insn->cp;
  switch (ent->kind) {
  case BJVM_CP_KIND_INTEGER:
    NEXT((int64_t)ent->integral.value);
  case BJVM_CP_KIND_FLOAT:
    NEXT((float)ent->floating.value);
  case BJVM_CP_KIND_CLASS: {
    // Initialize the class, then get its Java mirror
    if (bjvm_resolve_class(thread, &ent->class_info))
      return value_null();
    if (bjvm_link_class(thread, ent->class_info.classdesc))
      return value_null();
    bjvm_obj_header *obj = (void *)bjvm_get_class_mirror(thread, ent->class_info.classdesc);
    NEXT(obj);
  }
  case BJVM_CP_KIND_STRING: {
    bjvm_utf8 s = ent->string.chars;
    bjvm_obj_header *obj = bjvm_intern_string(thread, s);
    if (!obj)  // oom
      return value_null();
    NEXT(obj);
  }
  default:
    UNREACHABLE();
  }
}
FORWARD_TO_NULLARY(ldc)

static bjvm_stack_value ldc2_w_impl_void(ARGS) {
  sd++;
  bjvm_cp_entry *ent = insn->cp;
  switch (ent->kind) {
  case BJVM_CP_KIND_DOUBLE:
    NEXT(ent->floating.value);
  case BJVM_CP_KIND_LONG:
    NEXT(ent->integral.value);
  default:
    UNREACHABLE();
  }
}

static bjvm_stack_value iconst_impl_void(ARGS) {
  sd++;
  NEXT(insn->integer_imm)
}
FORWARD_TO_NULLARY(iconst)

static bjvm_stack_value fconst_impl_void(ARGS) {
  sd++;
  NEXT(insn->f_imm);
}
FORWARD_TO_NULLARY(fconst)

static bjvm_stack_value dconst_impl_void(ARGS) {
  sd++;
  NEXT(insn->d_imm);
}
FORWARD_TO_NULLARY(dconst)

static bjvm_stack_value lconst_impl_void(ARGS) {
  sd++;
  NEXT(insn->integer_imm);
}
FORWARD_TO_NULLARY(lconst)

/** Stack manipulation instructions */

static bjvm_stack_value pop_impl_void(ARGS) {
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}
FORWARD_TO_NULLARY(pop)

static bjvm_stack_value pop2_impl_void(ARGS) {
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}
FORWARD_TO_NULLARY(pop2)

static bjvm_stack_value nop_impl_void(ARGS) {
  NEXT_VOID
}

static bjvm_stack_value nop_impl_double(ARGS_TOS(double)) {
  NEXT(tos)
}

static bjvm_stack_value nop_impl_float(ARGS_TOS(float)) {
  NEXT(tos)
}

static bjvm_stack_value nop_impl_int(ARGS_TOS(int64_t)) {
  NEXT(tos)
}

static bjvm_stack_value dup_impl_int(ARGS_TOS(int64_t)) {
  frame->values[sd++ - 1].l = tos;
  NEXT(tos)
}

static bjvm_stack_value dup_impl_float(ARGS_TOS(float)) {
  frame->values[sd++ - 1].f = tos;
  NEXT(tos)
}

static bjvm_stack_value dup_impl_double(ARGS_TOS(double)) {
  frame->values[sd++ - 1].d = tos;
  NEXT(tos)
}

static bjvm_stack_value (*jmp_table_void[MAX_INSN_KIND])(ARGS) = {
  nop_impl_void,
  nullptr /* aaload_impl_void */,
  nullptr /* aastore_impl_void */,
  aconst_null_impl_void,
  nullptr /* areturn_impl_void */,
  nullptr /* arraylength_impl_void */,
  nullptr /* athrow_impl_void */,
  nullptr /* baload_impl_void */,
  nullptr /* bastore_impl_void */,
  nullptr /* caload_impl_void */,
  nullptr /* castore_impl_void */,
  nullptr /* d2f_impl_void */,
  nullptr /* d2i_impl_void */,
  nullptr /* d2l_impl_void */,
  nullptr /* dadd_impl_void */,
  nullptr /* daload_impl_void */,
  nullptr /* dastore_impl_void */,
  nullptr /* dcmpg_impl_void */,
  nullptr /* dcmpl_impl_void */,
  nullptr /* ddiv_impl_void */,
  nullptr /* dmul_impl_void */,
  nullptr /* dneg_impl_void */,
  nullptr /* drem_impl_void */,
  nullptr /* dreturn_impl_void */,
  nullptr /* dsub_impl_void */,
  nullptr /* dup_impl_void */,
  nullptr /* dup_x1_impl_void */,
  nullptr /* dup_x2_impl_void */,
  nullptr /* dup2_impl_void */,
  nullptr /* dup2_x1_impl_void */,
  nullptr /* dup2_x2_impl_void */,
  nullptr /* f2d_impl_void */,
  nullptr /* f2i_impl_void */,
  nullptr /* f2l_impl_void */,
  nullptr /* fadd_impl_void */,
  nullptr /* faload_impl_void */,
  nullptr /* fastore_impl_void */,
  nullptr /* fcmpg_impl_void */,
  nullptr /* fcmpl_impl_void */,
  nullptr /* fdiv_impl_void */,
  nullptr /* fmul_impl_void */,
  nullptr /* fneg_impl_void */,
  nullptr /* frem_impl_void */,
  nullptr /* freturn_impl_void */,
  nullptr /* fsub_impl_void */,
  nullptr /* i2b_impl_void */,
  nullptr /* i2c_impl_void */,
  nullptr /* i2d_impl_void */,
  nullptr /* i2f_impl_void */,
  nullptr /* i2l_impl_void */,
  nullptr /* i2s_impl_void */,
  nullptr /* iadd_impl_void */,
  nullptr /* iaload_impl_void */,
  nullptr /* iand_impl_void */,
  nullptr /* iastore_impl_void */,
  nullptr /* idiv_impl_void */,
  nullptr /* imul_impl_void */,
  nullptr /* ineg_impl_void */,
  nullptr /* ior_impl_void */,
  nullptr /* irem_impl_void */,
  nullptr /* ireturn_impl_void */,
  nullptr /* ishl_impl_void */,
  nullptr /* ishr_impl_void */,
  nullptr /* isub_impl_void */,
  nullptr /* iushr_impl_void */,
  nullptr /* ixor_impl_void */,
  nullptr /* l2d_impl_void */,
  nullptr /* l2f_impl_void */,
  nullptr /* l2i_impl_void */,
  nullptr /* ladd_impl_void */,
  nullptr /* laload_impl_void */,
  nullptr /* land_impl_void */,
  nullptr /* lastore_impl_void */,
  nullptr /* lcmp_impl_void */,
  nullptr /* ldiv_impl_void */,
  nullptr /* lmul_impl_void */,
  nullptr /* lneg_impl_void */,
  nullptr /* lor_impl_void */,
  nullptr /* lrem_impl_void */,
  nullptr /* lreturn_impl_void */,
  nullptr /* lshl_impl_void */,
  nullptr /* lshr_impl_void */,
  nullptr /* lsub_impl_void */,
  nullptr /* lushr_impl_void */,
  nullptr /* lxor_impl_void */,
  nullptr /* monitorenter_impl_void */,
  nullptr /* monitorexit_impl_void */,
  nullptr /* pop_impl_void */,
  nullptr /* pop2_impl_void */,
  return_impl_void,
  nullptr /* saload_impl_void */,
  nullptr /* sastore_impl_void */,
  nullptr /* swap_impl_void */,
  nullptr /* anewarray_impl_void */,
  nullptr /* checkcast_impl_void */,
  nullptr /* getfield_impl_void */,
  getstatic_impl_void,
  nullptr /* instanceof_impl_void */,
  invokedynamic_impl_void,
  new_impl_void,
  nullptr /* putfield_impl_void */,
  nullptr /* putstatic_impl_void */,
  nullptr /* invokevirtual_impl_void */,
  nullptr /* invokespecial_impl_void */,
  invokestatic_impl_void,
  ldc_impl_void,
  ldc2_w_impl_void,
  dload_impl_void,
  fload_impl_void,
  iload_impl_void,
  lload_impl_void,
  nullptr /* dstore_impl_void */,
  nullptr /* fstore_impl_void */,
  nullptr /* istore_impl_void */,
  nullptr /* lstore_impl_void */,
  aload_impl_void,
  nullptr /* astore_impl_void */,
  goto_impl_void,
  nullptr /* jsr_impl_void */,
  if_acmpeq_impl_void,
  if_acmpne_impl_void,
  if_icmpeq_impl_void,
  if_icmpne_impl_void,
  if_icmplt_impl_void,
  if_icmpge_impl_void,
  if_icmpgt_impl_void,
  if_icmple_impl_void,
  ifeq_impl_void,
  ifne_impl_void,
  iflt_impl_void,
  ifge_impl_void,
  ifgt_impl_void,
  ifle_impl_void,
  ifnonnull_impl_void,
  ifnull_impl_void,
  iconst_impl_void,
  dconst_impl_void,
  fconst_impl_void,
  lconst_impl_void,
  iinc_impl_void,
  nullptr /* invokeinterface_impl_void */,
  multianewarray_impl_void,
  newarray_impl_void,
  tableswitch_impl_void,
  lookupswitch_impl_void,
  nullptr /* ret_impl_void */,
  anewarray_resolved_impl_void,
  nullptr /* checkcast_resolved_impl_void */,
  nullptr /* instanceof_resolved_impl_void */,
  new_resolved_impl_void,
  nullptr /* invokevtable_monomorphic_impl_void */,
  nullptr /* invokevtable_polymorphic_impl_void */,
  nullptr /* invokeitable_monomorphic_impl_void */,
  nullptr /* invokeitable_polymorphic_impl_void */,
  nullptr /* invokespecial_resolved_impl_void */,
  invokestatic_resolved_impl_void,
  invokecallsite_impl_void,
  nullptr /* getfield_B_impl_void */,
  nullptr /* getfield_C_impl_void */,
  nullptr /* getfield_S_impl_void */,
  nullptr /* getfield_I_impl_void */,
  nullptr /* getfield_J_impl_void */,
  nullptr /* getfield_F_impl_void */,
  nullptr /* getfield_D_impl_void */,
  nullptr /* getfield_Z_impl_void */,
  nullptr /* getfield_L_impl_void */,
  nullptr /* putfield_B_impl_void */,
  nullptr /* putfield_C_impl_void */,
  nullptr /* putfield_S_impl_void */,
  nullptr /* putfield_I_impl_void */,
  nullptr /* putfield_J_impl_void */,
  nullptr /* putfield_F_impl_void */,
  nullptr /* putfield_D_impl_void */,
  nullptr /* putfield_Z_impl_void */,
  nullptr /* putfield_L_impl_void */,
  getstatic_B_impl_void,
  getstatic_C_impl_void,
  getstatic_S_impl_void,
  getstatic_I_impl_void,
  getstatic_J_impl_void,
  getstatic_F_impl_void,
  getstatic_D_impl_void,
  getstatic_Z_impl_void,
  getstatic_L_impl_void,
  nullptr /* putstatic_B_impl_void */,
  nullptr /* putstatic_C_impl_void */,
  nullptr /* putstatic_S_impl_void */,
  nullptr /* putstatic_I_impl_void */,
  nullptr /* putstatic_J_impl_void */,
  nullptr /* putstatic_F_impl_void */,
  nullptr /* putstatic_D_impl_void */,
  nullptr /* putstatic_Z_impl_void */,
  nullptr /* putstatic_L_impl_void */
};

static bjvm_stack_value (*jmp_table_double[MAX_INSN_KIND])(ARGS_TOS(double)) = {
  nop_impl_double,
  nullptr /* aaload_impl_double */,
  nullptr /* aastore_impl_double */,
  aconst_null_impl_double,
  nullptr /* areturn_impl_double */,
  nullptr /* arraylength_impl_double */,
  nullptr /* athrow_impl_double */,
  nullptr /* baload_impl_double */,
  nullptr /* bastore_impl_double */,
  nullptr /* caload_impl_double */,
  nullptr /* castore_impl_double */,
  d2f_impl_double,
  d2i_impl_double,
  d2l_impl_double,
  dadd_impl_double,
  nullptr /* daload_impl_double */,
  dastore_impl_double,
  dcmpg_impl_double,
  dcmpl_impl_double,
  ddiv_impl_double,
  dmul_impl_double,
  dneg_impl_double,
  drem_impl_double,
  dreturn_impl_double,
  dsub_impl_double,
  dup_impl_double,
  dup_x1_impl_double,
  dup_x2_impl_double,
  dup2_impl_double,
  dup2_x1_impl_double,
  dup2_x2_impl_double,
  nullptr /* f2d_impl_double */,
  nullptr /* f2i_impl_double */,
  nullptr /* f2l_impl_double */,
  nullptr /* fadd_impl_double */,
  nullptr /* faload_impl_double */,
  nullptr /* fastore_impl_double */,
  nullptr /* fcmpg_impl_double */,
  nullptr /* fcmpl_impl_double */,
  nullptr /* fdiv_impl_double */,
  nullptr /* fmul_impl_double */,
  nullptr /* fneg_impl_double */,
  nullptr /* frem_impl_double */,
  nullptr /* freturn_impl_double */,
  nullptr /* fsub_impl_double */,
  nullptr /* i2b_impl_double */,
  nullptr /* i2c_impl_double */,
  nullptr /* i2d_impl_double */,
  nullptr /* i2f_impl_double */,
  nullptr /* i2l_impl_double */,
  nullptr /* i2s_impl_double */,
  nullptr /* iadd_impl_double */,
  nullptr /* iaload_impl_double */,
  nullptr /* iand_impl_double */,
  nullptr /* iastore_impl_double */,
  nullptr /* idiv_impl_double */,
  nullptr /* imul_impl_double */,
  nullptr /* ineg_impl_double */,
  nullptr /* ior_impl_double */,
  nullptr /* irem_impl_double */,
  nullptr /* ireturn_impl_double */,
  nullptr /* ishl_impl_double */,
  nullptr /* ishr_impl_double */,
  nullptr /* isub_impl_double */,
  nullptr /* iushr_impl_double */,
  nullptr /* ixor_impl_double */,
  nullptr /* l2d_impl_double */,
  nullptr /* l2f_impl_double */,
  nullptr /* l2i_impl_double */,
  nullptr /* ladd_impl_double */,
  nullptr /* laload_impl_double */,
  nullptr /* land_impl_double */,
  nullptr /* lastore_impl_double */,
  nullptr /* lcmp_impl_double */,
  nullptr /* ldiv_impl_double */,
  nullptr /* lmul_impl_double */,
  nullptr /* lneg_impl_double */,
  nullptr /* lor_impl_double */,
  nullptr /* lrem_impl_double */,
  nullptr /* lreturn_impl_double */,
  nullptr /* lshl_impl_double */,
  nullptr /* lshr_impl_double */,
  nullptr /* lsub_impl_double */,
  nullptr /* lushr_impl_double */,
  nullptr /* lxor_impl_double */,
  nullptr /* monitorenter_impl_double */,
  nullptr /* monitorexit_impl_double */,
  pop_impl_double,
  pop2_impl_double,
  return_impl_double,
  nullptr /* saload_impl_double */,
  nullptr /* sastore_impl_double */,
  swap_impl_double,
  anewarray_impl_double,
  nullptr /* checkcast_impl_double */,
  nullptr /* getfield_impl_double */,
  getstatic_impl_double,
  nullptr /* instanceof_impl_double */,
  invokedynamic_impl_double,
  new_impl_double,
  putfield_impl_double,
  putstatic_impl_double,
  invokevirtual_impl_double,
  invokespecial_impl_double,
  invokestatic_impl_double,
  ldc_impl_double,
  ldc2_w_impl_double,
  dload_impl_double,
  fload_impl_double,
  iload_impl_double,
  lload_impl_double,
  dstore_impl_double,
  nullptr /* fstore_impl_double */,
  nullptr /* istore_impl_double */,
  nullptr /* lstore_impl_double */,
  aload_impl_double,
  nullptr /* astore_impl_double */,
  goto_impl_double,
  jsr_impl_double,
  if_acmpeq_impl_double,
  if_acmpne_impl_double,
  if_icmpeq_impl_double,
  if_icmpne_impl_double,
  if_icmplt_impl_double,
  if_icmpge_impl_double,
  if_icmpgt_impl_double,
  if_icmple_impl_double,
  ifeq_impl_double,
  ifne_impl_double,
  iflt_impl_double,
  ifge_impl_double,
  ifgt_impl_double,
  ifle_impl_double,
  ifnonnull_impl_double,
  ifnull_impl_double,
  iconst_impl_double,
  dconst_impl_double,
  fconst_impl_double,
  lconst_impl_double,
  iinc_impl_double,
  invokeinterface_impl_double,
  multianewarray_impl_double,
  newarray_impl_double,
  tableswitch_impl_double,
  lookupswitch_impl_double,
  ret_impl_double,
  anewarray_resolved_impl_double,
  nullptr /* checkcast_resolved_impl_double */,
  nullptr /* instanceof_resolved_impl_double */,
  new_resolved_impl_double,
  invokevtable_monomorphic_impl_double,
  invokevtable_polymorphic_impl_double,
  invokeitable_monomorphic_impl_double,
  invokeitable_polymorphic_impl_double,
  invokespecial_resolved_impl_double,
  invokestatic_resolved_impl_double,
  invokecallsite_impl_double,
  nullptr /* getfield_B_impl_double */,
  nullptr /* getfield_C_impl_double */,
  nullptr /* getfield_S_impl_double */,
  nullptr /* getfield_I_impl_double */,
  nullptr /* getfield_J_impl_double */,
  nullptr /* getfield_F_impl_double */,
  nullptr /* getfield_D_impl_double */,
  nullptr /* getfield_Z_impl_double */,
  nullptr /* getfield_L_impl_double */,
  nullptr /* putfield_B_impl_double */,
  nullptr /* putfield_C_impl_double */,
  nullptr /* putfield_S_impl_double */,
  nullptr /* putfield_I_impl_double */,
  nullptr /* putfield_J_impl_double */,
  nullptr /* putfield_F_impl_double */,
  putfield_D_impl_double,
  nullptr /* putfield_Z_impl_double */,
  nullptr /* putfield_L_impl_double */,
  getstatic_B_impl_double,
  getstatic_C_impl_double,
  getstatic_S_impl_double,
  getstatic_I_impl_double,
  getstatic_J_impl_double,
  getstatic_F_impl_double,
  getstatic_D_impl_double,
  getstatic_Z_impl_double,
  getstatic_L_impl_double,
  nullptr /* putstatic_B_impl_double */,
  nullptr /* putstatic_C_impl_double */,
  nullptr /* putstatic_S_impl_double */,
  nullptr /* putstatic_I_impl_double */,
  nullptr /* putstatic_J_impl_double */,
  nullptr /* putstatic_F_impl_double */,
  putstatic_D_impl_double,
  nullptr /* putstatic_Z_impl_double */,
  nullptr /* putstatic_L_impl_double */
};

static bjvm_stack_value (*jmp_table_int[MAX_INSN_KIND])(ARGS_TOS(int64_t)) = {
  nop_impl_int,
  aaload_impl_int,
  aastore_impl_int,
  aconst_null_impl_int,
  areturn_impl_int,
  arraylength_impl_int,
  athrow_impl_int,
  baload_impl_int,
  bastore_impl_int,
  caload_impl_int,
  castore_impl_int,
  d2f_impl_int,
  d2i_impl_int,
  d2l_impl_int,
  dadd_impl_int,
  daload_impl_int,
  dastore_impl_int,
  dcmpg_impl_int,
  dcmpl_impl_int,
  ddiv_impl_int,
  dmul_impl_int,
  dneg_impl_int,
  drem_impl_int,
  dreturn_impl_int,
  dsub_impl_int,
  dup_impl_int,
  dup_x1_impl_int,
  dup_x2_impl_int,
  dup2_impl_int,
  dup2_x1_impl_int,
  dup2_x2_impl_int,
  f2d_impl_int,
  f2i_impl_int,
  f2l_impl_int,
  fadd_impl_int,
  faload_impl_int,
  fastore_impl_int,
  fcmpg_impl_int,
  fcmpl_impl_int,
  fdiv_impl_int,
  fmul_impl_int,
  fneg_impl_int,
  frem_impl_int,
  freturn_impl_int,
  fsub_impl_int,
  i2b_impl_int,
  i2c_impl_int,
  i2d_impl_int,
  i2f_impl_int,
  i2l_impl_int,
  i2s_impl_int,
  iadd_impl_int,
  iaload_impl_int,
  iand_impl_int,
  iastore_impl_int,
  idiv_impl_int,
  imul_impl_int,
  ineg_impl_int,
  ior_impl_int,
  irem_impl_int,
  ireturn_impl_int,
  ishl_impl_int,
  ishr_impl_int,
  isub_impl_int,
  iushr_impl_int,
  ixor_impl_int,
  l2d_impl_int,
  l2f_impl_int,
  l2i_impl_int,
  ladd_impl_int,
  laload_impl_int,
  land_impl_int,
  lastore_impl_int,
  lcmp_impl_int,
  ldiv_impl_int,
  lmul_impl_int,
  lneg_impl_int,
  lor_impl_int,
  lrem_impl_int,
  lreturn_impl_int,
  lshl_impl_int,
  lshr_impl_int,
  lsub_impl_int,
  lushr_impl_int,
  lxor_impl_int,
  monitorenter_impl_int,
  monitorexit_impl_int,
  pop_impl_int,
  pop2_impl_int,
  return_impl_int,
  saload_impl_int,
  sastore_impl_int,
  swap_impl_int,
  anewarray_impl_int,
  checkcast_impl_int,
  getfield_impl_int,
  getstatic_impl_int,
  instanceof_impl_int,
  invokedynamic_impl_int,
  new_impl_int,
  putfield_impl_int,
  putstatic_impl_int,
  invokevirtual_impl_int,
  invokespecial_impl_int,
  invokestatic_impl_int,
  ldc_impl_int,
  ldc2_w_impl_int,
  dload_impl_int,
  fload_impl_int,
  iload_impl_int,
  lload_impl_int,
  dstore_impl_int,
  fstore_impl_int,
  istore_impl_int,
  lstore_impl_int,
  aload_impl_int,
  astore_impl_int,
  goto_impl_int,
  jsr_impl_int,
  if_acmpeq_impl_int,
  if_acmpne_impl_int,
  if_icmpeq_impl_int,
  if_icmpne_impl_int,
  if_icmplt_impl_int,
  if_icmpge_impl_int,
  if_icmpgt_impl_int,
  if_icmple_impl_int,
  ifeq_impl_int,
  ifne_impl_int,
  iflt_impl_int,
  ifge_impl_int,
  ifgt_impl_int,
  ifle_impl_int,
  ifnonnull_impl_int,
  ifnull_impl_int,
  iconst_impl_int,
  dconst_impl_int,
  fconst_impl_int,
  lconst_impl_int,
  iinc_impl_int,
  invokeinterface_impl_int,
  multianewarray_impl_int,
  newarray_impl_int,
  tableswitch_impl_int,
  lookupswitch_impl_int,
  ret_impl_int,
  anewarray_resolved_impl_int,
  checkcast_resolved_impl_int,
  instanceof_resolved_impl_int,
  new_resolved_impl_int,
  invokevtable_monomorphic_impl_int,
  invokevtable_polymorphic_impl_int,
  invokeitable_monomorphic_impl_int,
  invokeitable_polymorphic_impl_int,
  invokespecial_resolved_impl_int,
  invokestatic_resolved_impl_int,
  invokecallsite_impl_int,
  getfield_B_impl_int,
  getfield_C_impl_int,
  getfield_S_impl_int,
  getfield_I_impl_int,
  getfield_J_impl_int,
  getfield_F_impl_int,
  getfield_D_impl_int,
  getfield_Z_impl_int,
  getfield_L_impl_int,
  putfield_B_impl_int,
  putfield_C_impl_int,
  putfield_S_impl_int,
  putfield_I_impl_int,
  putfield_J_impl_int,
  putfield_F_impl_int,
  putfield_D_impl_int,
  putfield_Z_impl_int,
  putfield_L_impl_int,
  getstatic_B_impl_int,
  getstatic_C_impl_int,
  getstatic_S_impl_int,
  getstatic_I_impl_int,
  getstatic_J_impl_int,
  getstatic_F_impl_int,
  getstatic_D_impl_int,
  getstatic_Z_impl_int,
  getstatic_L_impl_int,
  putstatic_B_impl_int,
  putstatic_C_impl_int,
  putstatic_S_impl_int,
  putstatic_I_impl_int,
  putstatic_J_impl_int,
  putstatic_F_impl_int,
  putstatic_D_impl_int,
  putstatic_Z_impl_int,
  putstatic_L_impl_int
};


static bjvm_stack_value (*jmp_table_float[MAX_INSN_KIND])(ARGS_TOS(float)) = {
  nop_impl_float,
  aaload_impl_float,
  aastore_impl_float,
  aconst_null_impl_float,
  areturn_impl_float,
  arraylength_impl_float,
  athrow_impl_float,
  baload_impl_float,
  bastore_impl_float,
  caload_impl_float,
  castore_impl_float,
  d2f_impl_float,
  d2i_impl_float,
  d2l_impl_float,
  dadd_impl_float,
  daload_impl_float,
  dastore_impl_float,
  dcmpg_impl_float,
  dcmpl_impl_float,
  ddiv_impl_float,
  dmul_impl_float,
  dneg_impl_float,
  drem_impl_float,
  dreturn_impl_float,
  dsub_impl_float,
  dup_impl_float,
  dup_x1_impl_float,
  dup_x2_impl_float,
  dup2_impl_float,
  dup2_x1_impl_float,
  dup2_x2_impl_float,
  f2d_impl_float,
  f2i_impl_float,
  f2l_impl_float,
  fadd_impl_float,
  faload_impl_float,
  fastore_impl_float,
  fcmpg_impl_float,
  fcmpl_impl_float,
  fdiv_impl_float,
  fmul_impl_float,
  fneg_impl_float,
  frem_impl_float,
  freturn_impl_float,
  fsub_impl_float,
  i2b_impl_float,
  i2c_impl_float,
  i2d_impl_float,
  i2f_impl_float,
  i2l_impl_float,
  i2s_impl_float,
  iadd_impl_float,
  iaload_impl_float,
  iand_impl_float,
  iastore_impl_float,
  idiv_impl_float,
  imul_impl_float,
  ineg_impl_float,
  ior_impl_float,
  irem_impl_float,
  ireturn_impl_float,
  ishl_impl_float,
  ishr_impl_float,
  isub_impl_float,
  iushr_impl_float,
  ixor_impl_float,
  l2d_impl_float,
  l2f_impl_float,
  l2i_impl_float,
  ladd_impl_float,
  laload_impl_float,
  land_impl_float,
  lastore_impl_float,
  lcmp_impl_float,
  ldiv_impl_float,
  lmul_impl_float,
  lneg_impl_float,
  lor_impl_float,
  lrem_impl_float,
  lreturn_impl_float,
  lshl_impl_float,
  lshr_impl_float,
  lsub_impl_float,
  lushr_impl_float,
  lxor_impl_float,
  monitorenter_impl_float,
  monitorexit_impl_float,
  pop_impl_float,
  pop2_impl_float,
  return_impl_float,
  saload_impl_float,
  sastore_impl_float,
  swap_impl_float,
  anewarray_impl_float,
  checkcast_impl_float,
  getfield_impl_float,
  getstatic_impl_float,
  instanceof_impl_float,
  invokedynamic_impl_float,
  new_impl_float,
  putfield_impl_float,
  putstatic_impl_float,
  invokevirtual_impl_float,
  invokespecial_impl_float,
  invokestatic_impl_float,
  ldc_impl_float,
  ldc2_w_impl_float,
  dload_impl_float,
  fload_impl_float,
  iload_impl_float,
  lload_impl_float,
  dstore_impl_float,
  fstore_impl_float,
  istore_impl_float,
  lstore_impl_float,
  aload_impl_float,
  astore_impl_float,
  goto_impl_float,
  jsr_impl_float,
  if_acmpeq_impl_float,
  if_acmpne_impl_float,
  if_icmpeq_impl_float,
  if_icmpne_impl_float,
  if_icmplt_impl_float,
  if_icmpge_impl_float,
  if_icmpgt_impl_float,
  if_icmple_impl_float,
  ifeq_impl_float,
  ifne_impl_float,
  iflt_impl_float,
  ifge_impl_float,
  ifgt_impl_float,
  ifle_impl_float,
  ifnonnull_impl_float,
  ifnull_impl_float,
  iconst_impl_float,
  dconst_impl_float,
  fconst_impl_float,
  lconst_impl_float,
  iinc_impl_float,
  invokeinterface_impl_float,
  multianewarray_impl_float,
  newarray_impl_float,
  tableswitch_impl_float,
  lookupswitch_impl_float,
  ret_impl_float,
  anewarray_resolved_impl_float,
  checkcast_resolved_impl_float,
  instanceof_resolved_impl_float,
  new_resolved_impl_float,
  invokevtable_monomorphic_impl_float,
  invokevtable_polymorphic_impl_float,
  invokeitable_monomorphic_impl_float,
  invokeitable_polymorphic_impl_float,
  invokespecial_resolved_impl_float,
  invokestatic_resolved_impl_float,
  invokecallsite_impl_float,
  getfield_B_impl_float,
  getfield_C_impl_float,
  getfield_S_impl_float,
  getfield_I_impl_float,
  getfield_J_impl_float,
  getfield_F_impl_float,
  getfield_D_impl_float,
  getfield_Z_impl_float,
  getfield_L_impl_float,
  putfield_B_impl_float,
  putfield_C_impl_float,
  putfield_S_impl_float,
  putfield_I_impl_float,
  putfield_J_impl_float,
  putfield_F_impl_float,
  putfield_D_impl_float,
  putfield_Z_impl_float,
  putfield_L_impl_float,
  getstatic_B_impl_float,
  getstatic_C_impl_float,
  getstatic_S_impl_float,
  getstatic_I_impl_float,
  getstatic_J_impl_float,
  getstatic_F_impl_float,
  getstatic_D_impl_float,
  getstatic_Z_impl_float,
  getstatic_L_impl_float,
  putstatic_B_impl_float,
  putstatic_C_impl_float,
  putstatic_S_impl_float,
  putstatic_I_impl_float,
  putstatic_J_impl_float,
  putstatic_F_impl_float,
  putstatic_D_impl_float,
  putstatic_Z_impl_float,
  putstatic_L_impl_float
};