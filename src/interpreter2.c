#include "arrays.h"
#include "bjvm.h"
#include "classfile.h"

#include <math.h>
#include <tgmath.h>

// Interpreter 2 naming conventions:
//
// There are implementations for TOS types of int/long/reference, float, and double. Some instruction/TOS
// combinations are impossible (rejected by the verifier) and are thus omitted. The execution of "stack polymorphic"
// instructions like pop will consult the TOS stack type that the instruction is annotated with (during analysis),
// before loading the value from the stack and using the appropriate jump table.
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
  int8_t: jmp_table_int, \
  int16_t: jmp_table_int, \
  uint16_t: jmp_table_int, \
  int: jmp_table_int, \
  int64_t: jmp_table_int, \
  float: jmp_table_float, \
  double: jmp_table_double \
)

#define JMP(tos) MUSTTAIL return SELECT_TABLE(tos)[insns[pc].kind](thread, frame, insns, pc, sd, tos);
#define NEXT(tos) MUSTTAIL return SELECT_TABLE(tos)[insns[pc].kind](thread, frame, insns, pc + 1, sd, tos);
#define JMP_VOID MUSTTAIL return jmp_table_void[insns[pc].kind](thread, frame, insns, pc, sd);
#define NEXT_VOID MUSTTAIL return jmp_table_void[insns[pc].kind](thread, frame, insns, pc + 1, sd);

// Spill all the information currently in locals/registers to the frame (required at safepoints and when interrupting)
#define SPILL(tos) \
  frame->program_counter = pc; \
  frame->values[sd - 1] = _Generic((tos), \
    int64_t: (bjvm_stack_value) { .l = (int64_t)tos }, \
    float: (bjvm_stack_value) { .f = (float)tos }, \
    double: (bjvm_stack_value) { .d = (double)tos }, \
    bjvm_obj_header *: (bjvm_stack_value) { .obj = (bjvm_obj_header *)(uintptr_t)tos } /* shut up float branch */\
  );

#define SPILL_VOID \
  frame->program_counter = pc;

// Reload the top of stack type -- used after an instruction which may have instigated a GC
#define RELOAD(tos) \
  tos = _Generic(tos, \
    int64_t: frame->values[sd - 1].l, \
    float: frame->values[sd - 1].f, \
    double: frame->values[sd - 1].d, \
    bjvm_obj_header *: frame->values[sd - 1].obj \
  );

// Call the next instruction, but where we don't know a priori the top-of-stack type for that instruction, and must
// look it up from the analyzed tos type.
#define STACK_POLYMORPHIC_NEXT(tos) \
  switch (insn->tos_after) { \
    case TOS_VOID: NEXT_VOID \
    case TOS_INT: NEXT(tos.l) \
    case TOS_FLOAT: NEXT(tos.f) \
    case TOS_DOUBLE: NEXT(tos.d) \
    default: __builtin_unreachable(); \
}

// Call the instruction at pc, but where we don't know a priori the top-of-stack type for that instruction, and must
// look it up from the analyzed tos type.
#define STACK_POLYMORPHIC_JMP(tos) \
switch (insn->tos_after) { \
  case TOS_VOID: JMP_VOID \
  case TOS_INT: JMP(tos.l) \
  case TOS_FLOAT: JMP(tos.f) \
  case TOS_DOUBLE: JMP(tos.d) \
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

// For a given field kind, return the correct instruction kind for a resolved getstatic or putstatic instruction.
bjvm_insn_code_kind getstatic_putstatic_resolved_kind(bool putstatic, bjvm_type_kind field_kind) {
  switch (field_kind) {
  case BJVM_TYPE_KIND_BOOLEAN:
    return putstatic ? bjvm_insn_putstatic_B : bjvm_insn_getstatic_B;
  case BJVM_TYPE_KIND_CHAR:
    return putstatic ? bjvm_insn_putstatic_C : bjvm_insn_getstatic_C;
  case BJVM_TYPE_KIND_FLOAT:
    return putstatic ? bjvm_insn_putstatic_F : bjvm_insn_getstatic_F;
  case BJVM_TYPE_KIND_DOUBLE:
    return putstatic ? bjvm_insn_putstatic_D : bjvm_insn_getstatic_D;
  case BJVM_TYPE_KIND_BYTE:
    return putstatic ? bjvm_insn_putstatic_B : bjvm_insn_getstatic_B;
  case BJVM_TYPE_KIND_SHORT:
    return putstatic ? bjvm_insn_putstatic_S : bjvm_insn_getstatic_S;
  case BJVM_TYPE_KIND_INT:
    return putstatic ? bjvm_insn_putstatic_I : bjvm_insn_getstatic_I;
  case BJVM_TYPE_KIND_LONG:
    return putstatic ? bjvm_insn_putstatic_J : bjvm_insn_getstatic_J;
  case BJVM_TYPE_KIND_REFERENCE:
    return putstatic ? bjvm_insn_putstatic_L : bjvm_insn_getstatic_L;
  default:
    UNREACHABLE();
    break;
  }
}

// Convert getstatic and putstatic instructions into one of the resolved forms -- or throw a linkage error if
// appropriate. The stack should be made consistent before this function is called, as it may interrupt.
DECLARE_ASYNC(int, resolve_getstatic_putstatic,
  locals(bjvm_cp_field_info *field_info; bjvm_cp_class_info *class),
  arguments(bjvm_thread *thread; bjvm_bytecode_insn *inst;),
  invoked_methods(invoked_method(bjvm_initialize_class)));

DEFINE_ASYNC_SL(resolve_getstatic_putstatic, 100) {
  // For brevity
#define inst self->args.inst
#define thread self->args.thread

  bool putstatic = inst->kind == bjvm_insn_putstatic;
  self->field_info = &inst->cp->field;
  self->class = self->field_info->class_info;

  // First, attempt to resolve the class that the instruction is referring to.
  if (bjvm_resolve_class(thread, self->class))
    ASYNC_RETURN(-1);

  // Then, attempt to initialize the class.
  AWAIT(bjvm_initialize_class, thread, self->class->classdesc);
  if (thread->current_exception)
    ASYNC_RETURN(-1);

#define field self->field_info->field

  // Look up the field on the class.
  field = bjvm_field_lookup(self->class->classdesc,
    self->field_info->nat->name, self->field_info->nat->descriptor);

  // Check that the field exists on the class and is static. (TODO access checks)
  if (!field || !(field->access_flags & BJVM_ACCESS_STATIC)) {
    INIT_STACK_STRING(complaint, 1000);
    bprintf(complaint, "Expected static field %.*s on class %.*s", fmt_slice(self->field_info->nat->name),
            fmt_slice(self->field_info->class_info->name));
    bjvm_incompatible_class_change_error(thread, complaint);
    ASYNC_RETURN(-1);
  }

  // Select the appropriate resolved instruction kind.
  inst->kind = getstatic_putstatic_resolved_kind(putstatic, field_to_kind(self->field_info->parsed_descriptor));

  // Store the static address of the field in the instruction.
  inst->ic = (void *)field->my_class->static_fields + field->byte_offset;

#undef thread
#undef inst
#undef field

  ASYNC_END(0);
}

static bjvm_stack_value getstatic_impl_void(ARGS) {
  resolve_getstatic_putstatic_t ctx = { 0 };
  ctx.args.thread = thread;
  ctx.args.inst = insn;
  SPILL_VOID
  future_t fut = resolve_getstatic_putstatic(&ctx);
  if (thread->current_exception) {
    return value_null();
  }
  assert(fut.status == FUTURE_READY);  // for now
  JMP_VOID    // we rewrote this instruction to a resolved form, so jump to that implementation
}
FORWARD_TO_NULLARY(getstatic)

// Never actually directly called -- we just do it this way because it's easier and we might as well merge code paths
// for different TOS types.
static bjvm_stack_value putstatic_impl_void(ARGS) {
  resolve_getstatic_putstatic_t ctx = { 0 };
  ctx.args.thread = thread;
  ctx.args.inst = insn;
  SPILL_VOID
  future_t fut = resolve_getstatic_putstatic(&ctx);
  if (thread->current_exception) {
    return value_null();
  }
  assert(fut.status == FUTURE_READY);  // for now
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}
FORWARD_TO_NULLARY(putstatic)

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
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_C_impl_int(ARGS_TOS(int64_t)) {
  *(uint16_t *)insn->ic = (uint16_t)tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_S_impl_int(ARGS_TOS(int64_t)) {
  *(int16_t *)insn->ic = (int16_t)tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_I_impl_int(ARGS_TOS(int64_t)) {
  *(int *)insn->ic = (int)tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_J_impl_int(ARGS_TOS(int64_t)) {
  *(int64_t *)insn->ic = tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_F_impl_float(ARGS_TOS(float)) {
  *(float *)insn->ic = tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_D_impl_double(ARGS_TOS(double)) {
  *(double *)insn->ic = tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_L_impl_int(ARGS_TOS(int64_t)) {
  *(bjvm_obj_header **)insn->ic = (bjvm_obj_header *)tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putstatic_Z_impl_int(ARGS_TOS(int64_t)) {
  *(int8_t *)insn->ic = (int8_t)tos;
  --sd;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

/** getfield/putfield */

bjvm_insn_code_kind getfield_putfield_resolved_kind(bool putfield, bjvm_type_kind field_kind) {
  switch (field_kind) {
  case BJVM_TYPE_KIND_BOOLEAN:
    return putfield ? bjvm_insn_putfield_B : bjvm_insn_getfield_B;
  case BJVM_TYPE_KIND_CHAR:
    return putfield ? bjvm_insn_putfield_C : bjvm_insn_getfield_C;
  case BJVM_TYPE_KIND_FLOAT:
    return putfield ? bjvm_insn_putfield_F : bjvm_insn_getfield_F;
  case BJVM_TYPE_KIND_DOUBLE:
    return putfield ? bjvm_insn_putfield_D : bjvm_insn_getfield_D;
  case BJVM_TYPE_KIND_BYTE:
    return putfield ? bjvm_insn_putfield_B : bjvm_insn_getfield_B;
  case BJVM_TYPE_KIND_SHORT:
    return putfield ? bjvm_insn_putfield_S : bjvm_insn_getfield_S;
  case BJVM_TYPE_KIND_INT:
    return putfield ? bjvm_insn_putfield_I : bjvm_insn_getfield_I;
  case BJVM_TYPE_KIND_LONG:
    return putfield ? bjvm_insn_putfield_J : bjvm_insn_getfield_J;
  case BJVM_TYPE_KIND_REFERENCE:
    return putfield ? bjvm_insn_putfield_L : bjvm_insn_getfield_L;
  default:
    UNREACHABLE();
  }
}

// Convert getfield and putfield instructions into one of the resolved forms -- or throw a linkage error if
// appropriate. The stack should be made consistent before this function is called, as it may interrupt.
DECLARE_ASYNC(int, resolve_getfield_putfield,
  locals(bjvm_cp_field_info *field_info; bjvm_cp_class_info *class),
  arguments(bjvm_thread *thread; bjvm_bytecode_insn *inst; bjvm_plain_frame *frame; int sd;),
  invoked_methods(invoked_method(bjvm_initialize_class)));

DEFINE_ASYNC_SL(resolve_getfield_putfield, 100) {
  // For brevity
#define inst self->args.inst
#define thread self->args.thread
#define frame self->args.frame
#define sd self->args.sd

  bool putfield = inst->kind == bjvm_insn_putfield;

  bjvm_obj_header *obj = frame->values[sd - 1 - putfield].obj;
  if (!obj) {
    bjvm_null_pointer_exception(thread);
    ASYNC_RETURN(-1);
  }
  bjvm_cp_field_info *field_info = &inst->cp->field;
  if (bjvm_resolve_field(thread, field_info)) {
    ASYNC_RETURN(-1);
  }
  if (field_info->field->access_flags & BJVM_ACCESS_STATIC) {
    INIT_STACK_STRING(complaint, 1000);
    bprintf(complaint, "Expected nonstatic field %.*s on class %.*s", fmt_slice(field_info->nat->name),
            fmt_slice(field_info->class_info->name));
    bjvm_incompatible_class_change_error(thread, complaint);
    ASYNC_RETURN(-1);
  }

  inst->kind = getfield_putfield_resolved_kind(putfield, field_to_kind(field_info->parsed_descriptor));
  inst->ic = field_info->field;
  inst->ic2 = (void *)field_info->field->byte_offset;

  ASYNC_END(0);

#undef frame
#undef thread
#undef inst
#undef sd
}

static bjvm_stack_value getfield_impl_int(ARGS_TOS(int64_t)) {
  resolve_getfield_putfield_t ctx = { 0 };
  ctx.args.thread = thread;
  ctx.args.inst = insn;
  ctx.args.frame = frame;
  ctx.args.sd = sd;
  SPILL_VOID
  future_t fut = resolve_getfield_putfield(&ctx);
  if (thread->current_exception) {
    return value_null();
  }
  assert(fut.status == FUTURE_READY);  // for now
  JMP(tos)
}

static bjvm_stack_value putfield_impl_void(ARGS) {
  resolve_getfield_putfield_t ctx = { 0 };
  ctx.args.thread = thread;
  ctx.args.inst = insn;
  ctx.args.frame = frame;
  ctx.args.sd = sd;
  SPILL_VOID
  future_t fut = resolve_getfield_putfield(&ctx);
  if (thread->current_exception) {
    return value_null();
  }
  assert(fut.status == FUTURE_READY);  // for now
  STACK_POLYMORPHIC_JMP(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(putfield)

static bjvm_stack_value getfield_B_impl_int(ARGS_TOS(int64_t)) {
  int8_t *field = (int8_t *)((char *)tos + insn->index);
  NEXT((int64_t)*field)
}

static bjvm_stack_value getfield_C_impl_int(ARGS_TOS(int64_t)) {
  uint16_t *field = (uint16_t *)((char *)tos + insn->index);
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

static bjvm_stack_value putfield_C_impl_int(ARGS_TOS(int64_t)) {
  uint16_t *field = (uint16_t *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (uint16_t)tos;
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

static bjvm_stack_value putfield_L_impl_int(ARGS_TOS(int64_t)) {
  bjvm_obj_header **field = (bjvm_obj_header **)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (bjvm_obj_header *)tos;
  sd -= 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value putfield_Z_impl_int(ARGS_TOS(int64_t)) {
  int8_t *field = (int8_t *)((char *)frame->values[sd - 2].obj + insn->index);
  *field = (int8_t)tos;
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
    SPILL(tos);
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_idiv(a, b));
}

static bjvm_stack_value ldiv_impl_int(ARGS_TOS(int64_t)) {
  int64_t a = frame->values[sd - 2].l, b = tos; \
  if (unlikely(b == 0)) {
    SPILL(tos);
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_ldiv(a, b));
}

static bjvm_stack_value lcmp_impl_int(ARGS_TOS(int64_t)) {
  int64_t a = frame->values[sd - 2].l, b = tos;
  sd--;
  NEXT(a > b ? 1 : (a < b ? -1 : 0));
}

static bjvm_stack_value irem_impl_int(ARGS_TOS(int64_t)) {
  int a = frame->values[sd - 2].i, b = (int)tos; \
  if (unlikely(b == 0)) {
    SPILL(tos);
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_irem(a, b));
}

static bjvm_stack_value lrem_impl_int(ARGS_TOS(int64_t)) {
  int64_t a = frame->values[sd - 2].l, b = tos; \
  if (unlikely(b == 0)) {
    SPILL(tos);
    bjvm_arithmetic_exception(thread, STR("/ by zero"));
    return value_null();
  }
  NEXT(java_lrem(a, b));
}

/** Array instructions (arraylength, array loads, array stores) */

static bjvm_stack_value arraylength_impl_int(ARGS_TOS(int64_t)) {
  bjvm_obj_header *array = (bjvm_obj_header *)tos;
  if (unlikely(!array)) {
    SPILL(tos);
    bjvm_null_pointer_exception(thread);
    return value_null();
  }
  NEXT(*ArrayLength(array))
}

#define ARRAY_LOAD(which, load) \
static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
bjvm_obj_header *array = (bjvm_obj_header *)frame->values[sd - 2].obj; \
int index = (int)tos; \
if (unlikely(!array)) { \
  SPILL(tos); \
  bjvm_null_pointer_exception(thread); \
  return value_null(); \
} \
int length = *ArrayLength(array); \
if (unlikely(index < 0 || index >= length)) { \
  SPILL(tos); \
  bjvm_array_index_oob_exception(thread, index, length); \
  return value_null(); \
} \
sd--; \
NEXT(load(array, index)) \
}

ARRAY_LOAD(iaload, IntArrayLoad)
ARRAY_LOAD(laload, LongArrayLoad)
ARRAY_LOAD(faload, FloatArrayLoad)
ARRAY_LOAD(daload, DoubleArrayLoad)
ARRAY_LOAD(aaload, ReferenceArrayLoad)
ARRAY_LOAD(baload, ByteArrayLoad)
ARRAY_LOAD(saload, ShortArrayLoad)
ARRAY_LOAD(caload, CharArrayLoad)

#define ARRAY_STORE(which, tt1, tt2, tt3, store) \
  static bjvm_stack_value which##_impl_##tt1(ARGS_TOS(tt2)) { \
    bjvm_obj_header *array = (bjvm_obj_header *)frame->values[sd - 3].obj; \
    int index = (int)frame->values[sd - 2].i; \
    if (unlikely(!array)) { \
      SPILL(tos); \
      bjvm_null_pointer_exception(thread); \
      return value_null(); \
    } \
    int length = *ArrayLength(array); \
    if (unlikely(index < 0 || index >= length)) { \
      SPILL(tos); \
      bjvm_array_index_oob_exception(thread, index, length); \
      return value_null(); \
    } \
    store(array, index, (tt3)tos); \
    sd -= 3; \
    STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]); \
  }

ARRAY_STORE(iastore, int, int64_t, int, IntArrayStore)
ARRAY_STORE(lastore, int, int64_t, int64_t, LongArrayStore)
ARRAY_STORE(fastore, float, float, float, FloatArrayStore)
ARRAY_STORE(dastore, double, double, double, DoubleArrayStore)
ARRAY_STORE(bastore, int, int64_t, int8_t, ByteArrayStore)
ARRAY_STORE(sastore, int, int64_t, int16_t, ShortArrayStore)
ARRAY_STORE(castore, int, int64_t, uint16_t, CharArrayStore)

// We implement aastore separately because it needs an additional instanceof check for ArrayStoreExceptions.
// <array> <index> <value>  ->  <void>
static bjvm_stack_value aastore_impl_int(ARGS_TOS(int64_t)) {
  bjvm_obj_header *array = (bjvm_obj_header *)frame->values[sd - 3].obj;
  bjvm_obj_header *value = (bjvm_obj_header *)tos;
  int index = (int)frame->values[sd - 2].i;
  if (unlikely(!array)) {
    SPILL(tos);
    bjvm_null_pointer_exception(thread);
    return value_null();
  }
  int length = *ArrayLength(array);
  if (unlikely(index < 0 || index >= length)) {
    SPILL(tos);
    bjvm_array_index_oob_exception(thread, index, length);
    return value_null();
  }
  // Instanceof check against the component type
  if (value && !bjvm_instanceof(value->descriptor, array->descriptor->one_fewer_dim)) {
    SPILL(tos);
    bjvm_array_store_exception(thread, hslc(value->descriptor->name));
    return value_null();
  }
  ReferenceArrayStore(array, index, (bjvm_obj_header *)tos);
  sd -= 3;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

/** Control-flow instructions (returns, jumps, branches) */

static bjvm_stack_value return_impl_void(ARGS) {
  SPILL_VOID
  return value_null();
}
FORWARD_TO_NULLARY(return)

static bjvm_stack_value areturn_impl_int(ARGS_TOS(int64_t)) {
  SPILL(tos);
  return (bjvm_stack_value){.l = tos};
}

static bjvm_stack_value ireturn_impl_int(ARGS_TOS(int64_t)) {
  SPILL(tos);
  return (bjvm_stack_value){.l = tos};
}

static bjvm_stack_value lreturn_impl_int(ARGS_TOS(int64_t)) {
  SPILL(tos);
  return (bjvm_stack_value){.l = tos};
}

static bjvm_stack_value freturn_impl_float(ARGS_TOS(float)) {
  SPILL(tos);
  return (bjvm_stack_value){.f = tos};
}

static bjvm_stack_value dreturn_impl_double(ARGS_TOS(double)) {
  SPILL(tos);
  return (bjvm_stack_value){.d = tos};
}

static bjvm_stack_value goto_impl_void(ARGS) {
  pc = insn->index;
  JMP_VOID
}

static bjvm_stack_value goto_impl_double(ARGS_TOS(double)) {
  pc = insn->index;
  JMP(tos)
}

static bjvm_stack_value goto_impl_float(ARGS_TOS(float)) {
  pc = insn->index;
  JMP(tos)
}

static bjvm_stack_value goto_impl_int(ARGS_TOS(int64_t)) {
  pc = insn->index;
  JMP(tos)
}

static bjvm_stack_value tableswitch_impl_int(ARGS_TOS(int64_t)) {
  int32_t index = (int32_t)tos;
  int32_t low = insn->tableswitch.low;
  int32_t high = insn->tableswitch.high;
  int32_t *offsets = insn->tableswitch.targets;
  if (index < low || index > high) {
    pc = insn->tableswitch.default_target;
  } else {
    pc = offsets[index - low];
  }
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value lookupswitch_impl_int(ARGS_TOS(int64_t)) {
  struct bjvm_bc_lookupswitch_data data = insn->lookupswitch;

  int32_t key = (int32_t)tos;
  int32_t *keys = insn->lookupswitch.keys;
  int32_t *offsets = insn->lookupswitch.targets;
  int32_t n = insn->lookupswitch.keys_count;
  int32_t default_target = insn->lookupswitch.default_target;
  // TODO replace this with a binary search
  for (int i = 0; i < n; i++) {
    if (keys[i] == key) {
      pc = offsets[i];
      sd--;
      STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
    }
  }
  pc = default_target;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

#define MAKE_INT_BRANCH_AGAINST_0(which, op) \
  static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
    pc = tos op 0 ? insn->index : pc + 1; \
    sd--; \
    STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]); \
  }

MAKE_INT_BRANCH_AGAINST_0(ifeq, ==)
MAKE_INT_BRANCH_AGAINST_0(ifne, !=)
MAKE_INT_BRANCH_AGAINST_0(iflt, <)
MAKE_INT_BRANCH_AGAINST_0(ifge, >=)
MAKE_INT_BRANCH_AGAINST_0(ifgt, >)
MAKE_INT_BRANCH_AGAINST_0(ifle, <=)
MAKE_INT_BRANCH_AGAINST_0(ifnull, ==)
MAKE_INT_BRANCH_AGAINST_0(ifnonnull, !=)

#define MAKE_INT_BRANCH(which, op) \
  static bjvm_stack_value which##_impl_int(ARGS_TOS(int64_t)) { \
    int64_t a = frame->values[sd - 2].i, b = tos; \
    pc = a op b ? insn->index : pc + 1; \
    sd -= 2; \
    STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]); \
  }

MAKE_INT_BRANCH(if_icmpeq, ==)
MAKE_INT_BRANCH(if_icmpne, !=)
MAKE_INT_BRANCH(if_icmplt, <)
MAKE_INT_BRANCH(if_icmpge, >=)
MAKE_INT_BRANCH(if_icmpgt, >)
MAKE_INT_BRANCH(if_icmple, <=)
MAKE_INT_BRANCH(if_acmpeq, ==)
MAKE_INT_BRANCH(if_acmpne, !=)

/** Monitors */

// TODO actually implement this stuff
static bjvm_stack_value monitorenter_impl_int(ARGS_TOS(int64_t)) {
  if (unlikely(!tos)) {
    SPILL(tos);
    bjvm_null_pointer_exception(thread);
    return value_null();
  }

  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value monitorexit_impl_int(ARGS_TOS(int64_t)) {
  if (unlikely(!tos)) {
    SPILL(tos);
    bjvm_null_pointer_exception(thread);
    return value_null();
  }

  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

/** New object creation */

static bjvm_stack_value new_impl_void(ARGS) {
  SPILL_VOID
  sd++;

  bjvm_cp_class_info *info = &insn->cp->class_info;
  int error = bjvm_resolve_class(thread, info);
  if (error)
    return value_null();

  if (insn->cp->class_info.classdesc->state < BJVM_CD_STATE_INITIALIZED) {
    bjvm_initialize_class_t init = { 0 };
    init.args.thread = thread;
    init.args.classdesc = insn->cp->class_info.classdesc;
    future_t fut = bjvm_initialize_class(&init);
    assert(fut.status == FUTURE_READY);  // for now
    if (thread->current_exception)
      return value_null();
  }

  insn->kind = bjvm_insn_new_resolved;
  insn->classdesc = info->classdesc;

  JMP_VOID
}
FORWARD_TO_NULLARY(new)

static bjvm_stack_value new_resolved_impl_void(ARGS) {
  bjvm_obj_header *obj = new_object(thread, insn->classdesc);
  if (!obj)
    return value_null();
  NEXT(obj)
}
FORWARD_TO_NULLARY(new_resolved)

static bjvm_stack_value newarray_impl_int(ARGS_TOS(int64_t)) {
  int count = tos;
  if (count < 0) {
    bjvm_negative_array_size_exception(thread, count);
    return value_null();
  }
  bjvm_obj_header *array = CreatePrimitiveArray1D(thread, insn->array_type, count);
  if (array) {
    NEXT(array)
  }
  return value_null();  // oom
}

static bjvm_stack_value anewarray_impl_int(ARGS_TOS(int64_t)) {
  bjvm_cp_class_info *info = &insn->cp->class_info;
  if (bjvm_resolve_class(thread, info))
    return value_null();
  assert(info->classdesc);
  if (bjvm_link_class(thread, info->classdesc))
    return value_null();
  insn->classdesc = info->classdesc;
  insn->kind = bjvm_insn_anewarray_resolved;
  JMP(tos)
}

// <length> -> <object>
static bjvm_stack_value anewarray_resolved_impl_int(ARGS_TOS(int64_t)) {
  int count = tos;
  if (count < 0) {
    bjvm_negative_array_size_exception(thread, count);
    return value_null();
  }
  bjvm_obj_header *array = CreateObjectArray1D(thread, insn->classdesc, count);
  if (array) {
    NEXT(array)
  }
  return value_null();  // oom
}

static bjvm_stack_value multianewarray_impl_int(ARGS_TOS(int64_t)) {
  SPILL(tos)
  uint16_t temp_sd = sd;
  if (bjvm_multianewarray(thread, frame, &insn->multianewarray, &temp_sd))
    return value_null();
  NEXT(frame->values[temp_sd - 1].obj)
}

/** Method invocations */

/** Local variable accessors */
static bjvm_stack_value iload_impl_void(ARGS) {
  sd++;
  NEXT(frame->values[frame->max_stack + insn->index].i)
}
FORWARD_TO_NULLARY(iload)

static bjvm_stack_value fload_impl_void(ARGS) {
  sd++;
  NEXT(frame->values[frame->max_stack + insn->index].f)
}
FORWARD_TO_NULLARY(fload)

static bjvm_stack_value dload_impl_void(ARGS) {
  sd++;
  NEXT(frame->values[frame->max_stack + insn->index].d)
}
FORWARD_TO_NULLARY(dload)

static bjvm_stack_value lload_impl_void(ARGS) {
  sd++;
  NEXT(frame->values[frame->max_stack + insn->index].l)
}
FORWARD_TO_NULLARY(lload)

static bjvm_stack_value aload_impl_void(ARGS) {
  sd++;
  NEXT(frame->values[frame->max_stack + insn->index].obj)
}
FORWARD_TO_NULLARY(aload)

static bjvm_stack_value astore_impl_int(ARGS_TOS(int64_t)) {
  frame->values[frame->max_stack + insn->index].obj = (bjvm_obj_header *)tos;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value istore_impl_int(ARGS_TOS(int64_t)) {
  frame->values[frame->max_stack + insn->index].i = (int)tos;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value fstore_impl_float(ARGS_TOS(float)) {
  frame->values[frame->max_stack + insn->index].f = tos;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value dstore_impl_double(ARGS_TOS(double)) {
  frame->values[frame->max_stack + insn->index].d = tos;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value lstore_impl_int(ARGS_TOS(int64_t)) {
  frame->values[frame->max_stack + insn->index].l = tos;
  sd--;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}

static bjvm_stack_value iinc_impl_void(ARGS) {
  frame->values[frame->max_stack + insn->iinc.index].i += insn->iinc.const_;
  NEXT_VOID
}
FORWARD_TO_NULLARY(iinc)

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
FORWARD_TO_NULLARY(ldc2_w)

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

// Never directly called
static bjvm_stack_value swap_impl_void(ARGS) {
  bjvm_stack_value tmp = frame->values[sd - 1];
  frame->values[sd - 1] = frame->values[sd - 2];
  frame->values[sd - 2] = tmp;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1]);
}
FORWARD_TO_NULLARY(swap)

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

// Rare enough (especially after analysis phase) that we might as well just have the one implementation and forward
// every TOS type to them.

// ..., val2, val1 -> ..., val2, val1, val2, val1
static bjvm_stack_value dup2_impl_void(ARGS) {
  bjvm_stack_value val1 = frame->values[sd - 1], val2 = frame->values[sd - 2];
  frame->values[sd] = val2;
  frame->values[sd + 1] = val1;
  sd += 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(dup2)

// ..., val2, val1 -> ..., val1, val2, val1
static bjvm_stack_value dup_x1_impl_void(ARGS) {
  bjvm_stack_value val1 = frame->values[sd - 1], val2 = frame->values[sd - 2];
  frame->values[sd - 2] = val1;
  frame->values[sd - 1] = val2;
  frame->values[sd] = val1;
  sd++;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(dup_x1)

// ..., val3, val2, val1 -> val1, val3, val2, val1
static bjvm_stack_value dup_x2_impl_void(ARGS) {
  bjvm_stack_value val1 = frame->values[sd - 1], val2 = frame->values[sd - 2], val3 = frame->values[sd - 3];
  frame->values[sd - 3] = val1;
  frame->values[sd - 2] = val3;
  frame->values[sd - 1] = val2;
  frame->values[sd] = val1;
  sd++;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(dup_x2)

// ..., val3, val2, val1 -> ..., val2, val1, val3, val2, val1
static bjvm_stack_value dup2_x1_impl_void(ARGS) {
  bjvm_stack_value val1 = frame->values[sd - 1], val2 = frame->values[sd - 2], val3 = frame->values[sd - 3];
  frame->values[sd - 3] = val2;
  frame->values[sd - 2] = val1;
  frame->values[sd - 1] = val3;
  frame->values[sd] = val2;
  frame->values[sd + 1] = val1;
  sd += 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(dup2_x1)

// ..., val4, val3, val2, val1 -> ..., val2, val1, val4, val3, val2, val1
static bjvm_stack_value dup2_x2_impl_void(ARGS) {
  bjvm_stack_value val1 = frame->values[sd - 1], val2 = frame->values[sd - 2], val3 = frame->values[sd - 3], val4 = frame->values[sd - 4];
  frame->values[sd - 4] = val2;
  frame->values[sd - 3] = val1;
  frame->values[sd - 2] = val4;
  frame->values[sd - 1] = val3;
  frame->values[sd] = val2;
  frame->values[sd + 1] = val1;
  sd += 2;
  STACK_POLYMORPHIC_NEXT(frame->values[sd - 1])
}
FORWARD_TO_NULLARY(dup2_x2)

bjvm_stack_value bjvm_interpret_2(bjvm_thread *thread, bjvm_stack_frame *frame) {

}

/** Jump table definitions. Must be kept in sync with the enum order. */

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
  nullptr /* if_acmpeq_impl_void */,
  nullptr /* if_acmpne_impl_void */,
  nullptr /* if_icmpeq_impl_void */,
  nullptr /* if_icmpne_impl_void */,
  nullptr /* if_icmplt_impl_void */,
  nullptr /* if_icmpge_impl_void */,
  nullptr /* if_icmpgt_impl_void */,
  nullptr /* if_icmple_impl_void */,
  nullptr /* ifeq_impl_void */,
  nullptr /* ifne_impl_void */,
  nullptr /* iflt_impl_void */,
  nullptr /* ifge_impl_void */,
  nullptr /* ifgt_impl_void */,
  nullptr /* ifle_impl_void */,
  nullptr /* ifnonnull_impl_void */,
  nullptr /* ifnull_impl_void */,
  iconst_impl_void,
  dconst_impl_void,
  fconst_impl_void,
  lconst_impl_void,
  iinc_impl_void,
  nullptr /* invokeinterface_impl_void */,
  nullptr /* multianewarray_impl_void */,
  nullptr /* newarray_impl_void */,
  nullptr /* tableswitch_impl_void */,
  nullptr /* lookupswitch_impl_void */,
  nullptr /* ret_impl_void */,
  nullptr /* anewarray_resolved_impl_void */,
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
  nullptr /* anewarray_impl_double */,
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
  nullptr /* jsr_impl_double */,
  nullptr /* if_acmpeq_impl_double */,
  nullptr /* if_acmpne_impl_double */,
  nullptr /* if_icmpeq_impl_double */,
  nullptr /* if_icmpne_impl_double */,
  nullptr /* if_icmplt_impl_double */,
  nullptr /* if_icmpge_impl_double */,
  nullptr /* if_icmpgt_impl_double */,
  nullptr /* if_icmple_impl_double */,
  nullptr /* ifeq_impl_double */,
  nullptr /* ifne_impl_double */,
  nullptr /* iflt_impl_double */,
  nullptr /* ifge_impl_double */,
  nullptr /* ifgt_impl_double */,
  nullptr /* ifle_impl_double */,
  nullptr /* ifnonnull_impl_double */,
  nullptr /* ifnull_impl_double */,
  iconst_impl_double,
  dconst_impl_double,
  fconst_impl_double,
  lconst_impl_double,
  iinc_impl_double,
  invokeinterface_impl_double,
  nullptr /* multianewarray_impl_double */,
  nullptr /* newarray_impl_double */,
  nullptr /* tableswitch_impl_double */,
  nullptr /* lookupswitch_impl_double */,
  nullptr /* ret_impl_double */,
  nullptr /* anewarray_resolved_impl_double */,
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
  nullptr /* d2f_impl_int */,
  nullptr /* d2i_impl_int */,
  nullptr /* d2l_impl_int */,
  nullptr /* dadd_impl_int */,
  daload_impl_int,
  nullptr /* dastore_impl_int */,
  nullptr /* dcmpg_impl_int */,
  nullptr /* dcmpl_impl_int */,
  nullptr /* ddiv_impl_int */,
  nullptr /* dmul_impl_int */,
  nullptr /* dneg_impl_int */,
  nullptr /* drem_impl_int */,
  nullptr /* dreturn_impl_int */,
  nullptr /* dsub_impl_int */,
  dup_impl_int,
  dup_x1_impl_int,
  dup_x2_impl_int,
  dup2_impl_int,
  dup2_x1_impl_int,
  dup2_x2_impl_int,
  nullptr /* f2d_impl_int */,
  nullptr /* f2i_impl_int */,
  nullptr /* f2l_impl_int */,
  nullptr /* fadd_impl_int */,
  faload_impl_int,
  nullptr /* fastore_impl_int */,
  nullptr /* fcmpg_impl_int */,
  nullptr /* fcmpl_impl_int */,
  nullptr /* fdiv_impl_int */,
  nullptr /* fmul_impl_int */,
  nullptr /* fneg_impl_int */,
  nullptr /* frem_impl_int */,
  nullptr /* freturn_impl_int */,
  nullptr /* fsub_impl_int */,
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
  nullptr /* dstore_impl_int */,
  nullptr /* fstore_impl_int */,
  istore_impl_int,
  lstore_impl_int,
  aload_impl_int,
  astore_impl_int,
  goto_impl_int,
  nullptr /* jsr_impl_int */,
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
  nullptr /* ret_impl_int */,
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
  nullptr /* putfield_F_impl_int */,
  nullptr /* putfield_D_impl_int */,
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
  nullptr /* putstatic_F_impl_int */,
  nullptr /* putstatic_D_impl_int */,
  putstatic_Z_impl_int,
  putstatic_L_impl_int
};


static bjvm_stack_value (*jmp_table_float[MAX_INSN_KIND])(ARGS_TOS(float)) = {
  nop_impl_float,
  nullptr /* aaload_impl_float */,
  nullptr /* aastore_impl_float */,
  aconst_null_impl_float,
  nullptr /* areturn_impl_float */,
  nullptr /* arraylength_impl_float */,
  nullptr /* athrow_impl_float */,
  nullptr /* baload_impl_float */,
  nullptr /* bastore_impl_float */,
  nullptr /* caload_impl_float */,
  nullptr /* castore_impl_float */,
  nullptr /* d2f_impl_float */,
  nullptr /* d2i_impl_float */,
  nullptr /* d2l_impl_float */,
  nullptr /* dadd_impl_float */,
  nullptr /* daload_impl_float */,
  nullptr /* dastore_impl_float */,
  nullptr /* dcmpg_impl_float */,
  nullptr /* dcmpl_impl_float */,
  nullptr /* ddiv_impl_float */,
  nullptr /* dmul_impl_float */,
  nullptr /* dneg_impl_float */,
  nullptr /* drem_impl_float */,
  nullptr /* dreturn_impl_float */,
  nullptr /* dsub_impl_float */,
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
  nullptr /* faload_impl_float */,
  fastore_impl_float,
  fcmpg_impl_float,
  fcmpl_impl_float,
  fdiv_impl_float,
  fmul_impl_float,
  fneg_impl_float,
  frem_impl_float,
  freturn_impl_float,
  fsub_impl_float,
  nullptr /* i2b_impl_float */,
  nullptr /* i2c_impl_float */,
  nullptr /* i2d_impl_float */,
  nullptr /* i2f_impl_float */,
  nullptr /* i2l_impl_float */,
  nullptr /* i2s_impl_float */,
  nullptr /* iadd_impl_float */,
  nullptr /* iaload_impl_float */,
  nullptr /* iand_impl_float */,
  nullptr /* iastore_impl_float */,
  nullptr /* idiv_impl_float */,
  nullptr /* imul_impl_float */,
  nullptr /* ineg_impl_float */,
  nullptr /* ior_impl_float */,
  nullptr /* irem_impl_float */,
  nullptr /* ireturn_impl_float */,
  nullptr /* ishl_impl_float */,
  nullptr /* ishr_impl_float */,
  nullptr /* isub_impl_float */,
  nullptr /* iushr_impl_float */,
  nullptr /* ixor_impl_float */,
  nullptr /* l2d_impl_float */,
  nullptr /* l2f_impl_float */,
  nullptr /* l2i_impl_float */,
  nullptr /* ladd_impl_float */,
  nullptr /* laload_impl_float */,
  nullptr /* land_impl_float */,
  nullptr /* lastore_impl_float */,
  nullptr /* lcmp_impl_float */,
  nullptr /* ldiv_impl_float */,
  nullptr /* lmul_impl_float */,
  nullptr /* lneg_impl_float */,
  nullptr /* lor_impl_float */,
  nullptr /* lrem_impl_float */,
  nullptr /* lreturn_impl_float */,
  nullptr /* lshl_impl_float */,
  nullptr /* lshr_impl_float */,
  nullptr /* lsub_impl_float */,
  nullptr /* lushr_impl_float */,
  nullptr /* lxor_impl_float */,
  nullptr /* monitorenter_impl_float */,
  nullptr /* monitorexit_impl_float */,
  pop_impl_float,
  pop2_impl_float,
  return_impl_float,
  nullptr /* saload_impl_float */,
  nullptr /* sastore_impl_float */,
  swap_impl_float,
  nullptr /* anewarray_impl_float */,
  nullptr /* checkcast_impl_float */,
  nullptr /* getfield_impl_float */,
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
  nullptr /* dstore_impl_float */,
  fstore_impl_float,
  nullptr /* istore_impl_float */,
  nullptr /* lstore_impl_float */,
  aload_impl_float,
  nullptr /* astore_impl_float */,
  goto_impl_float,
  nullptr /* jsr_impl_float */,
  nullptr /* if_acmpeq_impl_float */,
  nullptr /* if_acmpne_impl_float */,
  nullptr /* if_icmpeq_impl_float */,
  nullptr /* if_icmpne_impl_float */,
  nullptr /* if_icmplt_impl_float */,
  nullptr /* if_icmpge_impl_float */,
  nullptr /* if_icmpgt_impl_float */,
  nullptr /* if_icmple_impl_float */,
  nullptr /* ifeq_impl_float */,
  nullptr /* ifne_impl_float */,
  nullptr /* iflt_impl_float */,
  nullptr /* ifge_impl_float */,
  nullptr /* ifgt_impl_float */,
  nullptr /* ifle_impl_float */,
  nullptr /* ifnonnull_impl_float */,
  nullptr /* ifnull_impl_float */,
  iconst_impl_float,
  dconst_impl_float,
  fconst_impl_float,
  lconst_impl_float,
  iinc_impl_float,
  invokeinterface_impl_float,
  nullptr /* multianewarray_impl_float */,
  nullptr /* newarray_impl_float */,
  nullptr /* tableswitch_impl_float */,
  nullptr /* lookupswitch_impl_float */,
  nullptr /* ret_impl_float */,
  nullptr /* anewarray_resolved_impl_float */,
  nullptr /* checkcast_resolved_impl_float */,
  nullptr /* instanceof_resolved_impl_float */,
  new_resolved_impl_float,
  invokevtable_monomorphic_impl_float,
  invokevtable_polymorphic_impl_float,
  invokeitable_monomorphic_impl_float,
  invokeitable_polymorphic_impl_float,
  invokespecial_resolved_impl_float,
  invokestatic_resolved_impl_float,
  invokecallsite_impl_float,
  nullptr /* getfield_B_impl_float */,
  nullptr /* getfield_C_impl_float */,
  nullptr /* getfield_S_impl_float */,
  nullptr /* getfield_I_impl_float */,
  nullptr /* getfield_J_impl_float */,
  nullptr /* getfield_F_impl_float */,
  nullptr /* getfield_D_impl_float */,
  nullptr /* getfield_Z_impl_float */,
  nullptr /* getfield_L_impl_float */,
  nullptr /* putfield_B_impl_float */,
  nullptr /* putfield_C_impl_float */,
  nullptr /* putfield_S_impl_float */,
  nullptr /* putfield_I_impl_float */,
  nullptr /* putfield_J_impl_float */,
  putfield_F_impl_float,
  nullptr /* putfield_D_impl_float */,
  nullptr /* putfield_Z_impl_float */,
  nullptr /* putfield_L_impl_float */,
  getstatic_B_impl_float,
  getstatic_C_impl_float,
  getstatic_S_impl_float,
  getstatic_I_impl_float,
  getstatic_J_impl_float,
  getstatic_F_impl_float,
  getstatic_D_impl_float,
  getstatic_Z_impl_float,
  getstatic_L_impl_float,
  nullptr /* putstatic_B_impl_float */,
  nullptr /* putstatic_C_impl_float */,
  nullptr /* putstatic_S_impl_float */,
  nullptr /* putstatic_I_impl_float */,
  nullptr /* putstatic_J_impl_float */,
  putstatic_F_impl_float,
  nullptr /* putstatic_D_impl_float */,
  nullptr /* putstatic_Z_impl_float */,
  nullptr /* putstatic_L_impl_float */
};