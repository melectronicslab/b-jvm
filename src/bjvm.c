#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <threads.h>
#include <byteswap.h>
#include <stdbool.h>

#define BJVM_UNREACHABLE() do { fprintf(stderr, "Unreachable code reached at %s:%d\n", __FILE__, __LINE__); abort(); } while (0)
#define BJVM_DCHECK(expr) do { if (!(expr)) { fprintf(stderr, "Assertion failed: %s at %s:%d\n", #expr, __FILE__, __LINE__); abort(); } } while (0)

typedef uint16_t bjvm_char_t;

typedef enum {
	BJVM_PRIMITIVE_BOOLEAN = 4,
	BJVM_PRIMITIVE_CHAR = 5,
	BJVM_PRIMITIVE_FLOAT = 6,
	BJVM_PRIMITIVE_DOUBLE = 7,
	BJVM_PRIMITIVE_BYTE = 8,
	BJVM_PRIMITIVE_SHORT = 9,
	BJVM_PRIMITIVE_INT = 10,
	BJVM_PRIMITIVE_LONG = 11,
	BJVM_PRIMITIVE_VOID = 12
} bjvm_primitive_kind;

const char *bjvm_primitive_kind_to_string(bjvm_primitive_kind kind) {
	switch (kind) {
		case BJVM_PRIMITIVE_BOOLEAN: return "boolean";
		case BJVM_PRIMITIVE_BYTE: return "byte";
		case BJVM_PRIMITIVE_CHAR: return "char";
		case BJVM_PRIMITIVE_SHORT: return "short";
		case BJVM_PRIMITIVE_INT: return "int";
		case BJVM_PRIMITIVE_LONG: return "long";
		case BJVM_PRIMITIVE_FLOAT: return "float";
		case BJVM_PRIMITIVE_DOUBLE: return "double";
		case BJVM_PRIMITIVE_VOID: return "void";
	}
	BJVM_UNREACHABLE();
}

/**
 * Parse primitive type
 * @param c
 * @return
 */
int bjvm_parse_primitive_type(char c) {
	switch (c) {
		case 'Z': return BJVM_PRIMITIVE_BOOLEAN;
		case 'B': return BJVM_PRIMITIVE_BYTE;
		case 'C': return BJVM_PRIMITIVE_CHAR;
		case 'S': return BJVM_PRIMITIVE_SHORT;
		case 'I': return BJVM_PRIMITIVE_INT;
		case 'J': return BJVM_PRIMITIVE_LONG;
		case 'F': return BJVM_PRIMITIVE_FLOAT;
		case 'D': return BJVM_PRIMITIVE_DOUBLE;
		case 'V': return BJVM_PRIMITIVE_VOID;
		default: return -1;
	}
}

typedef enum {
	BJVM_CP_KIND_INVALID = 0,
	BJVM_CP_KIND_UTF8 = 1 << 0,
	BJVM_CP_KIND_INTEGER = 1 << 1,
	BJVM_CP_KIND_FLOAT = 1 << 2,
	BJVM_CP_KIND_LONG = 1 << 3,
	BJVM_CP_KIND_DOUBLE = 1 << 4,
	BJVM_CP_KIND_CLASS = 1 << 5,
	BJVM_CP_KIND_STRING = 1 << 6,
	BJVM_CP_KIND_FIELD_REF = 1 << 7,
	BJVM_CP_KIND_METHOD_REF = 1 << 8,
	BJVM_CP_KIND_INTERFACE_METHOD_REF = 1 << 9,
	BJVM_CP_KIND_NAME_AND_TYPE = 1 << 10,
	BJVM_CP_KIND_METHOD_HANDLE = 1 << 11,
	BJVM_CP_KIND_METHOD_TYPE = 1 << 12,
	BJVM_CP_KIND_INVOKE_DYNAMIC = 1 << 13,

	BJVM_CP_KIND_MAX
} bjvm_cp_entry_kind;

const char *bjvm_cp_entry_kind_to_string(bjvm_cp_entry_kind kind) {
	switch (kind) {
		case BJVM_CP_KIND_INVALID: return "invalid";
		case BJVM_CP_KIND_UTF8: return "utf8";
		case BJVM_CP_KIND_INTEGER: return "integer";
		case BJVM_CP_KIND_FLOAT: return "float";
		case BJVM_CP_KIND_LONG: return "long";
		case BJVM_CP_KIND_DOUBLE: return "double";
		case BJVM_CP_KIND_CLASS: return "class";
		case BJVM_CP_KIND_STRING: return "string";
		case BJVM_CP_KIND_FIELD_REF: return "field";
		case BJVM_CP_KIND_METHOD_REF: return "method";
		case BJVM_CP_KIND_INTERFACE_METHOD_REF: return "interfacemethod";
		case BJVM_CP_KIND_NAME_AND_TYPE: return "nameandtype";
		case BJVM_CP_KIND_METHOD_HANDLE: return "methodhandle";
		case BJVM_CP_KIND_METHOD_TYPE: return "methodtype";
		case BJVM_CP_KIND_INVOKE_DYNAMIC: return "invokedynamic";
		default: BJVM_UNREACHABLE();
	}
}

typedef struct {
	bjvm_char_t *chars;
	int len;
} bjvm_cp_utf8_entry;

bool compare_utf8_entry(bjvm_cp_utf8_entry *entry, const char *str) {
	if (entry->len != strlen(str))
		return false;
	for (int i = 0; i < entry->len; ++i)
		if (entry->chars[i] != str[i])
			return false;
	return true;
}

typedef struct {
	bjvm_cp_utf8_entry *name;
} bjvm_cp_class_info;

typedef struct bjvm_cp_name_and_type bjvm_cp_name_and_type;

typedef struct {
	bjvm_cp_class_info *class_info;
	bjvm_cp_name_and_type *name_and_type;
} bjvm_cp_fieldref_info;

// Used by both methodref and interface methodref
typedef struct {
	bjvm_cp_class_info *class_info;
	bjvm_cp_name_and_type *name_and_type;
} bjvm_cp_methodref_info;

typedef struct {
	bjvm_cp_utf8_entry *chars;
} bjvm_cp_string_info;

typedef struct bjvm_cp_name_and_type {
	bjvm_cp_utf8_entry *name;
	bjvm_cp_utf8_entry *descriptor;
} bjvm_cp_name_and_type;

typedef struct {
	// Sign extended if original entry was an Integer
	int64_t value;
} bjvm_cp_integral_info;

typedef struct {
	// Extended if original entry was a Float
	double value;
} bjvm_cp_floating_info;

typedef enum {
	BJVM_MH_KIND_GET_FIELD = 1,
	BJVM_MH_KIND_GET_STATIC = 2,
	BJVM_MH_KIND_PUT_FIELD = 3,
	BJVM_MH_KIND_PUT_STATIC = 4,
	BJVM_MH_KIND_INVOKE_VIRTUAL = 5,
	BJVM_MH_KIND_INVOKE_STATIC = 6,
	BJVM_MH_KIND_INVOKE_SPECIAL = 7,
	BJVM_MH_KIND_NEW_INVOKE_SPECIAL = 8,
	BJVM_MH_KIND_INVOKE_INTERFACE = 9
} bjvm_method_handle_kind;

typedef struct {
	bjvm_method_handle_kind handle_kind;
	uint16_t reference_index; // TODO
} bjvm_cp_method_handle_info;

typedef struct {
	bjvm_cp_utf8_entry *descriptor;
} bjvm_cp_method_type_info;

typedef struct {
	uint16_t bootstrap_method_attr_index; // TODO convert to pointer
	bjvm_cp_name_and_type *name_and_type;
} bjvm_cp_invoke_dynamic_info;

bjvm_cp_utf8_entry bjvm_init_utf8_entry(int len) {
	return (bjvm_cp_utf8_entry){
		.chars = calloc(len, sizeof(bjvm_char_t)),
		.len = len
	};
}

void bjvm_free_utf8_entry(bjvm_cp_utf8_entry *entry) {
	free(entry->chars);
	entry->chars = NULL;
	entry->len = 0;
}

typedef struct bjvm_constant_pool_entry {
	bjvm_cp_entry_kind kind;

	union {
		bjvm_cp_utf8_entry utf8;
		bjvm_cp_string_info string;

		bjvm_cp_floating_info floating;
		bjvm_cp_integral_info integral;

		bjvm_cp_name_and_type name_and_type;
		bjvm_cp_class_info class_info;

		bjvm_cp_fieldref_info fieldref_info;
		bjvm_cp_methodref_info methodref_info;
		bjvm_cp_method_handle_info method_handle_info;
		bjvm_cp_method_type_info method_type_info;
		bjvm_cp_invoke_dynamic_info invoke_dynamic_info;
	} data;
} bjvm_constant_pool_entry;

void bjvm_free_constant_pool_entry(bjvm_constant_pool_entry *entry) {
	switch (entry->kind) {
		case BJVM_CP_KIND_UTF8:
			bjvm_free_utf8_entry(&entry->data.utf8);
			break;
		default: // TODO will need to add more as we resolve descriptors
			break;
	}
}

typedef struct bjvm_constant_pool {
	int entries_len;
	bjvm_constant_pool_entry entries[];
} bjvm_constant_pool;

typedef struct bjvm_cp_method bjvm_cp_method;

typedef enum {
	BJVM_ACCESS_PUBLIC = 0x0001,
	BJVM_ACCESS_PRIVATE = 0x0002,
	BJVM_ACCESS_PROTECTED = 0x0004,
	BJVM_ACCESS_STATIC = 0x0008,
	BJVM_ACCESS_FINAL = 0x0010,
	BJVM_ACCESS_SYNCHRONIZED = 0x0020,
	BJVM_ACCESS_BRIDGE = 0x0040,
	BJVM_ACCESS_VARARGS = 0x0080,
	BJVM_ACCESS_NATIVE = 0x0100,
	BJVM_ACCESS_INTERFACE = 0x0200,
	BJVM_ACCESS_ABSTRACT = 0x0400,
	BJVM_ACCESS_STRICT = 0x0800,
	BJVM_ACCESS_SYNTHETIC = 0x1000,
	BJVM_ACCESS_ANNOTATION = 0x2000,
	BJVM_ACCESS_ENUM = 0x4000
} bjvm_access_flags;

typedef struct {
	uint16_t minor_version;
	uint16_t major_version;

	bjvm_constant_pool* pool;

	bjvm_access_flags access_flags;
	bjvm_cp_class_info* this_class;
	bjvm_cp_class_info* super_class;

	int interfaces_count;
	bjvm_cp_class_info** interfaces;

	int fields_count;
	bjvm_cp_fieldref_info** fields;

	int methods_count;
	bjvm_cp_method** methods;

	int attributes_count;
	void** attributes;
} bjvm_classfile;

void bjvm_free_method(bjvm_cp_method* method);

void bjvm_free_constant_pool(bjvm_constant_pool * pool) {
	for (int i = 0; i < pool->entries_len; ++i) {
		bjvm_free_constant_pool_entry(&pool->entries[i]);
	}
}

void bjvm_free_classfile(bjvm_classfile* cf) {
	bjvm_free_constant_pool(cf->pool);
	for (int i = 0; i < cf->methods_count; ++i) {
		bjvm_free_method(cf->methods[i]);
	}
}

/**
 * Get the constant pool entry associated with the given index. Aborts (as opposed to raising a VerifyError) upon an
 * invalid access.
 * @return Pointer to the cp entry.
 */
bjvm_constant_pool_entry *get_constant_pool_entry(bjvm_constant_pool *pool, int index) {
	BJVM_DCHECK(index >= 0 && index < pool->entries_len);
	return &pool->entries[index];
}

/**
 * Instruction code. Similar instructions like aload_0 are canonicalised to aload with an argument of 0.
 *
 * List of canonicalisations:
 *   aload_<n> -> aload, dload_<n> -> dload, fload_<n> -> fload, iload_<n> -> iload, lload_<n> -> lload,
 *   astore_<n> -> astore, dstore_<n> -> dstore, fstore_<n> -> fstore, istore_<n> -> istore, lstore_<n> -> lstore,
 *   bipush, sipush, iconst_<n>, iconst_<n> -> iconst, dconst_<d> -> dconst, fconst_<f> -> fconst
 */
typedef enum {
	/** No operands */
	bjvm_bc_insn_nop,

	bjvm_bc_insn_aaload, bjvm_bc_insn_aastore, bjvm_bc_insn_aconst_null, bjvm_bc_insn_areturn, bjvm_bc_insn_arraylength,
	bjvm_bc_insn_athrow, bjvm_bc_insn_baload, bjvm_bc_insn_bastore, bjvm_bc_insn_caload, bjvm_bc_insn_castore,
	bjvm_bc_insn_d2f, bjvm_bc_insn_d2i,
	bjvm_bc_insn_d2l, bjvm_bc_insn_dadd, bjvm_bc_insn_daload, bjvm_bc_insn_dastore, bjvm_bc_insn_dcmpg,
	bjvm_bc_insn_dcmpl, bjvm_bc_insn_ddiv, bjvm_bc_insn_dmul, bjvm_bc_insn_dneg, bjvm_bc_insn_drem,
	bjvm_bc_insn_dreturn, bjvm_bc_insn_dsub, bjvm_bc_insn_dup, bjvm_bc_insn_dup_x1, bjvm_bc_insn_dup_x2,
	bjvm_bc_insn_dup2, bjvm_bc_insn_dup2_x1,
	bjvm_bc_insn_dup2_x2, bjvm_bc_insn_f2d, bjvm_bc_insn_f2i, bjvm_bc_insn_f2l, bjvm_bc_insn_fadd, bjvm_bc_insn_faload,
	bjvm_bc_insn_fastore, bjvm_bc_insn_fcmpg, bjvm_bc_insn_fcmpl, bjvm_bc_insn_fdiv, bjvm_bc_insn_fmul,
	bjvm_bc_insn_fneg, bjvm_bc_insn_frem, bjvm_bc_insn_freturn, bjvm_bc_insn_fsub, bjvm_bc_insn_i2b, bjvm_bc_insn_i2c,
	bjvm_bc_insn_i2d, bjvm_bc_insn_i2f, bjvm_bc_insn_i2l, bjvm_bc_insn_i2s, bjvm_bc_insn_iadd, bjvm_bc_insn_iaload,
	bjvm_bc_insn_iand, bjvm_bc_insn_iastore, bjvm_bc_insn_idiv, bjvm_bc_insn_imul, bjvm_bc_insn_ineg, bjvm_bc_insn_ior,
	bjvm_bc_insn_irem, bjvm_bc_insn_ireturn, bjvm_bc_insn_ishl, bjvm_bc_insn_ishr, bjvm_bc_insn_isub,
	bjvm_bc_insn_iushr, bjvm_bc_insn_ixor, bjvm_bc_insn_l2d,
	bjvm_bc_insn_l2f, bjvm_bc_insn_l2i, bjvm_bc_insn_ladd, bjvm_bc_insn_laload, bjvm_bc_insn_land, bjvm_bc_insn_lastore,
	bjvm_bc_insn_lcmp, bjvm_bc_insn_ldiv, bjvm_bc_insn_lmul, bjvm_bc_insn_lneg, bjvm_bc_insn_lor, bjvm_bc_insn_lrem,
	bjvm_bc_insn_lreturn, bjvm_bc_insn_lshl, bjvm_bc_insn_lshr,
	bjvm_bc_insn_lsub, bjvm_bc_insn_lushr, bjvm_bc_insn_lxor, bjvm_bc_insn_monitorenter, bjvm_bc_insn_monitorexit,
	bjvm_bc_insn_pop, bjvm_bc_insn_pop2, bjvm_bc_insn_return, bjvm_bc_insn_saload, bjvm_bc_insn_sastore,
	bjvm_bc_insn_swap,

	/** Indexes into constant pool */
	bjvm_bc_insn_anewarray, bjvm_bc_insn_checkcast, bjvm_bc_insn_getfield, bjvm_bc_insn_getstatic,
	bjvm_bc_insn_instanceof, bjvm_bc_insn_invokedynamic, bjvm_bc_insn_new, bjvm_bc_insn_putfield,
	bjvm_bc_insn_putstatic, bjvm_bc_insn_invokevirtual,
	bjvm_bc_insn_invokespecial, bjvm_bc_insn_invokestatic, bjvm_bc_insn_ldc, bjvm_bc_insn_ldc2_w,

	/** Indexes into local variable table */
	bjvm_bc_insn_dload, bjvm_bc_insn_fload, bjvm_bc_insn_iload, bjvm_bc_insn_lload, bjvm_bc_insn_dstore,
	bjvm_bc_insn_fstore, bjvm_bc_insn_istore, bjvm_bc_insn_lstore, bjvm_bc_insn_aload, bjvm_bc_insn_astore,

	/** Indexes into instruction table */
	bjvm_bc_insn_goto_, bjvm_bc_insn_jsr,

	bjvm_bc_insn_if_acmpeq, bjvm_bc_insn_if_acmpne, bjvm_bc_insn_if_icmpeq, bjvm_bc_insn_if_icmpne,
	bjvm_bc_insn_if_icmplt, bjvm_bc_insn_if_icmpge,
	bjvm_bc_insn_if_icmpgt, bjvm_bc_insn_if_icmple, bjvm_bc_insn_ifeq, bjvm_bc_insn_ifne, bjvm_bc_insn_iflt,
	bjvm_bc_insn_ifge, bjvm_bc_insn_ifgt, bjvm_bc_insn_ifle, bjvm_bc_insn_ifnonnull, bjvm_bc_insn_ifnull,

	/** Has some numerical immediate */
	bjvm_bc_insn_iconst, bjvm_bc_insn_dconst, bjvm_bc_insn_fconst, bjvm_bc_insn_lconst,

	/** Cursed */
	bjvm_bc_insn_iinc, bjvm_bc_insn_invokeinterface, bjvm_bc_insn_multianewarray, bjvm_bc_insn_newarray,
	bjvm_bc_insn_tableswitch, bjvm_bc_insn_lookupswitch, bjvm_bc_insn_ret
} bjvm_insn_code_kind;

struct bjvm_bc_tableswitch_data {
	int default_target;
	int *targets;
	int targets_count;

	int low, high;
};

struct bjvm_bc_lookupswitch_data {
	int default_target;
	int *targets;
	int targets_count;

	int *keys;
	int keys_count;
};

struct bjvm_bc_iinc_data {
	uint16_t index;
	int16_t const_;
};

struct bjvm_bc_invokeinterface_data {
	uint16_t index;
	uint8_t count;
};

struct bjvm_multianewarray_data {
	uint16_t index;
	uint8_t dimensions;
};

typedef struct {
	bjvm_insn_code_kind kind;
	int program_counter;

	union {
		// for newarray
		bjvm_primitive_kind array_type;
		// constant pool index or local variable index or branch target (instruction index)
		uint16_t index;
		// Integer or long immediate
		long integer_imm;
		// Float immediate
		float f_imm;
		// Double immediate
		double d_imm;
		// lookupswitch
		struct bjvm_bc_lookupswitch_data lookupswitch;
		// tableswitch
		struct bjvm_bc_tableswitch_data tableswitch;
		// iinc
		struct bjvm_bc_iinc_data iinc;
		// invoke interface
		struct bjvm_bc_invokeinterface_data invokeinterface;
		// multianewarray
		struct bjvm_multianewarray_data multianewarray;
		// non-owned pointer into the constant pool
		bjvm_constant_pool_entry *cp;
	} insn_data;
} bjvm_bytecode_insn;

void init_lookupswitch_data(struct bjvm_bc_lookupswitch_data *data, int count) {
	data->keys = calloc(count, sizeof(int));
	data->keys_count = count;
}

void free_lookupswitch_data(struct bjvm_bc_lookupswitch_data *data) {
	free(data->keys);
}

typedef enum {
	BJVM_ATTRIBUTE_KIND_CODE = 0,
} bjvm_attribute_kind;

typedef struct {
	uint16_t max_stack;
	uint16_t max_locals;
	uint32_t code_length;
	int max_pc;

	bjvm_bytecode_insn *code;
} bjvm_attribute_code;

void free_code_attribute(bjvm_attribute_code *code) {
	free(code->code);
}

typedef struct bjvm_attribute {
	bjvm_attribute_kind kind;
	bjvm_cp_utf8_entry *name;
	uint32_t length;

	union {
		bjvm_attribute_code code;
	};
} bjvm_attribute;

void bjvm_free_attribute(bjvm_attribute* attribute) {
	switch (attribute->kind) {
		case BJVM_ATTRIBUTE_KIND_CODE:
			free_code_attribute(&attribute->code);
			break;
	}
}

typedef struct bjvm_cp_method {
	bjvm_bytecode_insn* insns;
	int insn_count;

	bjvm_access_flags access_flags;

	bjvm_cp_utf8_entry* name;
	bjvm_cp_utf8_entry* descriptor;

	int attributes_count;
	bjvm_attribute** attributes;
} bjvm_cp_method;

void bjvm_free_method(bjvm_cp_method* method) {
	free(method->insns);
	for (int i = 0; i < method->attributes_count; ++i) {
		bjvm_free_attribute(method->attributes[i]);
	}
}

typedef struct {
	uint8_t *bytes;
	size_t len;
} cf_byteslice;

// Thread local jmp_buf for verify error
_Thread_local static jmp_buf verify_error_jmp_buf;
_Thread_local char *verify_error_msg = NULL;
_Thread_local bool verify_error_needs_free = false;

_Noreturn void verify_error(const char *reason) {
	verify_error_msg = (char *) reason;
	verify_error_needs_free = false;
	longjmp(verify_error_jmp_buf, 1);
}

_Noreturn void verify_error_with_free(char *reason) {
	verify_error_msg = reason;
	verify_error_needs_free = true;
	longjmp(verify_error_jmp_buf, 1);
}

#define READER_NEXT_IMPL(name, type) \
	type name(cf_byteslice* reader, const char* reason) { \
		if (reader->len < sizeof(type)) { \
			verify_error(reason); \
		} \
		char data[sizeof(type)]; \
		memcpy(data, reader->bytes, sizeof(type)); \
		if (sizeof(type) == 2) { \
			*(uint16_t*)data = __bswap_16(*(uint16_t*)data); \
		} else if (sizeof(type) == 4) { \
			*(uint32_t*)data = __bswap_32(*(uint32_t*)data); \
		} else if (sizeof(type) == 8) { \
			*(uint64_t*)data = __bswap_64(*(uint64_t*)data); \
		} \
		return *(type*)data; \
	}

READER_NEXT_IMPL(reader_next_u8, uint8_t)
READER_NEXT_IMPL(reader_next_i8, int8_t)
READER_NEXT_IMPL(reader_next_u16, uint16_t)
READER_NEXT_IMPL(reader_next_i16, int16_t)
READER_NEXT_IMPL(reader_next_u32, uint32_t)
READER_NEXT_IMPL(reader_next_i32, int32_t)
READER_NEXT_IMPL(reader_next_u64, uint64_t)
READER_NEXT_IMPL(reader_next_i64, int64_t)
READER_NEXT_IMPL(reader_next_f32, float)
READER_NEXT_IMPL(reader_next_f64, double)

cf_byteslice reader_get_slice(cf_byteslice *reader, size_t len, const char *reason) {
	if (reader->len < len) {
		verify_error(reason);
	}

	cf_byteslice result = {.bytes = reader->bytes, .len = len};
	reader->bytes += len;
	reader->len -= len;
	return result;
}

#define VECTOR_PUSH(vector, vector_cap, vector_count) \
	({ \
		if (vector_count == vector_cap) { \
		    int overflow; \
			vector_cap = __builtin_mul_overflow(vector_cap, 2, &overflow); \
			BJVM_DCHECK(!overflow); \
			void* next = realloc(vector, vector_cap * sizeof(*vector)); \
			if (!next) { \
				fprintf(stderr, "Out of memory\n"); \
				abort(); \
			} \
			vector = next; \
		} \
		&vector[vector_count++]; \
	})

typedef struct {
	// Free these pointers if a verify error happens, to avoid memleaks
	void **free_on_error;
	int free_on_error_cap;
	int free_on_error_count;

	int current_code_max_pc;

	bjvm_constant_pool *cp;
} bjvm_classfile_parse_ctx;

/**
 * Record that this pointer needs to be freed if we encounter a VerifyError while parsing the classfile.
 */
void needs_free_on_verify_error(bjvm_classfile_parse_ctx *ctx, void *ptr) {
	*VECTOR_PUSH(ctx->free_on_error, ctx->free_on_error_count, ctx->free_on_error_cap) = ptr;
}

// See: 4.4.7. The CONSTANT_Utf8_info Structure
bjvm_cp_utf8_entry parse_modified_utf8(const uint8_t *bytes, int len) {
	bjvm_cp_utf8_entry result = bjvm_init_utf8_entry(len); // conservatively large
	int j = 0;
	for (int i = 0; i < len; ++i) {
		// "Code points in the range '\u0001' to '\u007F' are represented by a single byte"
		if (bytes[i] >= 0x01 && bytes[i] <= 0x7F) {
			result.chars[j++] = bytes[i];
		} else if ((bytes[i] & 0xE0) == 0xC0) {
			// "Code points in the range '\u0080' to '\u07FF' are represented by two bytes"
			if (i >= len - 1)
				goto inval;
			result.chars[j++] = ((bytes[i] & 0x1F) << 6) | (bytes[i + 1] & 0x3F);
			i++;
		} else if ((bytes[i] & 0xF0) == 0xE0) {
			// "Code points in the range '\u0800' to '\uFFFF' are represented by three bytes"
			if (i >= len - 2)
				goto inval;
			result.chars[j++] = ((bytes[i] & 0x0F) << 12) | ((bytes[i + 1] & 0x3F) << 6) | (bytes[i + 2] & 0x3F);
			i += 2;
		} else {
			// "No byte may have the value (byte)0 or lie in the range (byte)0xf0 - (byte)0xff."
			goto inval;
		}
	}
	result.len = j;
	return result;
inval:
	bjvm_free_utf8_entry(&result);
	verify_error("Invalid UTF-8 sequence");
}

bjvm_constant_pool_entry *checked_get_constant_pool_entry(bjvm_constant_pool *pool, int index, int expected_kinds) {
	if (!(index >= 0 && index < pool->entries_len)) {
		char buf[128] = {0};
		snprintf(buf, sizeof(buf), "Invalid constant pool entry index %d (pool size %d)", index, pool->entries_len);
		verify_error_with_free(strdup(buf));
	}

	bjvm_constant_pool_entry *entry = pool->entries + index;
	if (1 << entry->kind & expected_kinds)
		return entry;

	char buf[1000] = {0};
	char *write = buf + sprintf(buf, "Unexpected constant pool entry kind %d at index %d (expected one of: [ ",
	                            entry->kind, index);
	for (int i = 0; i < BJVM_CP_KIND_MAX; ++i) {
		if (expected_kinds & (1 << i)) {
			write += sprintf(
				write,
				"%s ",
				bjvm_cp_entry_kind_to_string(1 << i)
			);
		}
	}
	sprintf(write, "])");
	verify_error_with_free(strdup(buf));
}

bjvm_cp_utf8_entry *checked_get_utf8_entry(bjvm_constant_pool *pool, int index) {
	return &checked_get_constant_pool_entry(pool, index, BJVM_CP_KIND_UTF8)->data.utf8;
}

/**
 * Parse a single constant pool entry.
 * @param reader The reader to parse from.
 * @param ctx The parse context.
 * @param suppress_resolution If true, do not resolve anything -- just what the kind is.
 * @return The resolved entry.
 */
bjvm_constant_pool_entry parse_constant_pool_entry(cf_byteslice *reader, bjvm_classfile_parse_ctx *ctx, bool suppress_resolution) {
	enum {
		CONSTANT_Class = 7,
		CONSTANT_Fieldref = 9,
		CONSTANT_Methodref = 10,
		CONSTANT_InterfaceMethodref = 11,
		CONSTANT_String = 8,
		CONSTANT_Integer = 3,
		CONSTANT_Float = 4,
		CONSTANT_Long = 5,
		CONSTANT_Double = 6,
		CONSTANT_NameAndType = 12,
		CONSTANT_Utf8 = 1,
		CONSTANT_MethodHandle = 15,
		CONSTANT_MethodType = 16,
		CONSTANT_InvokeDynamic = 18,
	};

	uint8_t kind = reader_next_u8(reader, "cp kind");
	switch (kind) {
		case CONSTANT_Class: {
			uint16_t index = reader_next_u16(reader, "class index");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_CLASS,
				.data.class_info = {
					.name = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, index)
				}
			};
		}
		case CONSTANT_Fieldref:
		case CONSTANT_Methodref:
		case CONSTANT_InterfaceMethodref: {
			uint16_t class_index = reader_next_u16(reader, "class index");
			uint16_t name_and_type_index = reader_next_u16(reader, "name and type index");

			bjvm_cp_entry_kind entry_kind = kind == CONSTANT_Fieldref
			                          ? BJVM_CP_KIND_FIELD_REF
			                          : kind == CONSTANT_Methodref
			                            ? BJVM_CP_KIND_METHOD_REF
			                            : BJVM_CP_KIND_INTERFACE_METHOD_REF;
			bjvm_cp_class_info* class_info = suppress_resolution
									  ? NULL
									  : checked_get_constant_pool_entry(ctx->cp, class_index, BJVM_CP_KIND_CLASS)->data.
									  class_info;

			bjvm_cp_name_and_type* name_and_type = suppress_resolution
										 ? NULL
										 : checked_get_constant_pool_entry(
											 ctx->cp, name_and_type_index,
											 BJVM_CP_KIND_NAME_AND_TYPE)->data.name_and_type;

			return (bjvm_constant_pool_entry){
				.kind = entry_kind,
				.data.methodref_info = {
					.class_info = class_info,
					.name_and_type = name_and_type
				}
			};
		}
		case CONSTANT_String: {
			uint16_t index = reader_next_u16(reader, "string index");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_STRING,
				.data.string = {
					.chars = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, index)
				}
			};
		}
		case CONSTANT_Integer: {
			int32_t value = reader_next_i32(reader, "integer value");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_INTEGER,
				.data.integral = {.value = value}
			};
		}
		case CONSTANT_Float: {
			double value = reader_next_f32(reader, "double value");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_FLOAT,
				.data.floating = {.value = value}
			};
		}
		case CONSTANT_Long: {
			int64_t value = reader_next_i64(reader, "long value");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_LONG,
				.data.integral = {.value = value}
			};
		}
		case CONSTANT_Double: {
			double value = reader_next_f64(reader, "double value");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_DOUBLE,
				.data.floating = {.value = value}
			};
		}
		case CONSTANT_NameAndType: {
			uint16_t name_index = reader_next_u16(reader, "name index");
			uint16_t descriptor_index = reader_next_u16(reader, "descriptor index");

			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_NAME_AND_TYPE,
				.data.name_and_type = {
					.name = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, name_index),
					.descriptor = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, descriptor_index)
				}
			};
		}
		case CONSTANT_Utf8: {
			uint16_t length = reader_next_u16(reader, "utf8 length");
			cf_byteslice bytes_reader = reader_get_slice(reader, length, "utf8 data");

			bjvm_cp_utf8_entry utf8 = {};

			if (!suppress_resolution) {
				utf8 = parse_modified_utf8(bytes_reader.bytes, length);
				needs_free_on_verify_error(ctx, utf8.chars);
			}

			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_UTF8,
				.data.utf8 = utf8
			};
		}
		case CONSTANT_MethodHandle: {
			return (bjvm_constant_pool_entry) {
				.kind = BJVM_CP_KIND_METHOD_HANDLE,
				.data.method_handle_info = {
					.handle_kind = reader_next_u8(reader, "method handle kind"),
					.reference_index = reader_next_u16(reader, "reference index")
				}
			};
		}
		case CONSTANT_MethodType: {
			return (bjvm_constant_pool_entry) {
				.kind = BJVM_CP_KIND_METHOD_TYPE,
				.data.method_type_info = {
					.descriptor = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "descriptor index"))
				}
			};
		}
		case CONSTANT_InvokeDynamic: {
			return (bjvm_constant_pool_entry) {
				.kind = BJVM_CP_KIND_INVOKE_DYNAMIC,
				.data.invoke_dynamic_info = {
					.bootstrap_method_attr_index = reader_next_u16(reader, "bootstrap method attr index"),
					.name_and_type = suppress_resolution ? NULL : checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "name and type index"), BJVM_CP_KIND_NAME_AND_TYPE)->data.name_and_type
				}
			};
		}
		default:
			verify_error("Invalid constant pool entry kind");
	}
}

bjvm_constant_pool* init_constant_pool(uint16_t count) {
	bjvm_constant_pool *pool = calloc(1, sizeof(bjvm_constant_pool) + (count + 1) * sizeof(bjvm_constant_pool_entry));
	pool->entries_len = count + 1;
	return pool;
}

bool is_entry_wide(bjvm_constant_pool_entry * ent) {
	return ent->kind == BJVM_CP_KIND_DOUBLE || ent->kind == BJVM_CP_KIND_LONG;
}

bjvm_constant_pool* parse_constant_pool(cf_byteslice* reader, bjvm_classfile_parse_ctx *ctx) {
	uint16_t count = reader_next_u16(reader, "constant pool count");

	bjvm_constant_pool* pool = init_constant_pool(count);
	ctx->cp = pool;
	needs_free_on_verify_error(ctx, pool);

	get_constant_pool_entry(pool, 0)->kind = BJVM_CP_KIND_INVALID;  // entry at 0 is always invalid
	cf_byteslice initial_reader_state = *reader;
	for (int i = 0; i < 2; ++i) {
		for (int cp_i = 1; cp_i < count; ++cp_i) {
			// Read first, then link
			*get_constant_pool_entry(pool, cp_i) = parse_constant_pool_entry(reader, ctx, !(bool)i);
			if (is_entry_wide(get_constant_pool_entry(pool, cp_i)))
				cp_i++;
		}
		if (i == 0)
			*reader = initial_reader_state;
	}

	return pool;
}

int checked_pc(int insn_pc, int offset, bjvm_classfile_parse_ctx *ctx) {
	int overflow;
	int target = __builtin_add_overflow(insn_pc, offset, &overflow);
	if (overflow || target < 0 || target >= ctx->current_code_max_pc) {
		verify_error_msg = "Branch target out of bounds";
		longjmp(verify_error_jmp_buf, 1);
	}
	return target;
}

bjvm_bytecode_insn parse_tableswitch_insn(cf_byteslice* reader, int pc, bjvm_classfile_parse_ctx* ctx) {
	int original_pc = pc - 1;

	// consume u8s until pc = 0 mod 4
	while (pc % 4 != 0) {
		reader_next_u8(reader, "tableswitch padding");
		pc++;
	}

	int default_target = checked_pc(original_pc, reader_next_i32(reader, "tableswitch default target"), ctx);
	int low = reader_next_i32(reader, "tableswitch low");
	int high = reader_next_i32(reader, "tableswitch high");
	int targets_count = high - low + 1;

	int* targets = malloc(targets_count * sizeof(int));
	needs_free_on_verify_error(ctx, targets);
	for (int i = 0; i < targets_count; ++i) {
		targets[i] = checked_pc(original_pc, reader_next_i32(reader, "tableswitch target"), ctx);
	}

	return (bjvm_bytecode_insn){
		.kind = bjvm_bc_insn_tableswitch,
		.program_counter = original_pc,
		.insn_data.tableswitch = {
			.default_target = default_target,
			.low = low,
			.high = high,
			.targets = targets,
			.targets_count = targets_count
		}
	};
}

bjvm_bytecode_insn parse_lookupswitch_insn(cf_byteslice* reader, int pc, bjvm_classfile_parse_ctx * ctx) {
	int original_pc = pc - 1;

	while (pc % 4 != 0) {
		reader_next_u8(reader, "tableswitch padding");
		pc++;
	}

	int default_target = checked_pc(original_pc, reader_next_i32(reader, "lookupswitch default target"), ctx);
	int pairs_count = reader_next_i32(reader, "lookupswitch pairs count");

	int* keys = malloc(pairs_count * sizeof(int));
	int* targets = malloc(pairs_count * sizeof(int));
	needs_free_on_verify_error(ctx, keys);
	needs_free_on_verify_error(ctx, targets);

	for (int i = 0; i < pairs_count; ++i) {
		keys[i] = reader_next_i32(reader, "lookupswitch key");
		targets[i] = checked_pc(original_pc, reader_next_i32(reader, "lookupswitch target"), ctx);
	}

	return (bjvm_bytecode_insn){
		.kind = bjvm_bc_insn_lookupswitch,
		.program_counter = original_pc,
		.insn_data.lookupswitch = {
			.default_target = default_target,
			.keys = keys,
			.keys_count = pairs_count,
			.targets = targets,
			.targets_count = pairs_count
		}
	};
}

bjvm_bytecode_insn parse_insn_impl(cf_byteslice *reader, int pc, bjvm_classfile_parse_ctx *ctx) {
	/** Raw instruction codes (to be canonicalized). */
	enum {
		nop = 0x00,
		aconst_null = 0x01,
		iconst_m1 = 0x02,
		iconst_0 = 0x03,
		iconst_1 = 0x04,
		iconst_2 = 0x05,
		iconst_3 = 0x06,
		iconst_4 = 0x07,
		iconst_5 = 0x08,
		lconst_0 = 0x09,
		lconst_1 = 0x0a,
		fconst_0 = 0x0b,
		fconst_1 = 0x0c,
		fconst_2 = 0x0d,
		dconst_0 = 0x0e,
		dconst_1 = 0x0f,
		bipush = 0x10,
		sipush = 0x11,
		ldc = 0x12,
		ldc_w = 0x13,
		ldc2_w = 0x14,
		iload = 0x15,
		lload = 0x16,
		fload = 0x17,
		dload = 0x18,
		aload = 0x19,
		iload_0 = 0x1a,
		iload_1 = 0x1b,
		iload_2 = 0x1c,
		iload_3 = 0x1d,
		lload_0 = 0x1e,
		lload_1 = 0x1f,
		lload_2 = 0x20,
		lload_3 = 0x21,
		fload_0 = 0x22,
		fload_1 = 0x23,
		fload_2 = 0x24,
		fload_3 = 0x25,
		dload_0 = 0x26,
		dload_1 = 0x27,
		dload_2 = 0x28,
		dload_3 = 0x29,
		aload_0 = 0x2a,
		aload_1 = 0x2b,
		aload_2 = 0x2c,
		aload_3 = 0x2d,
		iaload = 0x2e,
		laload = 0x2f,
		faload = 0x30,
		daload = 0x31,
		aaload = 0x32,
		baload = 0x33,
		caload = 0x34,
		saload = 0x35,
		istore = 0x36,
		lstore = 0x37,
		fstore = 0x38,
		dstore = 0x39,
		astore = 0x3a,
		istore_0 = 0x3b,
		istore_1 = 0x3c,
		istore_2 = 0x3d,
		istore_3 = 0x3e,
		lstore_0 = 0x3f,
		lstore_1 = 0x40,
		lstore_2 = 0x41,
		lstore_3 = 0x42,
		fstore_0 = 0x43,
		fstore_1 = 0x44,
		fstore_2 = 0x45,
		fstore_3 = 0x46,
		dstore_0 = 0x47,
		dstore_1 = 0x48,
		dstore_2 = 0x49,
		dstore_3 = 0x4a,
		astore_0 = 0x4b,
		astore_1 = 0x4c,
		astore_2 = 0x4d,
		astore_3 = 0x4e,
		iastore = 0x4f,
		lastore = 0x50,
		fastore = 0x51,
		dastore = 0x52,
		aastore = 0x53,
		bastore = 0x54,
		castore = 0x55,
		sastore = 0x56,
		pop = 0x57,
		pop2 = 0x58,
		dup = 0x59,
		dup_x1 = 0x5a,
		dup_x2 = 0x5b,
		dup2 = 0x5c,
		dup2_x1 = 0x5d,
		dup2_x2 = 0x5e,
		swap = 0x5f,
		iadd = 0x60,
		ladd = 0x61,
		fadd = 0x62,
		dadd = 0x63,
		isub = 0x64,
		lsub = 0x65,
		fsub = 0x66,
		dsub = 0x67,
		imul = 0x68,
		lmul = 0x69,
		fmul = 0x6a,
		dmul = 0x6b,
		idiv = 0x6c,
		ldiv = 0x6d,
		fdiv = 0x6e,
		ddiv = 0x6f,
		irem = 0x70,
		lrem = 0x71,
		frem = 0x72,
		drem = 0x73,
		ineg = 0x74,
		lneg = 0x75,
		fneg = 0x76,
		dneg = 0x77,
		ishl = 0x78,
		lshl = 0x79,
		ishr = 0x7a,
		lshr = 0x7b,
		iushr = 0x7c,
		lushr = 0x7d,
		iand = 0x7e,
		land = 0x7f,
		ior = 0x80,
		lor = 0x81,
		ixor = 0x82,
		lxor = 0x83,
		iinc = 0x84,
		i2l = 0x85,
		i2f = 0x86,
		i2d = 0x87,
		l2i = 0x88,
		l2f = 0x89,
		l2d = 0x8a,
		f2i = 0x8b,
		f2l = 0x8c,
		f2d = 0x8d,
		d2i = 0x8e,
		d2l = 0x8f,
		d2f = 0x90,
		i2b = 0x91,
		i2c = 0x92,
		i2s = 0x93,
		lcmp = 0x94,
		fcmpl = 0x95,
		fcmpg = 0x96,
		dcmpl = 0x97,
		dcmpg = 0x98,
		ifeq = 0x99,
		ifne = 0x9a,
		iflt = 0x9b,
		ifge = 0x9c,
		ifgt = 0x9d,
		ifle = 0x9e,
		if_icmpeq = 0x9f,
		if_icmpne = 0xa0,
		if_icmplt = 0xa1,
		if_icmpge = 0xa2,
		if_icmpgt = 0xa3,
		if_icmple = 0xa4,
		if_acmpeq = 0xa5,
		if_acmpne = 0xa6,
		goto_ = 0xa7,
		jsr = 0xa8,
		ret = 0xa9,
		tableswitch = 0xaa,
		lookupswitch = 0xab,
		ireturn = 0xac,
		lreturn = 0xad,
		freturn = 0xae,
		dreturn = 0xaf,
		areturn = 0xb0,
		return_ = 0xb1,
		getstatic = 0xb2,
		putstatic = 0xb3,
		getfield = 0xb4,
		putfield = 0xb5,
		invokevirtual = 0xb6,
		invokespecial = 0xb7,
		invokestatic = 0xb8,
		invokeinterface = 0xb9,
		invokedynamic = 0xba,
		new_ = 0xbb,
		newarray = 0xbc,
		anewarray = 0xbd,
		arraylength = 0xbe,
		athrow = 0xbf,
		checkcast = 0xc0,
		instanceof = 0xc1,
		monitorenter = 0xc2,
		monitorexit = 0xc3,
		wide = 0xc4,
		multianewarray = 0xc5,
		ifnull = 0xc6,
		ifnonnull = 0xc7,
		goto_w = 0xc8,
		jsr_w = 0xc9
	};

	uint8_t opcode = reader_next_u8(reader, "instruction opcode");

	switch (opcode) {
		case nop: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_nop};
		case aconst_null: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aconst_null};
		case iconst_m1:
		case iconst_0:
		case iconst_1:
		case iconst_2:
		case iconst_3:
		case iconst_4:
		case iconst_5:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iconst, .insn_data.integer_imm = opcode - iconst_0};
		case lconst_0:
		case lconst_1:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lconst, .insn_data.integer_imm = opcode - lconst_0};
		case fconst_0:
		case fconst_1:
		case fconst_2:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fconst, .insn_data.f_imm = (float) (opcode - fconst_0)};
		case dconst_0:
		case dconst_1:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dconst, .insn_data.d_imm = (double) (opcode - dconst_0)};
		case bipush:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iconst, .insn_data.integer_imm = reader_next_i8(reader, "bipush immediate")
			};
		case sipush:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iconst, .insn_data.integer_imm = reader_next_i16(reader, "sipush immediate")
			};
		case ldc:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc, .insn_data.index = reader_next_u8(reader, "ldc index")
			};
		case ldc_w:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc, .insn_data.index = reader_next_u16(reader, "ld2_w index")
			};
		case ldc2_w:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc2_w, .insn_data.index = reader_next_u16(reader, "ldc2_w index")
			};
		case iload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iload, .insn_data.index = reader_next_u8(reader, "iload index")
			};
		case lload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_lload, .insn_data.index = reader_next_u8(reader, "lload index")
			};
		case fload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_fload, .insn_data.index = reader_next_u8(reader, "fload index")
			};
		case dload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_dload, .insn_data.index = reader_next_u8(reader, "dload index")
			};
		case aload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_aload, .insn_data.index = reader_next_u8(reader, "aload index")
			};
		case iload_0:
		case iload_1:
		case iload_2:
		case iload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iload, .insn_data.index = opcode - iload_0};
		case lload_0:
		case lload_1:
		case lload_2:
		case lload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lload, .insn_data.index = opcode - lload_0};
		case fload_0:
		case fload_1:
		case fload_2:
		case fload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fload, .insn_data.index = opcode - fload_0};
		case dload_0:
		case dload_1:
		case dload_2:
		case dload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dload, .insn_data.index = opcode - dload_0};
		case aload_0:
		case aload_1:
		case aload_2:
		case aload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aload, .insn_data.index = opcode - aload_0};
		case iaload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iaload};
		case laload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_laload};
		case faload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_faload};
		case daload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_daload};
		case aaload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aaload};
		case baload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_baload};
		case caload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_caload};
		case saload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_saload};
		case istore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_istore, .insn_data.index = reader_next_u8(reader, "istore index")
			};
		case lstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_lstore, .insn_data.index = reader_next_u8(reader, "lstore index")
			};
		case fstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_fstore, .insn_data.index = reader_next_u8(reader, "fstore index")
			};
		case dstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_dstore, .insn_data.index = reader_next_u8(reader, "dstore index")
			};
		case astore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_astore, .insn_data.index = reader_next_u8(reader, "astore index")
			};
		case istore_0:
		case istore_1:
		case istore_2:
		case istore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_istore, .insn_data.index = opcode - istore_0};
		case lstore_0:
		case lstore_1:
		case lstore_2:
		case lstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lstore, .insn_data.index = opcode - lstore_0};
		case fstore_0:
		case fstore_1:
		case fstore_2:
		case fstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fstore, .insn_data.index = opcode - fstore_0};
		case dstore_0:
		case dstore_1:
		case dstore_2:
		case dstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dstore, .insn_data.index = opcode - dstore_0};
		case astore_0:
		case astore_1:
		case astore_2:
		case astore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_astore, .insn_data.index = opcode - astore_0};
		case iastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iastore};
		case lastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lastore};
		case fastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fastore};
		case dastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dastore};
		case aastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aastore};
		case bastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_bastore};
		case castore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_castore};
		case sastore: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_sastore};
		case pop: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_pop};
		case pop2: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_pop2};
		case dup: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup};
		case dup_x1: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup_x1};
		case dup_x2: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup_x2};
		case dup2: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup2};
		case dup2_x1: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup2_x1};
		case dup2_x2: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dup2_x2};
		case swap: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_swap};
		case iadd: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iadd};
		case ladd: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ladd};
		case fadd: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fadd};
		case dadd: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dadd};
		case isub: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_isub};
		case lsub: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lsub};
		case fsub: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fsub};
		case dsub: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dsub};
		case imul: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_imul};
		case lmul: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lmul};
		case fmul: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fmul};
		case dmul: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dmul};
		case idiv: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_idiv};
		case ldiv: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ldiv};
		case fdiv: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fdiv};
		case ddiv: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ddiv};
		case irem: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_irem};
		case lrem: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lrem};
		case frem: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_frem};
		case drem: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_drem};
		case ineg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ineg};
		case lneg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lneg};
		case fneg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fneg};
		case dneg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dneg};
		case ishl: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ishl};
		case lshl: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lshl};
		case ishr: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ishr};
		case lshr: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lshr};
		case iushr: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iushr};
		case lushr: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lushr};
		case iand: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iand};
		case land: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_land};
		case ior: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ior};
		case lor: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lor};
		case ixor: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ixor};
		case lxor: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lxor};
		case iinc: {
			uint16_t index = reader_next_u8(reader, "iinc index");
			int16_t const_ = (int16_t)reader_next_i8(reader, "iinc const");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iinc, .insn_data.iinc = {index, const_}};
		}
		case i2l: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2l};
		case i2f: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2f};
		case i2d: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2d};
		case l2i: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_l2i};
		case l2f: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_l2f};
		case l2d: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_l2d};
		case f2i: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_f2i};
		case f2l: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_f2l};
		case f2d: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_f2d};
		case d2i: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_d2i};
		case d2l: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_d2l};
		case d2f: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_d2f};
		case i2b: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2b};
		case i2c: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2c};
		case i2s: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_i2s};
		case lcmp: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lcmp};
		case fcmpl: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fcmpl};
		case fcmpg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fcmpg};
		case dcmpl: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dcmpl};
		case dcmpg: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dcmpg};

		case ifeq: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifeq,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_eq offset"), ctx)
			};
		case ifne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifne,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_ne offset"), ctx)
			};
		case iflt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iflt,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_lt offset"), ctx)
			};
		case ifge: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifge,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_ge offset"), ctx)
			};
		case ifgt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifgt,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_gt offset"), ctx)
			};
		case ifle: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifle,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_le offset"), ctx)
			};

		case if_icmpeq: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpeq,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmpeq offset"), ctx)
			};
		case if_icmpne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpne,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmpne offset"), ctx)
			};
		case if_icmplt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmplt,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmplt offset"), ctx)
			};
		case if_icmpge: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpge,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmpge offset"), ctx)
			};
		case if_icmpgt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpgt,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmpgt offset"), ctx)
			};
		case if_icmple: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmple,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_icmple offset"), ctx)
			};
		case if_acmpeq: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_acmpeq,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_acmpeq offset"), ctx)
			};
		case if_acmpne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_acmpne,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "if_acmpne offset"), ctx)
			};

		case goto_: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_goto_,
				.insn_data.index = checked_pc(pc, reader_next_i16(reader, "goto offset"), ctx)
			};
		case jsr: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_jsr, .insn_data.index = checked_pc(pc, reader_next_i16(reader, "jsr offset"), ctx)
			};
		case ret: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ret, .insn_data.index = reader_next_u8(reader, "ret index")
			};
		case tableswitch: {
			return parse_tableswitch_insn(reader, pc + 1, ctx);
		}
		case lookupswitch: {
			return parse_lookupswitch_insn(reader, pc + 1, ctx);
		}
		case ireturn: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ireturn};
		case lreturn: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lreturn};
		case freturn: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_freturn};
		case dreturn: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dreturn};
		case areturn: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_areturn};
		case return_: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_return};

		case getstatic: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_getstatic, .insn_data.index = reader_next_u16(reader, "getstatic index")
			};
		case putstatic: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_putstatic, .insn_data.index = reader_next_u16(reader, "putstatic index")
			};

		case getfield: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_getfield, .insn_data.index = reader_next_u16(reader, "getfield index")
			};
		case putfield: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_putfield, .insn_data.index = reader_next_u16(reader, "putfield index")
			};

		case invokevirtual: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_invokevirtual, .insn_data.index = reader_next_u16(reader, "invokevirtual index")
			};
		case invokespecial: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_invokespecial, .insn_data.index = reader_next_u16(reader, "invokespecial index")
			};
		case invokestatic: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_invokestatic, .insn_data.index = reader_next_u16(reader, "invokestatic index")
			};

		case invokeinterface: {
			uint16_t index = reader_next_u16(reader, "invokeinterface index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_INTERFACE_METHOD_REF);
			reader_next_u8(reader, "invokeinterface count");
			reader_next_u8(reader, "invokeinterface zero");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_invokeinterface, .insn_data.cp = entry};
		}

		case invokedynamic: {
			uint16_t index = reader_next_u16(reader, "invokedynamic index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_INVOKE_DYNAMIC);
			reader_next_u16(reader, "invokedynamic zero");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_invokedynamic, .insn_data.cp = entry};
		}

		case new_: {
			uint16_t index = reader_next_u16(reader, "new index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS);
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_new, .insn_data.cp = entry};
		}

		case newarray: {
			uint8_t atype = reader_next_u8(reader, "newarray type");
			if (atype < BJVM_PRIMITIVE_BOOLEAN || atype > BJVM_PRIMITIVE_LONG) {
				char buf[64];
				snprintf(buf, sizeof(buf), "invalid newarray type %d", atype);
				verify_error_with_free(buf);
			}
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_newarray, .insn_data.array_type = atype};
		}

		case anewarray: {
			uint16_t index = reader_next_u16(reader, "anewarray index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS);
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_anewarray, .insn_data.cp = entry};
		}

		case arraylength: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_arraylength};
		case athrow: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_athrow};
		case checkcast: {
			uint16_t index = reader_next_u16(reader, "checkcast index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS);
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_checkcast, .insn_data.cp = entry};
		}

		case instanceof: {
			uint16_t index = reader_next_u16(reader, "instanceof index");
			bjvm_constant_pool_entry* entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS);
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_instanceof, .insn_data.cp = entry};
		}

		case monitorenter: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_monitorenter};
		case monitorexit: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_monitorexit};

		case wide: {
			switch (reader_next_u8(reader, "widened opcode")) {
				case iload: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iload, .insn_data.index = reader_next_u16(reader, "wide iload index")};
				}
				case lload: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lload, .insn_data.index = reader_next_u16(reader, "wide lload index")};
				}
				case fload: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fload, .insn_data.index = reader_next_u16(reader, "wide fload index")};
				}
				case dload: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dload, .insn_data.index = reader_next_u16(reader, "wide dload index")};
				}
				case aload: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aload, .insn_data.index = reader_next_u16(reader, "wide aload index")};
				}
				case istore: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_istore, .insn_data.index = reader_next_u16(reader, "wide istore index")};
				}
				case lstore: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lstore, .insn_data.index = reader_next_u16(reader, "wide lstore index")};
				}
				case fstore: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fstore, .insn_data.index = reader_next_u16(reader, "wide fstore index")};
				}
				case dstore: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dstore, .insn_data.index = reader_next_u16(reader, "wide dstore index")};
				}
				case astore: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_astore, .insn_data.index = reader_next_u16(reader, "wide astore index")};
				}
				case iinc: {
					uint16_t index = reader_next_u16(reader, "wide iinc index");
					int16_t const_ = reader_next_i16(reader, "wide iinc const");
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iinc, .insn_data.iinc = {index, const_}};
				}
				case ret: {
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_ret, .insn_data.index = reader_next_u16(reader, "wide ret index")};
				}

				default: {
					char buf[64];
					snprintf(buf, sizeof(buf), "invalid wide opcode %d", opcode);
					verify_error_with_free(buf);
				}
			}
		}

		default: {
			char buf[64];
			snprintf(buf, sizeof(buf), "invalid opcode %d", opcode);
			verify_error_with_free(buf);
		}
	}
}

/**
 * Parse an instruction at the given program counter and advance the reader.
 * @return The parsed instruction.
 */
bjvm_bytecode_insn parse_insn(cf_byteslice *reader, int pc, bjvm_classfile_parse_ctx *ctx) {
	bjvm_bytecode_insn insn = parse_insn_impl(reader, pc, ctx);
	insn.program_counter = pc;
	return insn;
}

bjvm_cp_method* parse_method(cf_byteslice * reader, bjvm_classfile_parse_ctx* ctx) {
	bjvm_cp_method* method = malloc(sizeof(bjvm_cp_method*));

	method->access_flags = reader_next_u16(reader, "method access flags");
	method->name = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "method name"));
	method->descriptor = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "method descriptor"));
	method->attributes_count = reader_next_u16(reader, "method attributes count");

	for (int i = 0; i < method->attributes_count; i++) {
		bjvm_attribute* attr = malloc(sizeof(bjvm_attribute));
		needs_free_on_verify_error(ctx, attr);

		attr->name = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "method attribute name"));
		attr->length = reader_next_u32(reader, "method attribute length");

		cf_byteslice attr_reader = reader_get_slice(reader, attr->length, "Attribute data");
		if (compare_utf8_entry(attr->name, "Code")) {
			printf("Reached!\n");
		}
	}
}

/**
 * Parse a Java class file.
 * @param bytes Start byte of the classfile.
 * @param len Length of the classfile in bytes.
 * @param result Where to write the result.
 * @return NULL on success, otherwise an error message (which is the caller's responsibility to free).
 */
char* parse_classfile(uint8_t* bytes, size_t len, bjvm_classfile** result) {
	cf_byteslice reader { .bytes = bytes, .len = len };
	bjvm_classfile *cf = *result = malloc(sizeof(bjvm_classfile));
	bjvm_classfile_parse_ctx ctx {
		.free_on_error = NULL,
		.free_on_error_count = 0,
		.free_on_error_cap = 0,
		.cp = NULL
	};
	needs_free_on_verify_error(&ctx, cf);

	if (setjmp(verify_error_jmp_buf) != 0) {
		for (int i = 0; i < ctx.free_on_error_count; i++) {
			free(ctx.free_on_error[i]);
		}
		return verify_error_needs_free ? verify_error_msg : strdup(verify_error_msg);
	}

	const uint32_t magic = reader_next_u32(&reader, "magic");
	if (magic != 0xCAFEBABE) {
		char buf[64];
		snprintf(buf, sizeof(buf), "invalid magic number 0x%08x", magic);
		verify_error_with_free(buf);
	}

	cf->minor_version = reader_next_u16(&reader, "minor version");
	cf->major_version = reader_next_u16(&reader, "major version");

	cf->pool = parse_constant_pool(&reader, &ctx);

	cf->access_flags = reader_next_u16(&reader, "access flags");
	cf->this_class = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "this class"), BJVM_CP_KIND_CLASS)->data.class_info;
	cf->super_class = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "super class"), BJVM_CP_KIND_CLASS)->data.class_info;

	cf->interfaces_count = reader_next_u16(&reader, "interfaces count");
	cf->interfaces = malloc(cf->interfaces_count * sizeof(bjvm_cp_class_info*));

	needs_free_on_verify_error(&ctx, cf->interfaces);

	for (int i = 0; i < cf->interfaces_count; i++) {
		cf->interfaces[i] = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "interface"), BJVM_CP_KIND_CLASS)->data.class_info;
	}

	cf->fields_count = reader_next_u16(&reader, "fields count");
	cf->fields = malloc(cf->fields_count * sizeof(bjvm_cp_fieldref_info*));

	needs_free_on_verify_error(&ctx, cf->fields);

	for (int i = 0; i < cf->fields_count; i++) {
		cf->fields[i] = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "field"), BJVM_CP_KIND_FIELD_REF)->data.fieldref_info;
	}

	cf->methods_count = reader_next_u16(&reader, "methods count");
	cf->methods = malloc(cf->methods_count * sizeof(bjvm_cp_method*));

	needs_free_on_verify_error(&ctx, cf->methods);

	for (int i = 0; i < cf->methods_count; ++i) {
		cf->methods[i] = parse_method(&reader, &ctx);
	}

	free(ctx.free_on_error); // we made it :)
	*result = cf;

	return NULL;
}