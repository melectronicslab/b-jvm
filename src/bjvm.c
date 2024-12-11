#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <stdbool.h>

#if defined(__APPLE__)
// Mac OS X / Darwin features
#include <libkern/OSByteOrder.h>
#define __bswap_16(x) OSSwapInt16(x)
#define __bswap_32(x) OSSwapInt32(x)
#define __bswap_64(x) OSSwapInt64(x)
#else
#include <byteswap.h>
#endif

#include "bjvm.h"

#define BJVM_UNREACHABLE() do { fprintf(stderr, "Unreachable code reached at %s:%d\n", __FILE__, __LINE__); abort(); } while (0)
#define BJVM_DCHECK(expr) do { if (!(expr)) { fprintf(stderr, "Assertion failed: %s at %s:%d\n", #expr, __FILE__, __LINE__); abort(); } } while (0)

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

bool compare_utf8_entry(bjvm_cp_utf8_entry *entry, const char *str) {
	if (entry->len != (int)strlen(str))
		return false;
	for (int i = 0; i < entry->len; ++i)
		if (entry->chars[i] != str[i])
			return false;
	return true;
}

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

void bjvm_free_constant_pool_entry(bjvm_constant_pool_entry *entry) {
	switch (entry->kind) {
		case BJVM_CP_KIND_UTF8:
			bjvm_free_utf8_entry(&entry->data.utf8);
			break;
		default: // TODO will need to add more as we resolve descriptors
			break;
	}
}

void free_method(bjvm_cp_method *method);
void free_field(bjvm_cp_field *field);

void bjvm_free_constant_pool(bjvm_constant_pool *pool) {
	for (int i = 0; i < pool->entries_len; ++i) {
		bjvm_free_constant_pool_entry(&pool->entries[i]);
	}
	free(pool);
}

void bjvm_free_classfile(bjvm_classfile *cf) {
	bjvm_free_constant_pool(cf->pool);
	free(cf->interfaces);
	for (int i = 0; i < cf->attributes_count; ++i) {
		bjvm_free_attribute(&cf->attributes[i]);
	}
	for (int i = 0; i < cf->methods_count; ++i) {
		free_method(&cf->methods[i]);
	}
	for (int i = 0; i < cf->fields_count; ++i) {
		free_field(&cf->fields[i]);
	}
	free(cf->fields);
	free(cf->methods);
	free(cf->attributes);
	free(cf);
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

void free_lookupswitch_data(struct bjvm_bc_lookupswitch_data *data) {
	free(data->keys);
}

void free_code_attribute(bjvm_attribute_code *code) {
	free(code->code);
}

void bjvm_free_attribute(bjvm_attribute *attribute) {
	switch (attribute->kind) {
		case BJVM_ATTRIBUTE_KIND_CODE:
			free_code_attribute(&attribute->code);
			break;
		case BJVM_ATTRIBUTE_KIND_CONSTANT_VALUE:
		case BJVM_ATTRIBUTE_KIND_UNKNOWN:
			break;
	}
}

void free_field(bjvm_cp_field* field) {
	for (int i = 0; i < field->attributes_count; ++i) {
		bjvm_free_attribute(&field->attributes[i]);
	}
	free(field->attributes);
}

void free_method(bjvm_cp_method *method) {
	for (int i = 0; i < method->attributes_count; ++i) {
		bjvm_free_attribute(&method->attributes[i]);
	}
	free(method->attributes);
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
		reader->bytes += sizeof(type); \
		reader->len -= sizeof(type); \
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

#define VECTOR_PUSH(vector, vector_count, vector_cap) \
	({ \
		if (vector_count >= vector_cap) { \
			int new_cap;   \
			int overflow = __builtin_mul_overflow(vector_cap, 2, &new_cap); \
			BJVM_DCHECK(!overflow); \
			if (new_cap < 2) new_cap = 2; \
			void* next = realloc(vector, new_cap * sizeof(*vector)); \
			if (!next) { \
				fprintf(stderr, "Out of memory\n"); \
				abort(); \
			} \
			vector_cap = new_cap; \
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
 * Used to unmark an otherwise to-be-freed pointer in the verify context.
 */
typedef struct {
	bjvm_classfile_parse_ctx *ctx;
	size_t offset;
} ctx_free_ticket;

void free_ticket(ctx_free_ticket ticket) {
	ticket.ctx->free_on_error[ticket.offset] = NULL;
}

/**
 * Record that this pointer needs to be freed if we encounter a VerifyError while parsing the classfile.
 */
ctx_free_ticket needs_free_on_verify_error(bjvm_classfile_parse_ctx *ctx, void *ptr) {
	*VECTOR_PUSH(ctx->free_on_error, ctx->free_on_error_count, ctx->free_on_error_cap) = ptr;
	return (ctx_free_ticket) {
		.ctx = ctx,
		.offset = ctx->free_on_error_count - 1
	};
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

bjvm_constant_pool_entry *checked_get_constant_pool_entry(bjvm_constant_pool *pool, int index, int expected_kinds, const char* reason) {
	if (!(index >= 0 && index < pool->entries_len)) {
		char buf[256] = {0};
		snprintf(buf, sizeof(buf), "Invalid constant pool entry index %d (pool size %d) %s %s", index, pool->entries_len, reason ? "while reading" : "", reason ? reason : "");
		verify_error_with_free(strdup(buf));
	}

	bjvm_constant_pool_entry *entry = pool->entries + index;
	if (entry->kind & expected_kinds)
		return entry;

	char buf[1000] = {0};
	char *write = buf + sprintf(buf, "Unexpected constant pool entry kind %d at index %d (expected one of: [ ",
	                            entry->kind, index);
	for (int i = 0; i < 14; ++i) {
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

bjvm_cp_utf8_entry *checked_get_utf8_entry(bjvm_constant_pool *pool, int index, const char *reason) {
	return &checked_get_constant_pool_entry(pool, index, BJVM_CP_KIND_UTF8, reason)->data.utf8;
}

char* lossy_utf8_entry_to_chars(const bjvm_cp_utf8_entry* utf8);

/**
 * Parse a single constant pool entry.
 * @param reader The reader to parse from.
 * @param ctx The parse context.
 * @param suppress_resolution If true, do not resolve anything -- just what the kind is.
 * @return The resolved entry.
 */
bjvm_constant_pool_entry parse_constant_pool_entry(cf_byteslice *reader, bjvm_classfile_parse_ctx *ctx,
                                                   bool suppress_resolution) {
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
					.name = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, index, "class info name")
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
			bjvm_cp_class_info *class_info = suppress_resolution
				                                 ? NULL
				                                 : &checked_get_constant_pool_entry(
					                                 ctx->cp, class_index, BJVM_CP_KIND_CLASS, "fieldref/methodref/interfacemethodref class info")->data.
				                                 class_info;

			bjvm_cp_name_and_type *name_and_type = suppress_resolution
				                                       ? NULL
				                                       : &checked_get_constant_pool_entry(
					                                       ctx->cp, name_and_type_index,
					                                       BJVM_CP_KIND_NAME_AND_TYPE, "fieldref/methodref/interfacemethodref name and type")->data.name_and_type;

			if (kind == CONSTANT_Fieldref) {
				return (bjvm_constant_pool_entry){
					.kind = entry_kind,
					.data.fieldref_info = {
						.class_info = class_info,
						.name_and_type = name_and_type
					}
				};
			}
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
					.chars = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, index, "string value")
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

			bjvm_cp_utf8_entry* name = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, name_index, "name and type name");

			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_NAME_AND_TYPE,
				.data.name_and_type = {
					.name = name,
					.descriptor = suppress_resolution ? NULL : checked_get_utf8_entry(ctx->cp, descriptor_index, "name and type descriptor")
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
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_METHOD_HANDLE,
				.data.method_handle_info = {
					.handle_kind = reader_next_u8(reader, "method handle kind"),
					.reference_index = reader_next_u16(reader, "reference index")
				}
			};
		}
		case CONSTANT_MethodType: {
			uint16_t desc_index = reader_next_u16(reader, "descriptor index");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_METHOD_TYPE,
				.data.method_type_info = {
					.descriptor = suppress_resolution
						              ? NULL
						              : checked_get_utf8_entry(ctx->cp, desc_index, "method type descriptor")
				}
			};
		}
		case CONSTANT_InvokeDynamic: {
			uint16_t bootstrap_method_attr_index = reader_next_u16(reader, "bootstrap method attr index");
			uint16_t name_and_type_index = reader_next_u16(reader, "name and type index");
			return (bjvm_constant_pool_entry){
				.kind = BJVM_CP_KIND_INVOKE_DYNAMIC,
				.data.invoke_dynamic_info = {
					.bootstrap_method_attr_index = bootstrap_method_attr_index,
					.name_and_type = suppress_resolution
						                 ? NULL
						                 : &checked_get_constant_pool_entry(
							                 ctx->cp, name_and_type_index,
							                 BJVM_CP_KIND_NAME_AND_TYPE, "indy name and type")->data.name_and_type
				}
			};
		}
		default:
			verify_error("Invalid constant pool entry kind");
	}
}

bjvm_constant_pool *init_constant_pool(uint16_t count) {
	bjvm_constant_pool *pool = calloc(1, sizeof(bjvm_constant_pool) + (count + 1) * sizeof(bjvm_constant_pool_entry));
	pool->entries_len = count + 1;
	return pool;
}

bool is_entry_wide(bjvm_constant_pool_entry *ent) {
	return ent->kind == BJVM_CP_KIND_DOUBLE || ent->kind == BJVM_CP_KIND_LONG;
}

bjvm_constant_pool *parse_constant_pool(cf_byteslice *reader, bjvm_classfile_parse_ctx *ctx) {
	uint16_t count = reader_next_u16(reader, "constant pool count");

	bjvm_constant_pool *pool = init_constant_pool(count);
	ctx->cp = pool;
	needs_free_on_verify_error(ctx, pool);

	get_constant_pool_entry(pool, 0)->kind = BJVM_CP_KIND_INVALID; // entry at 0 is always invalid
	cf_byteslice initial_reader_state = *reader;
	for (int i = 0; i < 2; ++i) {
		for (int cp_i = 1; cp_i < count; ++cp_i) {
			// Read first, then link
			*get_constant_pool_entry(pool, cp_i) = parse_constant_pool_entry(reader, ctx, !(bool) i);
			if (is_entry_wide(get_constant_pool_entry(pool, cp_i))) {
				get_constant_pool_entry(pool, cp_i + 1)->kind = BJVM_CP_KIND_INVALID;
				cp_i++;
			}
		}
		if (i == 0)
			*reader = initial_reader_state;
	}

	return pool;
}

int checked_pc(uint32_t insn_pc, int offset, bjvm_classfile_parse_ctx *ctx) {
	int target;
	int overflow = __builtin_add_overflow(insn_pc, offset, &target);
	if (overflow || target < 0 || target >= ctx->current_code_max_pc) {
		verify_error("Branch target out of bounds");
	}
	return target;
}

bjvm_bytecode_insn parse_tableswitch_insn(cf_byteslice *reader, int pc, bjvm_classfile_parse_ctx *ctx) {
	int original_pc = pc - 1;

	// consume u8s until pc = 0 mod 4
	while (pc % 4 != 0) {
		reader_next_u8(reader, "tableswitch padding");
		pc++;
	}

	int default_target = checked_pc(original_pc, reader_next_i32(reader, "tableswitch default target"), ctx);
	int low = reader_next_i32(reader, "tableswitch low");
	int high = reader_next_i32(reader, "tableswitch high");
	long targets_count = (long)high - low + 1;

	if (targets_count > 1 << 15) { // preposterous, won't fit in the code segment
		verify_error("tableswitch instruction is too large");
	}
	if (targets_count <= 0) {
		verify_error("tableswitch high < low");
	}

	int *targets = malloc(targets_count * sizeof(int));
	for (int i = 0; i < targets_count; ++i) {
		targets[i] = checked_pc(original_pc, reader_next_i32(reader, "tableswitch target"), ctx);
	}

	return (bjvm_bytecode_insn){
		.kind = bjvm_bc_insn_tableswitch,
		.program_counter = original_pc,
		.tableswitch = {
			.default_target = default_target,
			.low = low,
			.high = high,
			.targets = targets,
			.targets_count = targets_count
		}
	};
}

bjvm_bytecode_insn parse_lookupswitch_insn(cf_byteslice *reader, int pc, bjvm_classfile_parse_ctx *ctx) {
	int original_pc = pc - 1;

	while (pc % 4 != 0) {
		reader_next_u8(reader, "tableswitch padding");
		pc++;
	}

	int default_target = checked_pc(original_pc, reader_next_i32(reader, "lookupswitch default target"), ctx);
	int pairs_count = reader_next_i32(reader, "lookupswitch pairs count");

	int *keys = malloc(pairs_count * sizeof(int));
	int *targets = malloc(pairs_count * sizeof(int));
	needs_free_on_verify_error(ctx, keys);
	needs_free_on_verify_error(ctx, targets);

	for (int i = 0; i < pairs_count; ++i) {
		keys[i] = reader_next_i32(reader, "lookupswitch key");
		targets[i] = checked_pc(original_pc, reader_next_i32(reader, "lookupswitch target"), ctx);
	}

	return (bjvm_bytecode_insn){
		.kind = bjvm_bc_insn_lookupswitch,
		.program_counter = original_pc,
		.lookupswitch = {
			.default_target = default_target,
			.keys = keys,
			.keys_count = pairs_count,
			.targets = targets,
			.targets_count = pairs_count
		}
	};
}

const char *insn_code_name(bjvm_insn_code_kind code) {
	switch (code) {
		case bjvm_bc_insn_aaload: return "aaload";
		case bjvm_bc_insn_aastore: return "aastore";
		case bjvm_bc_insn_aconst_null: return "aconst_null";
		case bjvm_bc_insn_areturn: return "areturn";
		case bjvm_bc_insn_arraylength: return "arraylength";
		case bjvm_bc_insn_athrow: return "athrow";
		case bjvm_bc_insn_baload: return "baload";
		case bjvm_bc_insn_bastore: return "bastore";
		case bjvm_bc_insn_caload: return "caload";
		case bjvm_bc_insn_castore: return "castore";
		case bjvm_bc_insn_d2f: return "d2f";
		case bjvm_bc_insn_d2i: return "d2i";
		case bjvm_bc_insn_d2l: return "d2l";
		case bjvm_bc_insn_dadd: return "dadd";
		case bjvm_bc_insn_daload: return "daload";
		case bjvm_bc_insn_dastore: return "dastore";
		case bjvm_bc_insn_dcmpg: return "dcmpg";
		case bjvm_bc_insn_dcmpl: return "dcmpl";
		case bjvm_bc_insn_ddiv: return "ddiv";
		case bjvm_bc_insn_dmul: return "dmul";
		case bjvm_bc_insn_dneg: return "dneg";
		case bjvm_bc_insn_drem: return "drem";
		case bjvm_bc_insn_dreturn: return "dreturn";
		case bjvm_bc_insn_dsub: return "dsub";
		case bjvm_bc_insn_dup: return "dup";
		case bjvm_bc_insn_dup_x1: return "dup_x1";
		case bjvm_bc_insn_dup_x2: return "dup_x2";
		case bjvm_bc_insn_dup2: return "dup2";
		case bjvm_bc_insn_dup2_x1: return "dup2_x1";
		case bjvm_bc_insn_dup2_x2: return "dup2_x2";
		case bjvm_bc_insn_f2d: return "f2d";
		case bjvm_bc_insn_f2i: return "f2i";
		case bjvm_bc_insn_f2l: return "f2l";
		case bjvm_bc_insn_fadd: return "fadd";
		case bjvm_bc_insn_faload: return "faload";
		case bjvm_bc_insn_fastore: return "fastore";
		case bjvm_bc_insn_fcmpg: return "fcmpg";
		case bjvm_bc_insn_fcmpl: return "fcmpl";
		case bjvm_bc_insn_fdiv: return "fdiv";
		case bjvm_bc_insn_fmul: return "fmul";
		case bjvm_bc_insn_fneg: return "fneg";
		case bjvm_bc_insn_frem: return "frem";
		case bjvm_bc_insn_freturn: return "freturn";
		case bjvm_bc_insn_fsub: return "fsub";
		case bjvm_bc_insn_i2b: return "i2b";
		case bjvm_bc_insn_i2c: return "i2c";
		case bjvm_bc_insn_i2d: return "i2d";
		case bjvm_bc_insn_i2f: return "i2f";
		case bjvm_bc_insn_i2l: return "i2l";
		case bjvm_bc_insn_i2s: return "i2s";
		case bjvm_bc_insn_iadd: return "iadd";
		case bjvm_bc_insn_iaload: return "iaload";
		case bjvm_bc_insn_iand: return "iand";
		case bjvm_bc_insn_iastore: return "iastore";
		case bjvm_bc_insn_idiv: return "idiv";
		case bjvm_bc_insn_imul: return "imul";
		case bjvm_bc_insn_ineg: return "ineg";
		case bjvm_bc_insn_ior: return "ior";
		case bjvm_bc_insn_irem: return "irem";
		case bjvm_bc_insn_ireturn: return "ireturn";
		case bjvm_bc_insn_ishl: return "ishl";
		case bjvm_bc_insn_ishr: return "ishr";
		case bjvm_bc_insn_isub: return "isub";
		case bjvm_bc_insn_iushr: return "iushr";
		case bjvm_bc_insn_ixor: return "ixor";
		case bjvm_bc_insn_l2d: return "l2d";
		case bjvm_bc_insn_l2f: return "l2f";
		case bjvm_bc_insn_l2i: return "l2i";
		case bjvm_bc_insn_ladd: return "ladd";
		case bjvm_bc_insn_laload: return "laload";
		case bjvm_bc_insn_land: return "land";
		case bjvm_bc_insn_lastore: return "lastore";
		case bjvm_bc_insn_lcmp: return "lcmp";
		case bjvm_bc_insn_ldc: return "ldc";
		case bjvm_bc_insn_ldc2_w: return "ldc2_w";
		case bjvm_bc_insn_ldiv: return "ldiv";
		case bjvm_bc_insn_lmul: return "lmul";
		case bjvm_bc_insn_lneg: return "lneg";
		case bjvm_bc_insn_lor: return "lor";
		case bjvm_bc_insn_lrem: return "lrem";
		case bjvm_bc_insn_lreturn: return "lreturn";
		case bjvm_bc_insn_lshl: return "lshl";
		case bjvm_bc_insn_lshr: return "lshr";
		case bjvm_bc_insn_lsub: return "lsub";
		case bjvm_bc_insn_lushr: return "lushr";
		case bjvm_bc_insn_lxor: return "lxor";
		case bjvm_bc_insn_monitorenter: return "monitorenter";
		case bjvm_bc_insn_monitorexit: return "monitorexit";
		case bjvm_bc_insn_nop: return "nop";
		case bjvm_bc_insn_pop: return "pop";
		case bjvm_bc_insn_pop2: return "pop2";
		case bjvm_bc_insn_return: return "return_";
		case bjvm_bc_insn_saload: return "saload";
		case bjvm_bc_insn_sastore: return "sastore";
		case bjvm_bc_insn_swap: return "swap";
		case bjvm_bc_insn_dload: return "dload";
		case bjvm_bc_insn_fload: return "fload";
		case bjvm_bc_insn_iload: return "iload";
		case bjvm_bc_insn_lload: return "lload";
		case bjvm_bc_insn_dstore: return "dstore";
		case bjvm_bc_insn_fstore: return "fstore";
		case bjvm_bc_insn_istore: return "istore";
		case bjvm_bc_insn_lstore: return "lstore";
		case bjvm_bc_insn_aload: return "aload";
		case bjvm_bc_insn_astore: return "astore";
		case bjvm_bc_insn_anewarray: return "anewarray";
		case bjvm_bc_insn_checkcast: return "checkcast";
		case bjvm_bc_insn_getfield: return "getfield";
		case bjvm_bc_insn_getstatic: return "getstatic";
		case bjvm_bc_insn_instanceof: return "instanceof";
		case bjvm_bc_insn_invokedynamic: return "invokedynamic";
		case bjvm_bc_insn_new: return "new";
		case bjvm_bc_insn_putfield: return "putfield";
		case bjvm_bc_insn_putstatic: return "putstatic";
		case bjvm_bc_insn_invokevirtual: return "invokevirtual";
		case bjvm_bc_insn_invokespecial: return "invokespecial";
		case bjvm_bc_insn_invokestatic: return "invokestatic";
		case bjvm_bc_insn_goto: return "goto";
		case bjvm_bc_insn_jsr: return "jsr";
		case bjvm_bc_insn_ret: return "ret";
		case bjvm_bc_insn_if_acmpeq: return "if_acmpeq";
		case bjvm_bc_insn_if_acmpne: return "if_acmpne";
		case bjvm_bc_insn_if_icmpeq: return "if_icmpeq";
		case bjvm_bc_insn_if_icmpne: return "if_icmpne";
		case bjvm_bc_insn_if_icmplt: return "if_icmplt";
		case bjvm_bc_insn_if_icmpge: return "if_icmpge";
		case bjvm_bc_insn_if_icmpgt: return "if_icmpgt";
		case bjvm_bc_insn_if_icmple: return "if_icmple";
		case bjvm_bc_insn_ifeq: return "ifeq";
		case bjvm_bc_insn_ifne: return "ifne";
		case bjvm_bc_insn_iflt: return "iflt";
		case bjvm_bc_insn_ifge: return "ifge";
		case bjvm_bc_insn_ifgt: return "ifgt";
		case bjvm_bc_insn_ifle: return "ifle";
		case bjvm_bc_insn_ifnonnull: return "ifnonnull";
		case bjvm_bc_insn_ifnull: return "ifnull";
		case bjvm_bc_insn_iconst: return "iconst";
		case bjvm_bc_insn_dconst: return "dconst";
		case bjvm_bc_insn_fconst: return "fconst";
		case bjvm_bc_insn_lconst: return "lconst";
		case bjvm_bc_insn_iinc: return "iinc";
		case bjvm_bc_insn_invokeinterface: return "invokeinterface";
		case bjvm_bc_insn_multianewarray: return "multianewarray";
		case bjvm_bc_insn_newarray: return "newarray";
		case bjvm_bc_insn_tableswitch: return "tableswitch";
		case bjvm_bc_insn_lookupswitch: return "lookupswitch";
	}
}

bjvm_bytecode_insn parse_insn_impl(cf_byteslice *reader, uint32_t pc, bjvm_classfile_parse_ctx *ctx) {
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
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iconst, .integer_imm = opcode - iconst_0};
		case lconst_0:
		case lconst_1:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lconst, .integer_imm = opcode - lconst_0};
		case fconst_0:
		case fconst_1:
		case fconst_2:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fconst, .f_imm = (float) (opcode - fconst_0)};
		case dconst_0:
		case dconst_1:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dconst, .d_imm = (double) (opcode - dconst_0)};
		case bipush:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iconst, .integer_imm = reader_next_i8(reader, "bipush immediate")
			};
		case sipush:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iconst, .integer_imm = reader_next_i16(reader, "sipush immediate")
			};
		case ldc:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u8(reader, "ldc index"), BJVM_CP_KIND_INTEGER | BJVM_CP_KIND_FLOAT | BJVM_CP_KIND_STRING | BJVM_CP_KIND_CLASS, "ldc index")
			};
		case ldc_w:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "ldc_w index"), BJVM_CP_KIND_INTEGER | BJVM_CP_KIND_FLOAT | BJVM_CP_KIND_STRING | BJVM_CP_KIND_CLASS, "ldc_w index")
			};
		case ldc2_w:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ldc2_w,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "ldc2_w index"), BJVM_CP_KIND_DOUBLE | BJVM_CP_KIND_LONG, "ldc2_w index")
			};
		case iload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iload, .index = reader_next_u8(reader, "iload index")
			};
		case lload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_lload, .index = reader_next_u8(reader, "lload index")
			};
		case fload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_fload, .index = reader_next_u8(reader, "fload index")
			};
		case dload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_dload, .index = reader_next_u8(reader, "dload index")
			};
		case aload:
			return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_aload, .index = reader_next_u8(reader, "aload index")
			};
		case iload_0:
		case iload_1:
		case iload_2:
		case iload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iload, .index = opcode - iload_0};
		case lload_0:
		case lload_1:
		case lload_2:
		case lload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lload, .index = opcode - lload_0};
		case fload_0:
		case fload_1:
		case fload_2:
		case fload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fload, .index = opcode - fload_0};
		case dload_0:
		case dload_1:
		case dload_2:
		case dload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dload, .index = opcode - dload_0};
		case aload_0:
		case aload_1:
		case aload_2:
		case aload_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aload, .index = opcode - aload_0};
		case iaload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iaload};
		case laload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_laload};
		case faload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_faload};
		case daload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_daload};
		case aaload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_aaload};
		case baload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_baload};
		case caload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_caload};
		case saload: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_saload};
		case istore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_istore, .index = reader_next_u8(reader, "istore index")
			};
		case lstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_lstore, .index = reader_next_u8(reader, "lstore index")
			};
		case fstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_fstore, .index = reader_next_u8(reader, "fstore index")
			};
		case dstore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_dstore, .index = reader_next_u8(reader, "dstore index")
			};
		case astore: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_astore, .index = reader_next_u8(reader, "astore index")
			};
		case istore_0:
		case istore_1:
		case istore_2:
		case istore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_istore, .index = opcode - istore_0};
		case lstore_0:
		case lstore_1:
		case lstore_2:
		case lstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_lstore, .index = opcode - lstore_0};
		case fstore_0:
		case fstore_1:
		case fstore_2:
		case fstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_fstore, .index = opcode - fstore_0};
		case dstore_0:
		case dstore_1:
		case dstore_2:
		case dstore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_dstore, .index = opcode - dstore_0};
		case astore_0:
		case astore_1:
		case astore_2:
		case astore_3:
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_astore, .index = opcode - astore_0};
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
			int16_t const_ = (int16_t) reader_next_i8(reader, "iinc const");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iinc, .iinc = {index, const_}};
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
				.index = checked_pc(pc, reader_next_i16(reader, "if_eq offset"), ctx)
			};
		case ifne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifne,
				.index = checked_pc(pc, reader_next_i16(reader, "if_ne offset"), ctx)
			};
		case iflt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_iflt,
				.index = checked_pc(pc, reader_next_i16(reader, "if_lt offset"), ctx)
			};
		case ifge: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifge,
				.index = checked_pc(pc, reader_next_i16(reader, "if_ge offset"), ctx)
			};
		case ifgt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifgt,
				.index = checked_pc(pc, reader_next_i16(reader, "if_gt offset"), ctx)
			};
		case ifle: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifle,
				.index = checked_pc(pc, reader_next_i16(reader, "if_le offset"), ctx)
			};

		case if_icmpeq: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpeq,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmpeq offset"), ctx)
			};
		case if_icmpne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpne,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmpne offset"), ctx)
			};
		case if_icmplt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmplt,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmplt offset"), ctx)
			};
		case if_icmpge: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpge,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmpge offset"), ctx)
			};
		case if_icmpgt: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmpgt,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmpgt offset"), ctx)
			};
		case if_icmple: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_icmple,
				.index = checked_pc(pc, reader_next_i16(reader, "if_icmple offset"), ctx)
			};
		case if_acmpeq: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_acmpeq,
				.index = checked_pc(pc, reader_next_i16(reader, "if_acmpeq offset"), ctx)
			};
		case if_acmpne: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_if_acmpne,
				.index = checked_pc(pc, reader_next_i16(reader, "if_acmpne offset"), ctx)
			};

		case goto_: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_goto,
				.index = checked_pc(pc, reader_next_i16(reader, "goto offset"), ctx)
			};
		case jsr: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_jsr, .index = checked_pc(pc, reader_next_i16(reader, "jsr offset"), ctx)
			};
		case ret: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ret, .index = reader_next_u8(reader, "ret index")
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
				.kind = bjvm_bc_insn_getstatic,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "getstatic index"), BJVM_CP_KIND_FIELD_REF, "getstatic field ref")
			};
		case putstatic: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_putstatic,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "putstatic index"), BJVM_CP_KIND_FIELD_REF, "putstatic field ref")
			};

		case getfield: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_getfield,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "getfield index"), BJVM_CP_KIND_FIELD_REF, "getfield field ref")
			};
		case putfield: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_putfield,
				.cp = checked_get_constant_pool_entry(ctx->cp, reader_next_u16(reader, "putfield index"), BJVM_CP_KIND_FIELD_REF, "putfield field ref")
			};

		case invokevirtual: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_invokevirtual,
				.cp = checked_get_constant_pool_entry(ctx->cp,
					reader_next_u16(reader, "invokevirtual index"),
					BJVM_CP_KIND_METHOD_REF | BJVM_CP_KIND_INTERFACE_METHOD_REF,
					"invokevirtual method ref")
			};
		case invokespecial: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_invokespecial,
			.cp = checked_get_constant_pool_entry(ctx->cp,
					reader_next_u16(reader, "invokespecial index"),
					BJVM_CP_KIND_METHOD_REF | BJVM_CP_KIND_INTERFACE_METHOD_REF,
					"invokespecial method ref")
			};
		case invokestatic: return (bjvm_bytecode_insn){
			.kind = bjvm_bc_insn_invokestatic, .cp = checked_get_constant_pool_entry(ctx->cp,
				reader_next_u16(reader, "invokestatic index"),
				BJVM_CP_KIND_METHOD_REF | BJVM_CP_KIND_INTERFACE_METHOD_REF,
				"invokestatic method ref")
			};

		case invokeinterface: {
			uint16_t index = reader_next_u16(reader, "invokeinterface index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(
				ctx->cp, index, BJVM_CP_KIND_INTERFACE_METHOD_REF, "invokeinterface method ref");
			reader_next_u8(reader, "invokeinterface count");
			reader_next_u8(reader, "invokeinterface zero");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_invokeinterface, .cp = entry};
		}

		case invokedynamic: {
			uint16_t index = reader_next_u16(reader, "invokedynamic index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(
				ctx->cp, index, BJVM_CP_KIND_INVOKE_DYNAMIC, "indy method ref");
			reader_next_u16(reader, "invokedynamic zero");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_invokedynamic, .cp = entry};
		}

		case new_: {
			uint16_t index = reader_next_u16(reader, "new index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS, "new class");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_new, .cp = entry};
		}

		case newarray: {
			uint8_t atype = reader_next_u8(reader, "newarray type");
			if (atype < BJVM_PRIMITIVE_BOOLEAN || atype > BJVM_PRIMITIVE_LONG) {
				char buf[64];
				snprintf(buf, sizeof(buf), "invalid newarray type %d", atype);
				verify_error_with_free(strdup(buf));
			}
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_newarray, .array_type = atype};
		}

		case anewarray: {
			uint16_t index = reader_next_u16(reader, "anewarray index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS, "anewarray class");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_anewarray, .cp = entry};
		}

		case arraylength: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_arraylength};
		case athrow: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_athrow};
		case checkcast: {
			uint16_t index = reader_next_u16(reader, "checkcast index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS, "checkcast class");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_checkcast, .cp = entry};
		}

		case instanceof: {
			uint16_t index = reader_next_u16(reader, "instanceof index");
			bjvm_constant_pool_entry *entry = checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS, "instanceof class");
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_instanceof, .cp = entry};
		}

		case monitorenter: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_monitorenter};
		case monitorexit: return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_monitorexit};

		case wide: {
			switch (reader_next_u8(reader, "widened opcode")) {
				case iload: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_iload, .index = reader_next_u16(reader, "wide iload index")
					};
				}
				case lload: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_lload, .index = reader_next_u16(reader, "wide lload index")
					};
				}
				case fload: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_fload, .index = reader_next_u16(reader, "wide fload index")
					};
				}
				case dload: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_dload, .index = reader_next_u16(reader, "wide dload index")
					};
				}
				case aload: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_aload, .index = reader_next_u16(reader, "wide aload index")
					};
				}
				case istore: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_istore, .index = reader_next_u16(reader, "wide istore index")
					};
				}
				case lstore: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_lstore, .index = reader_next_u16(reader, "wide lstore index")
					};
				}
				case fstore: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_fstore, .index = reader_next_u16(reader, "wide fstore index")
					};
				}
				case dstore: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_dstore, .index = reader_next_u16(reader, "wide dstore index")
					};
				}
				case astore: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_astore, .index = reader_next_u16(reader, "wide astore index")
					};
				}
				case iinc: {
					uint16_t index = reader_next_u16(reader, "wide iinc index");
					int16_t const_ = reader_next_i16(reader, "wide iinc const");
					return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_iinc, .iinc = {index, const_}};
				}
				case ret: {
					return (bjvm_bytecode_insn){
						.kind = bjvm_bc_insn_ret, .index = reader_next_u16(reader, "wide ret index")
					};
				}

				default: {
					char buf[64];
					snprintf(buf, sizeof(buf), "invalid wide opcode %d", opcode);
					verify_error_with_free(strdup(buf));
				}
			}
		}

		case multianewarray: {
			uint16_t index = reader_next_u16(reader, "multianewarray index");
			uint8_t dimensions = reader_next_u8(reader, "multianewarray dimensions");
			bjvm_cp_class_info *entry = &checked_get_constant_pool_entry(ctx->cp, index, BJVM_CP_KIND_CLASS, "multianewarray class")->data.class_info;
			return (bjvm_bytecode_insn){.kind = bjvm_bc_insn_multianewarray, .multianewarray = { .entry = entry, .dimensions = dimensions}};
		}

		case ifnull: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifnull,
				.index = checked_pc(pc, reader_next_i16(reader, "ifnull offset"), ctx)
			};
		case ifnonnull: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_ifnonnull,
				.index = checked_pc(pc, reader_next_i16(reader, "ifnonnull offset"), ctx)
			};
		case goto_w: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_goto,
				.index = checked_pc(pc, reader_next_i32(reader, "goto_w offset"), ctx)
			};
		case jsr_w: return (bjvm_bytecode_insn){
				.kind = bjvm_bc_insn_jsr,
				.index = checked_pc(pc, reader_next_i32(reader, "jsr_w offset"), ctx)
			};

		default: {
			char buf[64];
			snprintf(buf, sizeof(buf), "invalid opcode %d", opcode);
			verify_error_with_free(strdup(buf));
		}
	}
}


char* lossy_utf8_entry_to_chars(const bjvm_cp_utf8_entry* utf8) {
	char* result = malloc(utf8->len + 1);
	int i = 0;
	for (; i < utf8->len; ++i) {
		result[i] = (char)utf8->chars[i];
	}
	result[i] = '\0';
	return result;
}

char* class_info_entry_to_string(const bjvm_cp_class_info * ent) {
	char result[1000];
	char* name = lossy_utf8_entry_to_chars(ent->name);
	snprintf(result, sizeof(result), "Class: %s", name);
	free(name);
	return strdup(result);
}

char *name_and_type_entry_to_string(const bjvm_cp_name_and_type * name_and_type) {
	char result[1000];
	char* name = lossy_utf8_entry_to_chars(name_and_type->name);
	char* type = lossy_utf8_entry_to_chars(name_and_type->descriptor);
	snprintf(result, sizeof(result), "NameAndType: %s %s", name, type);
	free(name);
	free(type);
	return strdup(result);
}

/**
 * Convert the constant pool entry to an owned string.
 */
char* constant_pool_entry_to_string(const bjvm_constant_pool_entry* ent) {
	char result[200];
	switch (ent->kind) {
		case BJVM_CP_KIND_INVALID: return strdup("<invalid>");
		case BJVM_CP_KIND_UTF8: return lossy_utf8_entry_to_chars(&ent->data.utf8);
		case BJVM_CP_KIND_INTEGER:
			snprintf(result, sizeof(result), "%d", (int) ent->data.integral.value);
			break;
		case BJVM_CP_KIND_FLOAT:
			snprintf(result, sizeof(result), "%.9gf", (float) ent->data.floating.value);
			break;
		case BJVM_CP_KIND_LONG:
			snprintf(result, sizeof(result), "%lldL", ent->data.integral.value);
			break;
		case BJVM_CP_KIND_DOUBLE:
			snprintf(result, sizeof(result), "%.15gd", (float) ent->data.floating.value);
			break;
		case BJVM_CP_KIND_CLASS:
			return class_info_entry_to_string(&ent->data.class_info);
		case BJVM_CP_KIND_STRING: {
			char* str = lossy_utf8_entry_to_chars(ent->data.string.chars);
			snprintf(result, sizeof(result), "String: '%s'", str);
			free(str);
			break;
		}
		case BJVM_CP_KIND_FIELD_REF: {
			char* class_name = class_info_entry_to_string(ent->data.fieldref_info.class_info);
			char* field_name = name_and_type_entry_to_string(ent->data.fieldref_info.name_and_type);

			snprintf(result, sizeof(result), "FieldRef: %s.%s", class_name, field_name);
			break;
		}
		case BJVM_CP_KIND_METHOD_REF:
		case BJVM_CP_KIND_INTERFACE_METHOD_REF: {
			char* class_name = class_info_entry_to_string(ent->data.fieldref_info.class_info);
			char* field_name = name_and_type_entry_to_string(ent->data.fieldref_info.name_and_type);

			snprintf(result, sizeof(result), "%s: %s.%s", ent->kind == BJVM_CP_KIND_METHOD_REF ? "MethodRef" : "InterfaceMethodRef", class_name, field_name);
			break;
		}
		case BJVM_CP_KIND_NAME_AND_TYPE: {
			return name_and_type_entry_to_string(&ent->data.name_and_type);
		}
		case BJVM_CP_KIND_METHOD_HANDLE: {
			return strdup("<method handle>");   // TODO
		}
		case BJVM_CP_KIND_METHOD_TYPE: {
			return strdup("<method type>");  // TODO
		}
		case BJVM_CP_KIND_INVOKE_DYNAMIC:
			return strdup("<indy>"); // TODO
	}
	return strdup(result);
}

char* insn_to_string(const bjvm_bytecode_insn* insn, int insn_index) {
	char buf[1000];
	char* write = buf, *end = write + sizeof(buf);

	write += snprintf(write, sizeof(buf), "%04d = pc %04d: ", insn_index, insn->program_counter);
	write += snprintf(write, end - write, "%s ", insn_code_name(insn->kind));

	printf("Printing for kind %s", insn_code_name(insn->kind));
	fflush(stdout);

	if (insn->kind <= bjvm_bc_insn_swap) {  // no operands
	} else if (insn->kind <= bjvm_bc_insn_ldc2_w) { // indexes into constant pool
		char* cp_str = constant_pool_entry_to_string(insn->cp);
		write += snprintf(write, end - write, "%s", cp_str);
		free(cp_str);
	} else if (insn->kind <= bjvm_bc_insn_astore) {  // indexes into local variables
		write += snprintf(write, end - write, "#%d", insn->index);
	} else if (insn->kind <= bjvm_bc_insn_ifnull) {  // indexes into the instruction array
		write += snprintf(write, end - write, "-> inst %d", insn->index);
	} else if (insn->kind == bjvm_bc_insn_lconst || insn->kind == bjvm_bc_insn_iconst) {
		write += snprintf(write, end - write, "%lld", insn->integer_imm);
	} else if (insn->kind == bjvm_bc_insn_dconst || insn->kind == bjvm_bc_insn_fconst) {
		write += snprintf(write, end - write, "%.15g", insn->f_imm);
	} else { // TODO

	}
	return strdup(buf);
}

char* code_attribute_to_string(const bjvm_attribute_code* attrib) {
	char** insns = malloc(attrib->insn_count * sizeof(char*));
	size_t total_length = 0;
	for (int i = 0; i < attrib->insn_count; ++i) {
		char* insn_str = insn_to_string(attrib->code + i, i);
		insns[i] = insn_str;
		total_length += strlen(insn_str) + 1;
	}
	char* result = calloc(total_length + 1, 1), *write = result;
	for (int i = 0; i < attrib->insn_count; ++i) {
		write = stpcpy(write, insns[i]);
		*write++ = '\n';
		free(insns[i]);
	}
	free(insns);
	*write = '\0';
	return result;
}

/**
 * Parse an instruction at the given program counter and advance the reader.
 * @return The parsed instruction.
 */
bjvm_bytecode_insn parse_insn(cf_byteslice *reader, uint32_t pc, bjvm_classfile_parse_ctx *ctx) {
	bjvm_bytecode_insn insn = parse_insn_impl(reader, pc, ctx);
	insn.program_counter = pc;
	return insn;
}

int convert_pc_to_insn(int pc, int* pc_to_insn, uint32_t max_pc) {
	BJVM_DCHECK(pc < (int)max_pc && pc >= 0);  // checked pc should have caught this earlier
	int insn = pc_to_insn[pc];
	if (insn == -1) {
		char buf[64];
		snprintf(buf, sizeof(buf), "invalid program counter %d", pc);
		verify_error_with_free(strdup(buf));
	}
	return insn;
}

void convert_pc_offsets_to_insn_offsets(bjvm_bytecode_insn* code, int insn_count, int* pc_to_insn, uint32_t max_pc) {
	for (int i = 0; i < insn_count; ++i) {
		bjvm_bytecode_insn* insn = &code[i];
		if (insn->kind == bjvm_bc_insn_tableswitch) {
			insn->tableswitch.default_target = convert_pc_to_insn(insn->tableswitch.default_target, pc_to_insn, max_pc);
			int count = insn->tableswitch.high - insn->tableswitch.low + 1;
			for (int j = 0; j < count; ++j) {
				insn->tableswitch.targets[j] = convert_pc_to_insn(insn->tableswitch.targets[j], pc_to_insn, max_pc);
			}
		} else if (insn->kind == bjvm_bc_insn_lookupswitch) {
			insn->lookupswitch.default_target = convert_pc_to_insn(insn->lookupswitch.default_target, pc_to_insn, max_pc);
			for (int j = 0; j < insn->lookupswitch.targets_count; ++j) {
				insn->lookupswitch.targets[j] = convert_pc_to_insn(insn->lookupswitch.targets[j], pc_to_insn, max_pc);
			}
		} else if (insn->kind >= bjvm_bc_insn_goto && insn->kind <= bjvm_bc_insn_ifnull) {
			// instruction uses index to store PC; convert to instruction
			insn->index = convert_pc_to_insn(insn->index, pc_to_insn, max_pc);  // always decreases, so ok
		}
	}
}

bjvm_attribute_code parse_code_attribute(cf_byteslice attr_reader, bjvm_classfile_parse_ctx* ctx) {
	uint16_t max_stack = reader_next_u16(&attr_reader, "max stack");
	uint16_t max_locals = reader_next_u16(&attr_reader, "max locals");
	uint32_t code_length = reader_next_u32(&attr_reader, "code length");

	uint8_t* code_start = attr_reader.bytes;

	cf_byteslice code_reader = reader_get_slice(&attr_reader, code_length, "code");
	bjvm_bytecode_insn *code = malloc(code_length * sizeof(bjvm_bytecode_insn));

	needs_free_on_verify_error(ctx, code);
	ctx->current_code_max_pc = code_length;

	int* pc_to_insn = malloc(code_length * sizeof(int));  // -1 = no corresponding instruction to that PC
	ctx_free_ticket ticket = needs_free_on_verify_error(ctx, pc_to_insn);
	memset(pc_to_insn, -1, code_length * sizeof(int));

	int insn_count = 0;
	while (code_reader.len > 0) {
		int pc = code_reader.bytes - code_start;
		pc_to_insn[pc] = insn_count;
		code[insn_count] = parse_insn(&code_reader, pc, ctx);
		++insn_count;
	}

	convert_pc_offsets_to_insn_offsets(code, insn_count, pc_to_insn, code_length);
	free(pc_to_insn);
	free_ticket(ticket);

	return (bjvm_attribute_code){
		.max_stack = max_stack,
		.max_locals = max_locals,
		.insn_count = insn_count,
		.code = code
	};
}

void parse_attribute(cf_byteslice *reader, bjvm_classfile_parse_ctx *ctx, bjvm_attribute *attr) {
	uint16_t index = reader_next_u16(reader, "method attribute name");
	attr->name = checked_get_utf8_entry(ctx->cp, index, "method attribute name");
	attr->length = reader_next_u32(reader, "method attribute length");

	cf_byteslice attr_reader = reader_get_slice(reader, attr->length, "Attribute data");
	if (compare_utf8_entry(attr->name, "Code")) {
		attr->kind = BJVM_ATTRIBUTE_KIND_CODE;
		attr->code = parse_code_attribute(attr_reader, ctx);
	} else if (compare_utf8_entry(attr->name, "ConstantValue")) {
		attr->kind = BJVM_ATTRIBUTE_KIND_CONSTANT_VALUE;
		attr->constant_value = checked_get_constant_pool_entry(
			ctx->cp,
			reader_next_u16(&attr_reader, "constant value index"),
			BJVM_CP_KIND_STRING | BJVM_CP_KIND_INTEGER | BJVM_CP_KIND_FLOAT | BJVM_CP_KIND_LONG | BJVM_CP_KIND_DOUBLE,
			"constant value");
	} else {
		attr->kind = BJVM_ATTRIBUTE_KIND_UNKNOWN;
	}
}

bjvm_cp_method parse_method(cf_byteslice *reader, bjvm_classfile_parse_ctx *ctx) {
	bjvm_cp_method method;

	method.access_flags = reader_next_u16(reader, "method access flags");
	method.name = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "method name"), "method name");
	method.descriptor = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "method descriptor"), "method descriptor");
	method.attributes_count = reader_next_u16(reader, "method attributes count");

	method.attributes = malloc(method.attributes_count * sizeof(bjvm_attribute));
	needs_free_on_verify_error(ctx, method.attributes);

	for (int i = 0; i < method.attributes_count; i++) {
		parse_attribute(reader, ctx, method.attributes + i);
	}

	return method;
}

bjvm_cp_field read_field(cf_byteslice * reader, bjvm_classfile_parse_ctx * ctx) {
	bjvm_cp_field field = {
		.access_flags = reader_next_u16(reader, "field access flags"),
		.name = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "field name"), "field name"),
		.descriptor = checked_get_utf8_entry(ctx->cp, reader_next_u16(reader, "field descriptor"), "field descriptor"),
		.attributes_count = reader_next_u16(reader, "field attributes count")
	};
	field.attributes = calloc(field.attributes_count, sizeof(bjvm_attribute));
	needs_free_on_verify_error(ctx, field.attributes);

	for (int i = 0; i < field.attributes_count; i++) {
		parse_attribute(reader, ctx, field.attributes + i);
	}

	return field;
}

/**
 * Parse a Java class file.
 * @param bytes Start byte of the classfile.
 * @param len Length of the classfile in bytes.
 * @param result Where to write the result.
 * @return NULL on success, otherwise an error message (which is the caller's responsibility to free).
 */
char *parse_classfile(uint8_t *bytes, size_t len, bjvm_classfile **result) {
	cf_byteslice reader = {.bytes = bytes, .len = len};
	bjvm_classfile *cf = *result = malloc(sizeof(bjvm_classfile));
	bjvm_classfile_parse_ctx ctx = {
		.free_on_error = NULL,
		.free_on_error_count = 0,
		.free_on_error_cap = 0,
		.cp = NULL
	};
	needs_free_on_verify_error(&ctx, cf);

	if (setjmp(verify_error_jmp_buf)) {
		for (int i = 0; i < ctx.free_on_error_count; i++) {
			free(ctx.free_on_error[i]);
		}
		free(ctx.free_on_error);
		return verify_error_needs_free ? verify_error_msg : strdup(verify_error_msg);
	}

	const uint32_t magic = reader_next_u32(&reader, "magic");
	if (magic != 0xCAFEBABE) {
		char buf[64];
		snprintf(buf, sizeof(buf), "invalid magic number 0x%08x", magic);
		verify_error_with_free(strdup(buf));
	}

	cf->minor_version = reader_next_u16(&reader, "minor version");
	cf->major_version = reader_next_u16(&reader, "major version");

	cf->pool = parse_constant_pool(&reader, &ctx);

	cf->access_flags = reader_next_u16(&reader, "access flags");
	cf->this_class = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "this class"),
	                                                  BJVM_CP_KIND_CLASS, "this class")->data.class_info;

	bool is_primordial_object = cf->is_primordial_object =
		compare_utf8_entry(cf->this_class->name, "java/lang/Object");

	uint16_t super_class = reader_next_u16(&reader, "super class");
	cf->super_class = is_primordial_object ? NULL :
		&checked_get_constant_pool_entry(cf->pool, super_class, BJVM_CP_KIND_CLASS, "super class")->data.class_info;

	cf->interfaces_count = reader_next_u16(&reader, "interfaces count");
	cf->interfaces = malloc(cf->interfaces_count * sizeof(bjvm_cp_class_info *));
	needs_free_on_verify_error(&ctx, cf->interfaces);

	for (int i = 0; i < cf->interfaces_count; i++) {
		cf->interfaces[i] = &checked_get_constant_pool_entry(cf->pool, reader_next_u16(&reader, "interface"),
		                                                     BJVM_CP_KIND_CLASS, "superinterface")->data.class_info;
	}

	cf->fields_count = reader_next_u16(&reader, "fields count");
	cf->fields = malloc(cf->fields_count * sizeof(bjvm_cp_field));
	needs_free_on_verify_error(&ctx, cf->fields);

	for (int i = 0; i < cf->fields_count; i++) {
		cf->fields[i] = read_field(&reader, &ctx);
	}

	cf->methods_count = reader_next_u16(&reader, "methods count");
	cf->methods = malloc(cf->methods_count * sizeof(bjvm_cp_method));
	needs_free_on_verify_error(&ctx, cf->methods);

	for (int i = 0; i < cf->methods_count; ++i) {
		cf->methods[i] = parse_method(&reader, &ctx);
	}

	// Parse attributes
	cf->attributes_count = reader_next_u16(&reader, "class attributes count");
	cf->attributes = malloc(cf->attributes_count * sizeof(bjvm_attribute));
	needs_free_on_verify_error(&ctx, cf->attributes);

	for (int i = 0; i < cf->attributes_count; i++) {
		parse_attribute(&reader, &ctx, cf->attributes + i);
	}

	free(ctx.free_on_error); // we made it :)
	return NULL;
}
