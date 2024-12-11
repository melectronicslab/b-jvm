//
// Created by Cowpox on 12/10/24.
//

#ifndef BJVM_H
#define BJVM_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

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
	bjvm_bc_insn_goto, bjvm_bc_insn_jsr,

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

typedef struct {
	wchar_t *chars;
	int len;
} bjvm_cp_utf8_entry;

typedef struct {
	bjvm_cp_utf8_entry *name;
} bjvm_cp_class_info;

typedef struct bjvm_cp_name_and_type {
	bjvm_cp_utf8_entry *name;
	bjvm_cp_utf8_entry *descriptor;
} bjvm_cp_name_and_type;

typedef struct {
	bjvm_cp_class_info *class_info;
	bjvm_cp_name_and_type *name_and_type;
} bjvm_cp_field_info;

// Used by both methodref and interface methodref
typedef struct {
	bjvm_cp_class_info *class_info;
	bjvm_cp_name_and_type *name_and_type;
} bjvm_cp_method_info;

typedef struct {
	bjvm_cp_utf8_entry *chars;
} bjvm_cp_string_info;

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
} bjvm_cp_entry_kind;

typedef struct bjvm_cp_entry {
	bjvm_cp_entry_kind kind;

	union {
		bjvm_cp_utf8_entry utf8;
		bjvm_cp_string_info string;

		bjvm_cp_floating_info floating;
		bjvm_cp_integral_info integral;

		bjvm_cp_name_and_type name_and_type;
		bjvm_cp_class_info class_info;

		bjvm_cp_field_info fieldref_info;
		bjvm_cp_method_info methodref_info;
		bjvm_cp_method_handle_info method_handle_info;
		bjvm_cp_method_type_info method_type_info;
		bjvm_cp_invoke_dynamic_info invoke_dynamic_info;
	} data;
} bjvm_cp_entry;

struct bjvm_multianewarray_data {
	bjvm_cp_class_info *entry;
	uint8_t dimensions;
};

typedef struct {
	bjvm_insn_code_kind kind;
	int program_counter;

	union {
		// for newarray
		bjvm_primitive_kind array_type;
		// constant pool index or local variable index or branch target (instruction index)
		uint32_t index;
		// Integer or long immediate
		int64_t integer_imm;
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
		bjvm_cp_entry *cp;
	};
} bjvm_bytecode_insn;


typedef struct bjvm_constant_pool {
	int entries_len;
	bjvm_cp_entry entries[];
} bjvm_constant_pool;

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

typedef enum {
	BJVM_ATTRIBUTE_KIND_CODE = 0,
	BJVM_ATTRIBUTE_KIND_CONSTANT_VALUE = 1,
	BJVM_ATTRIBUTE_KIND_UNKNOWN = 2,
} bjvm_attribute_kind;

typedef struct {
	uint16_t max_stack;
	uint16_t max_locals;
	int insn_count;
	int max_pc;

	bjvm_bytecode_insn *code;
} bjvm_attribute_code;

typedef struct bjvm_attribute {
	bjvm_attribute_kind kind;
	bjvm_cp_utf8_entry *name;
	uint32_t length;

	union {
		bjvm_attribute_code code;
		bjvm_cp_entry* constant_value;
	};
} bjvm_attribute;

typedef struct bjvm_cp_method {
	bjvm_access_flags access_flags;

	bjvm_cp_utf8_entry *name;
	bjvm_cp_utf8_entry *descriptor;

	int attributes_count;
	bjvm_attribute *attributes;
	bjvm_attribute_code* code;
} bjvm_cp_method;

typedef struct {
	bjvm_access_flags access_flags;
	bjvm_cp_utf8_entry* name;
	bjvm_cp_utf8_entry* descriptor;

	int attributes_count;
	bjvm_attribute* attributes;
	// Offset of the field in the static or instance data area
	int byte_offset;
} bjvm_cp_field;

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
	bjvm_cp_field* fields;

	int methods_count;
	bjvm_cp_method* methods;

	int attributes_count;
	bjvm_attribute* attributes;

	// Whether this class corresponds to the primordial object class
	bool is_primordial_object;
} bjvm_parsed_classfile;

typedef uint64_t bjvm_mark_word_t;

typedef struct bjvm_klass bjvm_klass;

// Equivalent to HotSpot's Klass
typedef struct bjvm_klass {
	// Superclass of this klass
	bjvm_klass* super;
	// Access flags
	bjvm_access_flags access_flags;
	// Size in bytes of an instance (ignoring array data)
	uint32_t instance_size;
	// Name of this class (e.g. "java/lang/Object")
	bjvm_cp_utf8_entry* name;
} bjvm_klass;

typedef struct bjvm_array_klass {
	bjvm_klass* base;

	int dimensions;
	bjvm_klass* base_component;
} bjvm_array_class;

// Equivalent to HotSpot's InstanceKlass
typedef struct bjvm_ordinary_class {
	bjvm_klass base;

	int arrays_count;
	bjvm_array_class* arrays;

	int fields_count;
	bjvm_cp_field* fields;

	int methods_count;
	bjvm_cp_method* methods;
} bjvm_instance_klass;

// Appears at the top of every object -- corresponds to HotSpot's oopDesc
typedef struct {
	volatile bjvm_mark_word_t mark_word;
	bjvm_klass* descriptor;
} bjvm_obj_header;

typedef struct bjvm_class_loader bjvm_class_loader;

typedef struct bjvm_class_loader {
	bjvm_obj_header obj_header;
	// Compare: https://github.com/openjdk/jdk8/blob/master/jdk/src/share/classes/java/lang/ClassLoader.java
	bjvm_class_loader* parent;
	// Used to synchronize class loading (instance of ConcurrentHashMap)
	bjvm_obj_header* parallelLockMap;
	// Whether this class loader is the bootstrap class loader
	bool is_bootstrap;
} bjvm_class_loader;

typedef struct {
	bjvm_class_loader* bootstrap_class_loader;

	void* classpath_manager;
} bjvm_vm;

typedef struct {
	// TODO
} bjvm_vm_options;

bjvm_vm *bjvm_create_vm(bjvm_vm_options options);

/**
 * Add the given classfile as accessible to the VM (but don't necessarily load it yet).
 */
int bjvm_vm_register_classfile(bjvm_vm* vm, const wchar_t* filename, const uint8_t* bytes, size_t len);

/**
 * Read the classfile in the class path. Returns -1 on failure to find the classfile. Writes a pointer to the classfile
 * bytes and the length of the classfile to the given pointers.
 */
int bjvm_vm_read_classfile(bjvm_vm* vm, const wchar_t* filename, const uint8_t** bytes, size_t* len);

void bjvm_vm_list_classfiles(bjvm_vm* vm, wchar_t** strings, size_t* count);

/**
 * Parse the given classfile. Writes a pointer to the parsed classfile into result. Returns an error message if the
 * parsing failed. It is the caller's responsibility to free the error message.
 */
char* bjvm_parse_classfile(uint8_t* bytes, size_t len, bjvm_parsed_classfile** result);

/**
 * Free the classfile.
 */
void bjvm_free_classfile(bjvm_parsed_classfile *cf);

#ifdef __cplusplus
}
#endif

#endif //BJVM_H
