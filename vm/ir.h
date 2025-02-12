// Three-address code intermediate representation.

#ifndef IR_H
#define IR_H

#include "classfile.h"
#include <types.h>

typedef u32 var_t;
typedef u32 bb_t;

typedef enum { IR_TYPE_INT, IR_TYPE_LONG, IR_TYPE_FLOAT, IR_TYPE_DOUBLE } ir_type_kind;

typedef enum {
  // Unary operators
  ir_op_iadd,
} ir_op_kind;

typedef struct {
  ir_op_kind kind;
  // target <- lhs * rhs   (or possibly unary)
  var_t target;
  var_t lhs, rhs;
} ir_assign;

typedef enum {
  IR_STORE_KIND_I8,
  IR_STORE_KIND_I16,
  IR_STORE_KIND_I32,
  IR_STORE_KIND_I64,
  IR_STORE_KIND_F32,
  IR_STORE_KIND_F64
} ir_store_kind;

typedef enum {
  // Sign-extended
  IR_LOAD_KIND_I8,
  IR_LOAD_KIND_I16,
  IR_LOAD_KIND_I32,
  IR_LOAD_KIND_I64,

  IR_LOAD_KIND_U16,
  IR_LOAD_KIND_F32,
  IR_LOAD_KIND_F64
} ir_load_kind;

typedef struct {
  ir_store_kind kind;
  // mem[value] <- addr
  var_t addr;
  var_t value;
} ir_store;

typedef struct {
  ir_load_kind kind;
  // target <- mem[addr]
  var_t target;
  var_t addr;
} ir_load;

typedef struct {
  var_t ret;
} ir_return;

typedef struct {
  // Variable to select
  var_t val;
  // Predecessor block taken
  bb_t pred;
} ir_phi_entry;

typedef struct {
  var_t target;
  ir_phi_entry *entries;
  int entry_count;
} ir_phi;

typedef struct {
  // ir_insn_kind kind;
  union {};
} ir_insn;

typedef struct {
  bb_t next;
} ir_goto;

typedef struct {
  bb_t taken, not_taken;
} ir_ifelse;

typedef struct {
  bb_t default_target;
  int targetc;
  bb_t *targets;
} ir_switch;

typedef struct {
  ir_phi *phis;
  int phi_count;

  ir_insn *insns;
  int insn_count;

  // Either a return or a branching instruction
  ir_insn last;
} ir_bb;

typedef struct {

} ir_function;

#endif // IR_H
