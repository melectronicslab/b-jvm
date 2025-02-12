
#include "doctest/doctest.h"
#include <wasm/wasm_utils.h>

TEST_SUITE_BEGIN("[wasm]");

TEST_CASE("write leb128 unsigned") {
  bytevector ctx = {nullptr};
  const u8 expected[4] = {0x00, 0xE5, 0x8E, 0x26};
  wasm_writeuint(&ctx, 0);
  wasm_writeuint(&ctx, 624485);
  REQUIRE(arrlen(ctx.bytes) == 4);
  REQUIRE(memcmp(ctx.bytes, expected, 4) == 0);
  arrfree(ctx.bytes);
}

TEST_CASE("write leb128 signed") {
  bytevector ctx = {nullptr};
  const u8 expected[4] = {0x00, 0xC0, 0xBB, 0x78};
  wasm_writeint(&ctx, 0);
  wasm_writeint(&ctx, -123456);
  REQUIRE(arrlen(ctx.bytes) == 4);
  REQUIRE(memcmp(ctx.bytes, expected, 4) == 0);
  arrfree(ctx.bytes);
}

TEST_CASE("Simple module") {
  wasm_module *module = wasm_module_create();

  wasm_value_type params_[] = {WASM_TYPE_KIND_INT32, WASM_TYPE_KIND_INT32};
  wasm_type params = wasm_make_tuple(module, params_, 2);

  wasm_expression *body =
      wasm_binop(module, WASM_OP_KIND_I32_ADD, wasm_i32_const(module, 1), wasm_i32_const(module, 2));

  wasm_expression *ifelse =
      wasm_if_else(module, wasm_unop(module, WASM_OP_KIND_I32_EQZ, wasm_local_get(module, 0, wasm_int32())),
                   wasm_i32_const(module, 2), body, wasm_int32());

  wasm_type locals = wasm_make_tuple(module, params_, 2);
  // Types should be interned
  REQUIRE(memcmp(&locals, &params, sizeof(locals)) == 0);

  wasm_function *fn = wasm_add_function(module, params, wasm_int32(), locals, ifelse, "add");
  wasm_export_function(module, fn);

  bytevector serialized = wasm_module_serialize(module);

  wasm_module_free(module);
  arrfree(serialized.bytes);
}

TEST_SUITE_END;