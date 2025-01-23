//
// Created by alec on 1/17/25.
//

#include <climits>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <unordered_map>

#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/catch_test_macros.hpp>

#include <async.h>
#include "tests-common.h"

struct async_wakeup_info {
  int index;
};

DECLARE_ASYNC(int, test_yield, locals(async_wakeup_info info;), arguments(int index),);
DEFINE_ASYNC(test_yield) {
  self->info.index = args->index;
  ASYNC_YIELD(&self->info);
  ASYNC_END(9);
}

#include <natives-dsl.h>

DECLARE_ASYNC_NATIVE("", AsyncNative, asyncNativeMethod, "(I)I", locals(), invoked_methods(invoked_method(test_yield))) {
  AWAIT(test_yield, 1);
  ASYNC_END((bjvm_stack_value){.i = get_async_result(test_yield)*2});
}

#define _DECLARE_CACHED_STATE(_)
#define _RELOAD_CACHED_STATE()

namespace a {
TEST_CASE("Async natives") {
  Bjvm::Tests::run_test_case
}
}