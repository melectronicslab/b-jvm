// Make sure the analysis gets through all of the JDK successfully.

#include <analysis.h>
#include "tests-common.h"
#include "doctest/doctest.h"

using namespace Bjvm::Tests;

TEST_CASE("Analysis passes on valid bytecode") {
  auto files = ListDirectory("jdk23", true);
  for (const auto &file : files) {
    if (!EndsWith(file, ".class"))
      continue;
    auto contents = ReadFile(file).value();
    classdesc cls;

    heap_string error;
    parse_result_t result =
        parse_classfile(contents.data(), contents.size(), &cls, &error);

    if (result != 0) {
        FAIL("Failed to parse classfile " << file << ": " << to_string_view(error));
    }

    for (int i = 0; i < cls.methods_count; ++i) {
      auto *method = cls.methods + i;
      if (!method->code)
        continue;

      int status = analyze_method_code(method, &error);

      if (status != 0) {
        FAIL("Failed to analyze method " << to_string_view(cls.name) << "#" << to_string_view(method->name) << ": " << to_string_view(error));
      }

      auto *analy = static_cast<code_analysis *>(method->code_analysis);
      scan_basic_blocks(method->code, analy);
      compute_dominator_tree(analy);

      // All Java code should be reducible because it uses structured control
      // flow!
      int failed_to_reduce = attempt_reduce_cfg(analy);
      REQUIRE(failed_to_reduce == 0);
    }

    free_classfile(cls);
  }
}

TEST_CASE("Analysis fuzzing") {
  // Ensure the analysis system doesn't hit UB/rejects things before passing
  // broken things on TODO
}