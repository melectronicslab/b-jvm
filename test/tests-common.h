//
// Created by alec on 12/21/24.
//

#ifndef TESTS_COMMON_H
#define TESTS_COMMON_H

#include <bjvm.h>

#include <memory>
#include <filesystem>
#include <vector>
#include <string>
#include <optional>

#include "doctest/doctest.h"

using std::string;
using std::optional;
using std::vector;
using std::basic_string_view;

namespace Bjvm::Tests {
static inline std::string_view to_string_view(heap_string str) {
    return {str.chars, (size_t)str.len};
}

    static inline std::string_view to_string_view(slice str) {
        return {str.chars, (size_t)str.len};
    }


    std::unique_ptr<vm, void(*)(vm*)> CreateTestVM(vm_options options = default_vm_options());
  std::vector<std::string> ListDirectory(const std::string &path,
                                         bool recursive);
  bool EndsWith(const std::string &s, const std::string &suffix);

    std::optional<std::vector<u8>> ReadFile(const std::string &file);

struct TestCaseResult {
  std::string_view stdin_;
  std::string stdout_;
  std::string stderr_;
};

struct ScheduledTestCaseResult : TestCaseResult {
  int yield_count;
  int sleep_count;
  u64 ms_slept;
};

TestCaseResult run_test_case(std::string classpath, bool capture_stdio = true, std::string main_class = "Main", std::string input = "");
ScheduledTestCaseResult run_scheduled_test_case(std::string classpath, bool capture_stdio = true, std::string main_class = "Main", std::string input = "");
}

#endif // TESTS_COMMON_H
