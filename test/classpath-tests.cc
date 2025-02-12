#include "doctest/doctest.h"
#include <classpath.h>

TEST_SUITE_BEGIN("[classpath]");

TEST_CASE("Basic classpath operations") {
  classpath cp;
  char *error = init_classpath(&cp, STR("test_files/broken_jar1/this_is_a_jar.jar"));
  REQUIRE(error != nullptr);
  free(error);
  free_classpath(&cp);

  char *error2 = init_classpath(&cp, STR("test_files/intact_jar/ok.jar"));
  REQUIRE(error2 == nullptr);
  u8 *bytes;
  size_t len;
  int ret_val = lookup_classpath(&cp, STR("Egg.class"), &bytes, &len);
  REQUIRE(bytes != nullptr);
  REQUIRE(ret_val == 0);
  free(bytes);
  ret_val = lookup_classpath(&cp, STR("Chicken.class"), &bytes, &len);
  REQUIRE(bytes != nullptr);
  REQUIRE(ret_val == 0);
  free(bytes);
  ret_val = lookup_classpath(&cp, STR("Dog.class"), &bytes, &len);
  REQUIRE(bytes == nullptr);
  REQUIRE(ret_val == -1);
  free_classpath(&cp);
}

TEST_CASE("Folder in classpath") {
  classpath cp;
  char *error = init_classpath(&cp, STR("./jdk23.jar:test_files/circularity:test_files/classpath_test"));
  REQUIRE(error == nullptr);

  u8 *bytes;
  size_t len;
  int ret_val = lookup_classpath(&cp, STR("jdk/internal/misc/Unsafe.class"), &bytes, &len);
  REQUIRE(bytes != nullptr);
  REQUIRE(ret_val == 0);
  free(bytes);
  ret_val = lookup_classpath(&cp, STR("Chick.class"), &bytes, &len);
  REQUIRE(bytes != nullptr);
  REQUIRE(ret_val == 0);
  free(bytes);
  ret_val = lookup_classpath(&cp, STR("nested/boi/Boi.class"), &bytes, &len);
  REQUIRE(bytes != nullptr);
  REQUIRE(ret_val == 0);
  free(bytes);
  ret_val = lookup_classpath(&cp, STR("../classpath_test/Chick.class"), &bytes, &len);
  REQUIRE(bytes == nullptr);
  REQUIRE(ret_val == -1);
  free(bytes);

  free_classpath(&cp);
}

TEST_SUITE_END;