#include <natives-dsl.h>
#include <stdio.h>
#include <limits.h>

DECLARE_NATIVE("sun/nio/ch", IOUtil, initIDs, "()V") {
  return value_null();
}

DECLARE_NATIVE("sun/nio/ch", IOUtil, iovMax, "()I") {
  return (stack_value){.i = 1024};
}

DECLARE_NATIVE("sun/nio/ch", IOUtil, writevMax, "()J") {
  return (stack_value){.l = 1024};
}