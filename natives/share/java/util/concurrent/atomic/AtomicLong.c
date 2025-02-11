#include <natives-dsl.h>

DECLARE_NATIVE("java/util/concurrent/atomic", AtomicLong, VMSupportsCS8,
               "()Z") {
  return (stack_value){.i = 1};
}