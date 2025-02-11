#include <natives-dsl.h>

DECLARE_NATIVE("java/lang", StringUTF16, isBigEndian, "()Z") {
  DCHECK(argc == 0);
  return (stack_value){.i = 0};
}
