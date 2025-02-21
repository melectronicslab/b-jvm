#include <natives-dsl.h>
#include <objects.h>

DECLARE_NATIVE("java/lang", String, intern, "()Ljava/lang/String;") {
  return (stack_value){.obj = InternJString(thread, obj->obj)};
}