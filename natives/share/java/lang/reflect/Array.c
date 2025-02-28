#include "bjvm.h"
#include <natives-dsl.h>
#include <arrays.h>

DECLARE_NATIVE("java/lang/reflect", Array, newArray, "(Ljava/lang/Class;I)Ljava/lang/Object;") {
  DCHECK(argc == 2);
  if (!args[0].handle->obj)
    return value_null();
  classdesc *class = unmirror_class(args[0].handle->obj);
  initialize_class_t pox = {.args = {thread, class}};
  future_t f = initialize_class(&pox);
  CHECK(f.status == FUTURE_READY);
  s32 count = args[1].i;
  obj_header *result;
  if (class->kind == CD_KIND_PRIMITIVE) {
    result = CreatePrimitiveArray1D(thread, class->primitive_component, count);
  } else {
    result = CreateObjectArray1D(thread, class, count);
  }
  return (stack_value){.obj = result};
}

DECLARE_NATIVE("java/lang/reflect", Array, getLength, "(Ljava/lang/Object;)I") {
  DCHECK(argc == 1);
  return (stack_value){.i = ArrayLength(args[0].handle->obj)};
}

DECLARE_NATIVE("java/lang/reflect", Array, get, "(Ljava/lang/Object;I)Ljava/lang/Object;") {
  DCHECK(argc == 2);

  object array = args[0].handle->obj;
  if (array->descriptor->kind == CD_KIND_ORDINARY) {
    raise_vm_exception(thread, STR("java/lang/IllegalArgumentException"), STR("Argument is not an array"));
    return value_null();
  }

  if (args[1].i < 0 || args[1].i >= ArrayLength(array)) {
    raise_vm_exception(thread, STR("java/lang/ArrayIndexOutOfBoundsException"), STR(""));
    return value_null();
  }
  switch (array->descriptor->kind) {
  case CD_KIND_ORDINARY:
  case CD_KIND_PRIMITIVE:
  default:
    UNREACHABLE();
  case CD_KIND_ORDINARY_ARRAY:
    return (stack_value){.obj = ReferenceArrayLoad(array, args[1].i)};
  case CD_KIND_PRIMITIVE_ARRAY:
    stack_value val;
    cp_method *fromPrimitive;
    switch (array->descriptor->primitive_component) {
    case TYPE_KIND_BOOLEAN:
      val.i = BooleanArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->boolean, STR("valueOf"),
        STR("(Z)Ljava/lang/Boolean;"), false, false);
      break;
    case TYPE_KIND_CHAR:
      val.i = CharArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->character, STR("valueOf"),
        STR("(C)Ljava/lang/Character;"), false, false);
      break;
    case TYPE_KIND_FLOAT:
      val.f = FloatArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->float_, STR("valueOf"),
        STR("(F)Ljava/lang/Float;"), false, false);
      break;
    case TYPE_KIND_DOUBLE:
      val.d = DoubleArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->double_, STR("valueOf"),
        STR("(D)Ljava/lang/Double;"), false, false);
      break;
    case TYPE_KIND_BYTE:
      val.i = (jint)ByteArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->byte, STR("valueOf"),
        STR("(B)Ljava/lang/Byte;"), false, false);
      break;
    case TYPE_KIND_SHORT:
      val.i = ShortArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->short_, STR("valueOf"),
        STR("(S)Ljava/lang/Short;"), false, false);
      break;
    case TYPE_KIND_INT:
      val.i = IntArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->integer, STR("valueOf"),
        STR("(I)Ljava/lang/Integer;"), false, false);
      break;
    case TYPE_KIND_LONG:
      val.l = LongArrayLoad(array, args[1].i);
      fromPrimitive = method_lookup(cached_classes(thread->vm)->long_, STR("valueOf"),
        STR("(J)Ljava/lang/Long;"), false, false);
      break;
    case TYPE_KIND_VOID:
    case TYPE_KIND_REFERENCE:
    default:
      UNREACHABLE();
    }

    DCHECK(fromPrimitive);

    // Now call fromPrimitive and return the result
    return call_interpreter_synchronous(thread, fromPrimitive, (stack_value[]){val});
  }
}