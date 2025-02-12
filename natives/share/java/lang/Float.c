#include <natives-dsl.h>

DECLARE_NATIVE("java/lang", Float, floatToRawIntBits, "(F)I") { return (stack_value){.i = args[0].i}; }

DECLARE_NATIVE("java/lang", Float, intBitsToFloat, "(I)F") { return (stack_value){.i = args[0].i}; }