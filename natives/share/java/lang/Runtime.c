#include <natives-dsl.h>

DECLARE_NATIVE("java/lang", Runtime, availableProcessors, "()I") { return (stack_value){.i = 1}; }

DECLARE_NATIVE("java/lang", Runtime, maxMemory, "()J") { return (stack_value){.l = LLONG_MAX}; }