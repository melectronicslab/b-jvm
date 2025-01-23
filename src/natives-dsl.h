#ifndef NATIVES_DSL_H
#define NATIVES_DSL_H

#include "arrays.h"
#include "bjvm.h"
#include <natives-dsl.h>
#include <stddef.h>

void _push_bjvm_native(bjvm_utf8 class_name, bjvm_utf8 method_name, bjvm_utf8 signature, bjvm_native_callback native);

#define ThrowLangException(exception_name)                                                                             \
  bjvm_raise_vm_exception(thread, STR("java/lang/" #exception_name), null_str())

#define ThrowLangExceptionM(exception_name, fmt, ...)                                                                  \
  do {                                                                                                                 \
    char msg[1024];                                                                                                    \
    size_t size = snprintf(msg, 1024, fmt, __VA_ARGS__);                                                               \
    bjvm_utf8 msg_slice = {msg, size};                                                                                 \
    bjvm_raise_vm_exception(thread, STR("java/lang/" exception_name), msg_slice);                                      \
  } while (0)

#define MakeHandle(obj) bjvm_make_handle(thread, obj)

static inline bjvm_obj_header *check_is_object(bjvm_obj_header *thing) { return thing; }

#define AsHeapString(expr, on_oom)                                                                                     \
  ({                                                                                                                   \
    bjvm_obj_header *__val = check_is_object(expr);                                                                    \
    heap_string __hstr;                                                                                                \
    if (read_string_to_utf8(thread, &__hstr, __val) != 0) {                                                            \
      assert(thread->current_exception);                                                                               \
      goto on_oom;                                                                                                     \
    }                                                                                                                  \
    __hstr;                                                                                                            \
  })

#define LoadFieldObject(receiver, name, desc)                                                                          \
  ({                                                                                                                   \
    bjvm_cp_field *field = bjvm_easy_field_lookup(receiver->descriptor, STR(name), STR(desc));                         \
    assert(field && name);                                                                                             \
    bjvm_get_field(receiver, field).obj;                                                                               \
  })

#define LoadFieldLong(receiver, name)                                                                                  \
  ({                                                                                                                   \
    bjvm_cp_field *field = bjvm_easy_field_lookup(receiver->descriptor, STR(name), STR("J"));                          \
    assert(field && name);                                                                                             \
    bjvm_get_field(receiver, field).l;                                                                                 \
  })

#define LoadFieldInt(receiver, name)                                                                                   \
  ({                                                                                                                   \
    bjvm_cp_field *field = bjvm_easy_field_lookup(receiver->descriptor, STR(name), STR("I"));                          \
    assert(field && name);                                                                                             \
    bjvm_get_field(receiver, field).i;                                                                                 \
  })

static inline void StoreField(bjvm_obj_header *thing, bjvm_utf8 field_name, bjvm_stack_value value, bjvm_utf8 desc) {
  bjvm_cp_field *field = bjvm_easy_field_lookup(thing->descriptor, field_name, desc);
  assert(field);

  bjvm_set_field(thing, field, value);
}

#define GenerateStoreField(type_cap, type, stack_field, desc)                                                          \
  static inline void StoreField##type_cap(bjvm_obj_header *thing, bjvm_utf8 name, type value) {                        \
    StoreField(thing, name, (bjvm_stack_value){.stack_field = value}, STR(#desc));                                     \
  }

static inline void StoreFieldInt(bjvm_obj_header *thing, bjvm_utf8 name, int32_t value) {
  bjvm_utf8 I = (bjvm_utf8){.chars = (char *)("I"), .len = sizeof("I") - 1};
  StoreField(thing, name, (bjvm_stack_value) {.i = value }, I);
};
static inline void StoreFieldLong(bjvm_obj_header *thing, bjvm_utf8 name, int64_t value) {
  bjvm_utf8 J = (bjvm_utf8){.chars = (char *)("J"), .len = sizeof("J") - 1};
  StoreField(thing, name, (bjvm_stack_value) {.l = value }, J);
};

#undef GenerateStoreField

#define HandleIsNull(expr) ((expr)->obj == nullptr)

#define force_expand_args(macro_name, ...) macro_name(__VA_ARGS__)

extern size_t bjvm_native_count;
extern size_t bjvm_native_capacity;
extern bjvm_native_t *bjvm_natives;

#define DECLARE_NATIVE_CALLBACK(class_name_, method_name_, modifier)                                                   \
  static bjvm_stack_value class_name_##_##method_name_##_cb##modifier(                                                 \
      [[maybe_unused]] bjvm_thread *thread, [[maybe_unused]] bjvm_handle *obj, [[maybe_unused]] bjvm_value *args,      \
      [[maybe_unused]] uint8_t argc)

#define create_init_constructor(package_path, class_name_, method_name_, method_descriptor_, modifier, async_sz,       \
                                variant)                                                                               \
  __attribute__((constructor)) static void class_name_##_##method_name_##_init##modifier() {                           \
    _push_bjvm_native(                                                                                                 \
        STR(package_path "/" #class_name_), STR(#method_name_), STR(method_descriptor_),                               \
        (bjvm_native_callback){.async_ctx_bytes = async_sz,                                                            \
                               .variant = (variant##_native_callback) & class_name_##_##method_name_##_cb##modifier}); \
  }

#define DECLARE_NATIVE_(package_path, class_name_, method_name_, method_descriptor_, modifier)                         \
  DECLARE_NATIVE_CALLBACK(class_name_, method_name_, modifier);                                                        \
  create_init_constructor(package_path, class_name_, method_name_, method_descriptor_, modifier, 0, sync)              \
      DECLARE_NATIVE_CALLBACK(class_name_, method_name_, modifier)

#define DECLARE_NATIVE(package_path, class_name_, method_name_, method_descriptor_)                                    \
  force_expand_args(DECLARE_NATIVE_, package_path, class_name_, method_name_, method_descriptor_, __COUNTER__)

#define check_field_offset(m_name, member_a, member_b)                                                                 \
  _Static_assert(offsetof(struct m_name##_s, member_a) == offsetof(async_natives_args, member_b),                      \
                 #member_a " mismatch " #member_b);

#define create_async_declaration(name, locals, async_methods)                                                          \
  DECLARE_ASYNC(                                                                                                       \
    bjvm_stack_value, \
    name, \
    locals,\
    arguments(bjvm_thread *thread; bjvm_handle *obj; bjvm_value *args; uint8_t argc), \
    async_methods\
  );              \
  /* the arguments struct for this needs to be compatible with the async_natives_args struct */                        \
  /* todo: maybe just reuse the async_natives_args struct, rather than making a separate (but equivalent) struct for   \
   * each native */                                                                                                    \
  check_field_offset(name, args.thread, thread);                                                                       \
  check_field_offset(name, args.obj, obj);                                                                             \
  check_field_offset(name, args.args, args);                                                                           \
  check_field_offset(name, args.argc, argc);                                                                           \
  check_field_offset(name, _state, stage);

#undef _DECLARE_CACHED_STATE
#undef _RELOAD_CACHED_STATE

#define _DECLARE_CACHED_STATE(_)                                                                                       \
  [[maybe_unused]] bjvm_thread *thread = self->args.thread;                                                            \
  [[maybe_unused]] bjvm_value *args = self->args.args;                                                                 \
  [[maybe_unused]] bjvm_handle *obj = self->args.obj;                                                                  \
  [[maybe_unused]] uint8_t argc = self->args.argc;

#define _RELOAD_CACHED_STATE()                                                                                         \
  do {                                                                                                                 \
    thread = self->args.thread;                                                                                        \
    args = self->args.args;                                                                                            \
    obj = self->args.obj;                                                                                              \
    argc = self->args.argc;                                                                                            \
  } while (0)

#define DECLARE_ASYNC_NATIVE_(package_path, class_name_, method_name_, method_descriptor_, locals,                     \
                              invoked_async_methods, modifier, start_index)                                            \
  create_async_declaration(class_name_##_##method_name_##_cb##modifier, locals, invoked_async_methods);                \
  create_init_constructor(package_path, class_name_, method_name_, method_descriptor_, modifier,                       \
                          sizeof(struct class_name_##_##method_name_##_cb##modifier##_s), async);                      \
  DEFINE_ASYNC_SL(class_name_##_##method_name_##_cb##modifier, start_index)

#define DECLARE_ASYNC_NATIVE_SL(package_path, class_name_, method_name_, method_descriptor_, locals,                   \
                                invoked_async_methods, start_index)                                                    \
  force_expand_args(DECLARE_ASYNC_NATIVE_, package_path, class_name_, method_name_, method_descriptor_, locals,        \
                    invoked_async_methods, __COUNTER__, start_index)

#define DECLARE_ASYNC_NATIVE(package_path, class_name_, method_name_, method_descriptor_, locals,                      \
                             invoked_async_methods)                                                                    \
  DECLARE_ASYNC_NATIVE_SL(package_path, class_name_, method_name_, method_descriptor_, locals, invoked_async_methods, 0)

typedef int32_t jint;
typedef int64_t jlong;
typedef float jfloat;
typedef double jdouble;
typedef bool jboolean;
typedef int8_t jbyte;
typedef int16_t jshort;
typedef uint16_t jchar;
typedef bjvm_obj_header *jobject;

#define empty(...)

#define CreateJavaMethodBinding(binding_name, return_type, class_name, method_name, method_descriptor, argc_, args_)   \
  DECLARE_ASYNC(return_type, binding_name, \
    locals(), \
    bjvm_thread *thread; jobject receiver; args_;, \
    invoked_methods(invoked_method(run_thread)) \
  );                                                                         \
                                                                                                                       \
  DEFINE_ASYNC_SL_(empty, binding_name, 0) {                                                                           \
    /* inline cache here? */                                                                                           \
    bjvm_cp_method *method =                                                                                           \
        bjvm_method_lookup(self->args.receiver->descriptor, STR(method_name), STR(method_descriptor), true, true);                \
    assert(method);                                                                                                    \
    assert((sizeof(self->args) - sizeof(bjvm_thread *)) / sizeof(bjvm_stack_value) ==                                  \
           method->descriptor->args_count + 1);                                                                         \
    assert((sizeof(self->args) - sizeof(bjvm_thread *)) % sizeof(bjvm_stack_value) == 0);                              \
    AWAIT_INNER_(empty, &self->invoked_async_methods.run_thread, run_thread, self->args.thread, method, (bjvm_stack_value*)self->args.receiver);                                                                \
    bjvm_stack_value result = get_async_result(run_thread); \
    ASYNC_END(*((return_type*)&result));                                                                                                       \
  }

#define CallMethod(receiver, name, desc, result, ...)                                                                  \
  do {                                                                                                                 \
    bjvm_cp_method *method = bjvm_method_lookup(receiver->descriptor, STR(name), STR(desc), true, true);               \
    assert(method);                                                                                                    \
    bjvm_stack_value args[] = {receiver, __VA_ARGS__};                                                                           \
    assert((sizeof(args) / sizeof(args[0])) == method->descriptor.args_count);                                         \
    AWAIT(run_thread, thread, method, &args);                                                                          \
    if (result != nullptr) {                                                                                           \
      *result = get_async_result(run_thread);                                                                          \
    }                                                                                                                  \
  } while (0)
#endif