#include <natives-dsl.h>

CreateJavaMethodBinding(init_ite, jobject, "java/lang/reflect/InvocationTargetException", "<init>",
                        "(Ljava/lang/Throwable;)V", 1, jobject throwable);

DECLARE_ASYNC_NATIVE("sun/reflect", NativeConstructorAccessorImpl, newInstance0,
                     "(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;", locals(),
                     invoked_methods(invoked_method(run_thread) invoked_method(init_ite) invoked_method(bjvm_initialize_class))) {
  bjvm_cp_method *method = *bjvm_unmirror_ctor(args[0].handle->obj);

  AWAIT(bjvm_initialize_class, thread, method->my_class);
  if (thread->current_exception)
    ASYNC_RETURN(value_null());

  bjvm_obj_header *instance = new_object(thread, method->my_class);
  bjvm_stack_value *forward_args = malloc(sizeof(bjvm_stack_value) * *ArrayLength(args[1].handle->obj));
  forward_args[0] = (bjvm_stack_value){.obj = instance};
  if (args[1].handle->obj) {
    for (int i = 0; i < *ArrayLength(args[1].handle->obj); ++i) {
      forward_args[i + 1] = *((bjvm_stack_value *)ArrayData(args[1].handle->obj) + i);
    }
  }

  AWAIT(run_thread, thread, method, forward_args);

  if (thread->current_exception) {
    bjvm_classdesc *classdesc = bootstrap_lookup_class(thread, STR("java/lang/reflect/InvocationTargetException"));
    bjvm_obj_header *obj = new_object(thread, classdesc);

#define reset_except() self->args.thread->current_exception = nullptr;
    AWAIT_INNER_(reset_except, &self->invoked_async_methods.init_ite, init_ite, thread, obj, thread->current_exception);
    assert(thread->current_exception == nullptr);
#undef reset_except

    thread->current_exception = obj;
  }

  ASYNC_END((bjvm_stack_value){.obj = instance});
}
