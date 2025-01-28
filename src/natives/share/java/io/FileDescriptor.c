#include <natives-dsl.h>
#include <unistd.h>

DECLARE_NATIVE("java/io", FileDescriptor, initIDs, "()V") {
  return value_null();
}

DECLARE_NATIVE("java/io", FileDescriptor, set, "(I)J") {
  return (bjvm_stack_value){.l = args[0].i};
}

DECLARE_NATIVE("java/io", FileDescriptor, getHandle, "(I)J") {
  return (bjvm_stack_value){.l = args[0].i};
}

DECLARE_NATIVE("java/io", FileDescriptor, getAppend, "(I)Z") {
  return (bjvm_stack_value){.i = true};
}

// unix-specific implementation: ignore the windows handle
DECLARE_NATIVE("java/io", FileDescriptor, close0, "()V") {
  int fd = LoadFieldInt(obj->obj, "fd");
  close(fd);
  return value_null();
}
