#include <errno.h>
#include <natives-dsl.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/poll.h>
#include <sys/stat.h>
#include <unistd.h>

DECLARE_NATIVE("java/io", FileInputStream, initIDs, "()V") {
  return value_null();
}

static bjvm_obj_header **get_fd(bjvm_obj_header *obj) {
  bjvm_cp_field *field = bjvm_easy_field_lookup(
      obj->descriptor, STR("fd"), STR("Ljava/io/FileDescriptor;"));
  return (void *)obj + field->byte_offset;
}

static int *get_native_fd(bjvm_obj_header *obj) {
  bjvm_cp_field *native_fd_field =
      bjvm_easy_field_lookup(obj->descriptor, STR("fd"), STR("I"));
  return (void *)obj + native_fd_field->byte_offset;
}

DECLARE_NATIVE("java/io", FileInputStream, open0, "(Ljava/lang/String;)V") {
  if (!args[0].handle->obj)
    return value_null();

  heap_string filename = AsHeapString(args[0].handle->obj, on_oom);

  bjvm_obj_header *fd = *get_fd(obj->obj);
  assert(fd);
  int unix_fd = open(filename.chars, O_RDONLY);
  if (unix_fd < 0) {
    // TODO use errno to give a better error message
    bjvm_raise_vm_exception(thread, STR("java/io/FileNotFoundException"),
                         hslc(filename));
  } else {
    *get_native_fd(fd) = unix_fd;
  }
  free_heap_str(filename);
  return value_null();

  on_oom:
  return value_null();
}

DECLARE_NATIVE("java/io", FileInputStream, readBytes, "([BII)I") {
  assert(argc == 3);
  bjvm_obj_header *fd = *get_fd(obj->obj);
  assert(fd);
  int unix_fd = *get_native_fd(fd);
  if (unix_fd < 0) {
    bjvm_raise_vm_exception(thread, STR("java/io/FileNotFoundException"),
                            STR("File not found"));
    return value_null();
  }

  bjvm_obj_header *array = args[0].handle->obj;
  if (!array) {
    ThrowLangException(NullPointerException);
    return value_null();
  }

  int offset = args[1].i;
  int length = args[2].i;

  if (offset < 0 || length < 0 || offset + length > *ArrayLength(array)) {
    ThrowLangException(ArrayIndexOutOfBoundsException);
    return value_null();
  }

  int bytes_read = (int) read(unix_fd, ArrayData(array) + offset, length);
  if (bytes_read < 0) {
    bjvm_raise_vm_exception(thread, STR("java/io/IOException"),
                           STR("Error reading file"));
    return value_null();
  }
  return (bjvm_stack_value) { .i = bytes_read == 0 ? -1 : bytes_read };
}

DECLARE_NATIVE("java/io", FileInputStream, close0, "()V") {
  bjvm_obj_header *fd = *get_fd(obj->obj);
  assert(fd);
  int handle = *get_native_fd(fd);
  if (handle < 0) {
    close(handle);
  }
  *get_native_fd(fd) = -1; // make invalid again
  return value_null();
}

DECLARE_NATIVE("java/io", FileInputStream, available0, "()I") {
  bjvm_obj_header *fd = *get_fd(obj->obj);
  assert(fd);
  int unix_fd = *get_native_fd(fd);
  if (unix_fd < 0) {
    return (bjvm_stack_value) { .i = 0 };
  }

  int available;
  int err = ioctl(unix_fd, FIONREAD, &available);
  if (err < 0) {
    bjvm_raise_vm_exception(thread, STR("java/io/IOException"),
                           STR("Error getting available bytes"));
    return value_null();
  }
  return (bjvm_stack_value) { .i = (int32_t ) available };
}