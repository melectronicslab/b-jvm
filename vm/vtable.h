//
// Created by Cowpox on 12/30/24.
//

#ifndef VTABLE_H
#define VTABLE_H

#include <types.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Structures to support fast invokevirtual and invokeinterface calls. All
// classes have a vtable and itables initialized at link time.

typedef struct classdesc classdesc;
typedef struct cp_method cp_method;

typedef struct vtable {
  // The first methods will always be the same methods as those of the super-
  // class. That way, an invokevirtual call can be resolved by simply looking
  // at a constant offset into this table. (This is standard.)
  cp_method **methods;
} vtable;

typedef uintptr_t itable_method_t; // cp_method*

enum : itable_method_t {
  // The method is either abstract or ambiguous, and therefore an
  // IncompatibleClassChangeError ought to be raised if it is called.
  ITABLE_METHOD_BIT_INVALID = 1 << 0,
  // The method is ambiguous: there are multiple methods with the same name
  // and signature which would be selected by an invokevirtual instruction.
  ITABLE_METHOD_BIT_AMBIGUOUS = 1 << 1
};

// The interface pointers are stored separately from the entries so that the
// scanning for the desired interface can be done a bit more quickly. (We will
// write WASM stubs for this.)
typedef struct {
  classdesc *interface;

  // All methods in the interface are found in at a consistent index, as
  // prescribed by the order in the original interface.
  //
  // The lower two bits of the method have significance, as explained above,
  // and the method is only valid in the context of an invokeinterface call if
  // the last bit is 0.
  itable_method_t *methods;
} itable;

typedef struct {
  // Scan this vector for the interface in question ...
  classdesc **interfaces;
  // ... and look for the matching itable here
  itable *entries;
} itables;

// Set up a class descriptor's itables and vtable, assuming all of its
// superinterfaces and its superclass have already been linked (and their
// itables/vtable set up).
void set_up_function_tables(classdesc *classdesc);

// Free memory associated with a class's vtable and itables.
void free_function_tables(classdesc *classdesc);

// Look up a method in the vtable. No ranges are checked.
cp_method *vtable_lookup(classdesc const *classdesc, size_t index);

// Look up a method in the itables. No ranges are checked, but nullptr is
// returned if the object does not actually implement the method, or if there
// are multiple methods that could be called.
cp_method *itable_lookup(classdesc const *cd,
                                   classdesc const *interface, size_t index);

#ifdef __cplusplus
}
#endif

#endif // VTABLE_H
