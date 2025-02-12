//
// Created by alec on 1/13/25.
//

#ifndef CLASSLOADER_H
#define CLASSLOADER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "bjvm.h"

typedef struct {
  obj_header *classloader_obj;
} classloader;

extern classloader bootstrap_classloader;

#ifdef __cplusplus
}
#endif

#endif // CLASSLOADER_H
