//
// Created by alec on 1/19/25.
//

#ifndef UNIXLIKE_FILESYSTEM_H
#define UNIXLIKE_FILESYSTEM_H

#include <adt.h>
#include <bjvm.h>
#include <stddef.h>
#include <types.h>

#include "fs.h"

typedef struct {
  fs fs;
  string_hash_table synthetic_entries;

  void *inherited_data;
} unixlike_fs;

unixlike_fs const *unix_get_active_fs(void);

#endif // UNIXLIKE_FILESYSTEM_H
