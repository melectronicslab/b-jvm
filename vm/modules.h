//
// Created by alec on 1/13/25.
//

#ifndef MODULES_H
#define MODULES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <util.h>
#include <adt.h>

typedef enum {
    MODULE_NORMAL,
    MODULE_OPEN,
} module_kind;

typedef enum {
    MODULEPATH_ENTRY_DEFINITION,
    MODULEPATH_ENTRY_DIRECTORY
} modulepath_entry_kind;

typedef struct {
    module_kind kind;
} module_definition;

typedef struct {
    heap_string path;
    module_definition *module_definitions;
    size_t module_count;
} module_directory;

typedef struct {
    modulepath_entry_kind kind;
    union {
        module_definition *module;
        module_directory *directory;
    };
} modulepath_entry;

typedef struct {
    modulepath_entry *entries;
    int entries_len;
    int entries_cap;

    heap_string as_colon_separated;
} modulepath;

#ifdef __cplusplus
}
#endif

#endif //MODULES_H
