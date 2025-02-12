#ifndef LINKAGE_H
#define LINKAGE_H

#include <bjvm.h>

int link_class(vm_thread *thread, classdesc *classdesc);
void setup_super_hierarchy(classdesc *classdesc);

#endif