//
// Created by Max Cai on 2/7/25.
//

#ifndef MONITORS_H
#define MONITORS_H

#include <async.h>
#include <types.h>
#include <bjvm.h>
#include <roundrobin_scheduler.h>

DECLARE_ASYNC(int, monitor_acquire,
                    locals(handle *handle; rr_wakeup_info wakeup_info;), arguments(vm_thread *thread; obj_header *obj;),
                    invoked_methods());

//DECLARE_ASYNC(int, monitor_wait,
//                    locals(), arguments(thread *thread; obj_header *obj;),
//                    invoked_methods());

int monitor_release(vm_thread *thread, obj_header *obj);


#endif // MONITORS_H
