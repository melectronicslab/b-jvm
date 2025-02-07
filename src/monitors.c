//
// Created by Max Cai on 2/7/25.
//

#include "monitors.h"

DEFINE_ASYNC(monitor_acquire) {
  // since this is a single-threaded vm, we don't need atomic operations
  self->handle = bjvm_make_handle(args->thread, args->obj);
  monitor_data *allocated_data = nullptr; // lazily allocated

  bjvm_header_word *shared_header = &self->handle->obj->header_word;
  bjvm_header_word fetched_header;
  __atomic_load(shared_header, &fetched_header, __ATOMIC_ACQUIRE);
  monitor_data *data;
  for (;;) { // loop until a monitor data is initialized
    data = inspect_monitor(&fetched_header);
    if (likely(data)) {
      break; // the monitor exists
    } else { // we need to allocate one ourselves
      if (!allocated_data) allocated_data = allocate_monitor(args->thread);
      if (unlikely(!allocated_data)) { // seriously? allocation failed?
        bjvm_drop_handle(args->thread, self->handle);
        ASYNC_RETURN(-1); // oom todo
      }

      allocated_data->mark_word = fetched_header.mark_word;
      allocated_data->tid = -1; // not owned (as a microöptimisation, we could claim it here beforehand)
      allocated_data->hold_count = 0;

      bjvm_header_word proposed_header = { .expanded_data = allocated_data };

      printf("putting monitor data at %p\n", shared_header);

      // try to put it in- loop again if CAS fails
      if (__atomic_compare_exchange(shared_header, &fetched_header, &proposed_header,
                                    false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE)) {
        break; // success
      }
    }
  }

  // now, a monitor is guaranteed to exist
  for (;;) {
    monitor_data *lock = __atomic_load_n((monitor_data **) &self->handle->obj->header_word, __ATOMIC_ACQUIRE); // must refetch
    assert(lock);
    s32 freed = -1;

    if (__atomic_load_n(&lock->tid, __ATOMIC_ACQUIRE) == args->thread->tid) {
      // we already own the monitor
      lock->hold_count++; // I think (?) this works under the C memory model, but if not, just use volatile
      printf("(tid %d) incremented hold count: %d\n", args->thread->tid, lock->hold_count);
      break;
    }

    // try to acquire mutex- loop again if CAS fails
    if (__atomic_compare_exchange_n(&lock->tid, &freed, args->thread->tid,
                                  false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE)) {
      printf("acquiring from thread %d\n", args->thread->tid);
      lock->hold_count = 1;
      break; // success
    }

    // since the strong CAS failed, we need to wait

    // todo: tell make special yield on monitor mode for scheduler instead of spinning
    printf("yielding from thread %d\n", args->thread->tid);
    self->wakeup_info.kind = RR_WAKEUP_YIELDING;
    ASYNC_YIELD((void *) &self->wakeup_info);
  }

  // done!
  bjvm_drop_handle(args->thread, self->handle);
  ASYNC_END(0);
}

int monitor_release(bjvm_thread *thread, bjvm_obj_header *obj) {
  // since this is a single-threaded vm, we don't need atomic operations
  // no handles necessary because no GC (i hope)
  bjvm_header_word fetched_header;
  __atomic_load(&obj->header_word, &fetched_header, __ATOMIC_ACQUIRE);
  monitor_data *lock = inspect_monitor(&fetched_header);

  // todo: error code enum? or just always cause an InternalError/IllegalMonitorStateException
  if (unlikely(!lock)) return -1;
  if (unlikely(lock->tid != thread->tid)) return -1;
  if (unlikely(lock->hold_count == 0)) return -1;

  u32 new_hold_count = --lock->hold_count;
  printf("decremented hold count: %d\n", lock->hold_count);

  if (new_hold_count == 0) {
    lock->tid = -1;
    printf("released monitor\n");
  }
  return 0;
}
