import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

import static org.junit.jupiter.api.Assertions.*;

class ReentrantLockTest {
	@Test
	void testLockUnderHighContention() throws InterruptedException, BrokenBarrierException {
		final int threadCount = 1000;
		final ReentrantLock lock = new ReentrantLock();
		final Phaser latch = new Phaser(threadCount + 1);
		class Holder {
			int counter = 0;
		}

		final Holder holder = new Holder();

		// Spawn a bunch of threads that try to lock and increment counter
		for (int i = 0; i < threadCount; i++) {
			new Thread(() -> {
				latch.arriveAndAwaitAdvance(); // Wait for all threads to be ready
				lock.lock();
				try {
					holder.counter++; // Protected by lock
				} finally {
					lock.unlock();
				}
				latch.arrive(); // Wait for all threads to finish
			}).start();
		}

		lock.lock();
		try {
			assertEquals(0, holder.counter);
		} finally {
			lock.unlock();
		}

		latch.arriveAndAwaitAdvance(); // let them start
		latch.arriveAndAwaitAdvance(); // wait for them to finish

		// Check for race conditions or deadlocks
		lock.lock();
		try {
			assertEquals(threadCount, holder.counter);
		} finally {
			lock.unlock();
		}
		assertFalse(lock.isLocked());
	}

	@Test
	void testReentrancy() {
		ReentrantLock lock = new ReentrantLock();
		lock.lock();
		assertTrue(lock.isLocked());
		lock.lock();
		assertTrue(lock.isLocked());
		lock.unlock();
		assertTrue(lock.isLocked());
		lock.unlock();
		assertFalse(lock.isLocked());

		assertTrue(lock.tryLock());
		assertTrue(lock.isLocked());
		assertTrue(lock.tryLock());
		assertTrue(lock.isLocked());
		lock.unlock();
		assertTrue(lock.isLocked());
		lock.unlock();
		assertFalse(lock.isLocked());
	}

    @RepeatedTest(10)
    void tryLockThrows() throws InterruptedException {
        ReentrantLock lock = new ReentrantLock();

        Thread.currentThread().interrupt();
        assertThrows(InterruptedException.class, lock::lockInterruptibly); // interruptible
        assertFalse(lock.isLocked());
        assertFalse(Thread.currentThread().isInterrupted()); // should clear when thrown

        Thread.currentThread().interrupt();
        assertDoesNotThrow(lock::lock); // uninterruptible
        assertTrue(lock.isLocked());
        assertTrue(Thread.currentThread().isInterrupted()); // unchanged

        assertDoesNotThrow(lock::unlock);
        assertTrue(Thread.currentThread().isInterrupted()); // unchanged
    }
}