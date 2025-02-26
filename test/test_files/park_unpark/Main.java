import java.util.ArrayList;

public class Main {
    private static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        CollatzConjecture collatz = new CollatzConjecture(27);
        System.out.println("Initial count: " + collatz.getN());

        ArrayList<Thread> threads = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            String name = "Worker " + i;
            long startDelay = i * 50;
            Thread t = new Thread(() -> {
                sleep(startDelay);
                // first iteration, sleep longer to pile up waiters
                int firstRes = collatz.stepAndSleep(300);
                System.out.println(name + " step: " + firstRes);
                for (;;) {
                    int res = collatz.stepAndSleep(50);
                    System.out.println(name + " step: " + res);
                    if (res == 1) {
                        System.out.println(name + " arrived");
                        break;
                    }
                }
            });
            threads.add(t);
        }

        for (Thread t : threads) {
            t.start();
        }

        for (Thread t : threads) {
            try {
                t.join();
                // System.out.println("Thread joined: " + t.getName());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Final count: " + collatz.getN()); // should be 5000
    }
}