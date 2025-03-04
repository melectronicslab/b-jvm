
public class Main {

    public static void main(String[] args) throws Exception {
        Thread t = new Thread(() -> {
            System.out.println("starting sleep");
            long start = System.currentTimeMillis();
            try {
                Thread.sleep(Long.MAX_VALUE);
            } catch (InterruptedException e) {
                long end = System.currentTimeMillis();
                System.out.println("interrupted");
                long slept = end - start;
                System.out.println("slept? " + (slept > 800 && slept < 1200)); // roughly 1 second
            } finally {
                System.out.println("finally");
            }
        });

        System.out.println("starting thread");
        t.start();

        Thread.sleep(1_000); // 1 second

        System.out.println("interrupting thread");
        t.interrupt();

        t.join();
        System.out.println("joined; exiting");
    }
}