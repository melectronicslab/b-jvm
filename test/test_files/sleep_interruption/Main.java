
public class Main {

    public static void main(String[] args) throws Exception {
        Thread t = new Thread(() -> {
            System.out.println("starting sleep");
            try {
                Thread.sleep(10_000); // 10 seconds
            } catch (InterruptedException e) {
                System.out.println("interrupted");
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