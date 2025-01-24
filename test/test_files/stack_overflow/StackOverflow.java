
public class StackOverflow {
    static int a = 0;
    public static void main(String[] args) {
        try {
            new StackOverflow().pox(0);
        } catch (StackOverflowError e) {
            System.out.println("Caught!");
        }
        System.out.println(":)");
    }

    void pox(int a) {
        pox2(a + 1);
    }

    void pox2(int b) {
        StackOverflow.a = b;
        pox(b + 1);
    }
}