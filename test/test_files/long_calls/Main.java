public class Main {
    public static void main(String[] args) {
        LongHelper longHelper = new LongHelper();
        if (longHelper.add(1L, 2L) == 3L) {
            System.out.print("a");
        }
        if (longHelper.add(1, 2L) == 3L) {
            System.out.print("b");
        }
        if (longHelper.add(1L, 2) == 3L) {
            System.out.print("c");
        }
        if (longHelper.add(1, 2) == 3L) {
            System.out.print("d");
        }
        if (longHelper.addStatic(1L, 2L) == 3L) {
            System.out.print("a");
        }
        if (longHelper.addStatic(1, 2L) == 3L) {
            System.out.print("b");
        }
        if (longHelper.addStatic(1L, 2) == 3L) {
            System.out.print("c");
        }
        if (longHelper.addStatic(1, 2) == 3L) {
            System.out.print("d");
        }
    }
}

class LongHelper {
    public long add(long a, long b) {
        return a + b;
    }
    public long add(int a, long b) {
        return a + b;
    }
    public long add(long a, int b) {
        return a + b;
    }
    public long add(int a, int b) {
        return a + b;
    }
    public static long addStatic(long a, long b) {
        return a + b;
    }
    public static long addStatic(int a, long b) {
        return a + b;
    }
    public static long addStatic(long a, int b) {
        return a + b;
    }
    public static long addStatic(int a, int b) {
        return a + b;
    }
}