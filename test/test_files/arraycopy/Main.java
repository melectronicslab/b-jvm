public class Main {
    public static void main(String[] args) {
        SystemArrayCopyTest.testSystemArrayCopyErrors();
    }
}

class SystemArrayCopyTest {
    static void testSystemArrayCopyErrors() {
        // Null source array
        try {
            System.arraycopy(null, 0, new int[5], 0, 5);
        } catch (NullPointerException e) {
            System.out.print("a");
        }

        // Null destination array
        try {
            System.arraycopy(new int[5], 0, null, 0, 5);
        } catch (NullPointerException e) {
            System.out.print("b");
        }

        // Source array out of bounds
        try {
            int[] src = new int[5];
            int[] dest = new int[5];
            System.arraycopy(src, -1, dest, 0, 5);
        } catch (IndexOutOfBoundsException e) {
            System.out.print("c");
        }

        // Destination array out of bounds
        try {
            int[] src = new int[5];
            int[] dest = new int[5];
            System.arraycopy(src, 0, dest, -1, 5);
        } catch (IndexOutOfBoundsException e) {
            System.out.print("d");
        }

        // Length exceeds source array bounds
        try {
            int[] src = new int[5];
            int[] dest = new int[5];
            System.arraycopy(src, 0, dest, 0, 10);
        } catch (IndexOutOfBoundsException e) {
            System.out.print("e");
        }

        // Length exceeds destination array bounds
        try {
            int[] src = new int[5];
            int[] dest = new int[5];
            System.arraycopy(src, 0, dest, 2, 5);
        } catch (IndexOutOfBoundsException e) {
            System.out.print("f");
        }

        try {
            Object[] src = new String[5];
            Integer[] dest = new Integer[5];
            System.arraycopy(src, 0, dest, 0, 5);
        } catch (ArrayStoreException e) {
            System.out.print("this is ok");
        }

        Integer[] dest = new Integer[5];

        // Source and destination arrays of different types
        try {
            Object[] src = new String[5];
            src[1] = "hello";
            dest[0] = 10;
            System.arraycopy(src, 0, dest, 0, 5);
        } catch (ArrayStoreException e) {
            System.out.print("g");
        }

        if (dest[0] == null)
            System.out.print("h");
    }
}
