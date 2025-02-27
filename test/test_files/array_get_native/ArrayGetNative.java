// Test case for Array.get

import java.lang.reflect.Array;

public class ArrayGetNative {
    public static void main(String[] args) {
        int[] array = {1, 2, 3, 4, 5};
        for (int i = 0; i < array.length; i++) {
            System.out.print(Array.get(array, i));
        }

        try {
            Array.get(array, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        long[] array1 = {1, 2, 3, 4, 5};
        for (int i = 0; i < array1.length; i++) {
            System.out.print(Array.get(array1, i));
        }

        try {
            Array.get(array1, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        short[] array2 = {1, 2, 3, 4, 5};
        for (int i = 0; i < array2.length; i++) {
            System.out.print(Array.get(array2, i));
        }

        try {
            Array.get(array2, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        double[] array3 = {1.0, 2.0, 3.0, 4.0, 5.0};
        try {
            Array.get((Double)array3[0], 0);
        } catch (IllegalArgumentException e) {
            System.out.println("Caught IllegalArgumentException");
        }

        try {
            Array.get(array3, -1);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        byte[] array4 = {1, 2, 3, 4, 5};
        for (int i = 0; i < array4.length; i++) {
            System.out.print(Array.get(array4, i));
        }
        try {
            Array.get(array4, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        char[] array5 = {'a', 'b', 'c', 'd', 'e'};
        for (int i = 0; i < array5.length; i++) {
            System.out.print(Array.get(array5, i));
        }
        try {
            Array.get(array5, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        boolean[] array6 = {true, false, true, false, true};
        for (int i = 0; i < array6.length; i++) {
            System.out.print(Array.get(array6, i));
        }

        try {
            Array.get(array6, 5);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }

        Object[] array7 = { "hello", "world", "foo", "bar", "baz" };
        for (int i = 0; i < array7.length; i++) {
            System.out.print(Array.get(array7, i));
        }

        try {
            System.out.print(Array.get(array7, 5));
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Caught ArrayIndexOutOfBoundsException");
        }
    }
}