class AsyncNative {
    private native int asyncNativeMethod(int input);
    public static void main(String[] argv) {
        var asyncNative = new AsyncNative();

        System.out.println(asyncNative.asyncNativeMethod(5));
        System.out.println(asyncNative.asyncNativeMethod(10));
    }
}