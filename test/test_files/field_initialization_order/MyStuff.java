public class MyStuff {
    private final boolean bool1 = getBool();
    private final Object obj2 = getObj();
    private final byte byte3 = getByte();
    private final long long4 = getLong();

    public MyStuff() {
        System.out.println("Evaluated constructor");
    }

    static {
        System.out.println("Evaluated static initializer");
    }

    {
        System.out.println("Evaluated instance initializer");
    }

    public String toString() {
        return "MyStuff[bool1=" + bool1 + ", obj2=" + obj2 + ", byte3=" + byte3 + ", long4=" + long4 + "]";
    }

    // methods with side effects
    static boolean getBool() {
        System.out.println("Evaluated getBool (1)");
        return true;
    }

    static Object getObj() {
        System.out.println("Evaluated getObj (2)");
        return new String("'a string lol'");
    }

    static byte getByte() {
        System.out.println("Evaluated getByte (3)");
        return 0b0000_0011;
    }

    static long getLong() {
        System.out.println("Evaluated getLong (4)");
        return 45L;
    }

}