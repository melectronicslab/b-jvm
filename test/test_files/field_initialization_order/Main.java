import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {
        System.out.println("Making stuff");
        MyStuff stuff = new MyStuff();
        System.out.println(stuff);

        System.out.println("Testing funny constructor");
        var list = new ArrayList<String>() {{ System.out.println(this); add("an item"); System.out.println(this); }};
        System.out.println(list);
    }
}