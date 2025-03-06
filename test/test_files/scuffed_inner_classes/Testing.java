public class Testing {
    public static void main(String[] args) {
        Outer outer = new Outer("Hello");
        Outer.Inner inner = outer.getInner();

        System.out.println("Outer message: " + outer.message()); // Hello
        System.out.println("Legitimate inner message: " + inner.message()); // Inner: Hello
        System.out.println("Outer message (according to legitimate inner): " + inner.getOuterMessage()); // Hello

        // invaded by the french!
        System.out.println("Creating a fake inner object assigned to the outer");
        ChildOfInner innerChild = new ChildOfInner(outer);
        System.out.println("Outer message (according to fake inner): " + innerChild.getOuterMessage()); // Bonjour
        System.out.println("Fake inner message: " + innerChild.message()); // Inner: Hello

        System.out.println("Setting outer message using fake inner");
        innerChild.setOuterMessage("Bonjour");

        System.out.println("Outer message (according to fake inner): " + innerChild.getOuterMessage()); // Bonjour
        System.out.println("Outer message (according to legitimate inner): " + inner.getOuterMessage()); // Bonjour

        System.out.println("Testing another implanted inner object");
        System.out.println("Message of new inner: " + new ChildOfInner(outer).message()); // Inner: Bonjour
    }

    public static class Outer {
        private String message;
        private final Inner inner; // only one inner class per parent

        public Outer(String message) {
            this.message = message;
            this.inner = new Inner();
        }

        public Inner getInner() {
            return inner;
        }

        public String message() {return message;}

        private class Inner {
            private String message;

            // Constructs a message based on outer message
            public Inner(Outer Outer.this) {
                this.message = Outer.this.message + " from Inner";
            }

            public String message() {return message;}

            public String getOuterMessage() {
                return Outer.this.message;
            }

            public void setOuterMessage(String message) {
                Outer.this.message = message;
            }
        }
    }

    public static class ChildOfInner extends Outer.Inner {
        public ChildOfInner(Outer outer) {
            outer.super();
        }
    }
}