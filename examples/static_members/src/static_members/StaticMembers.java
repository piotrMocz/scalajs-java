package static_members;

class StaticMembers {

    static int staticField = 42;

    static void staticMethod() {
        System.out.println(staticField);
    }

    public static void main(String[] args) {
        System.out.println(staticField);
        staticMethod();
        System.out.println(TestClass.staticField);
        TestClass.staticMethod();
    }

}