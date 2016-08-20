package simple_classes;

class SimpleClasses {

    public static void main(String[] args) {
        TestClass testClass = new TestClass(42, "hello");
        System.out.println(testClass.x);
        System.out.println(testClass.y);
        testClass.voidMethodNoParams();
        System.out.println(testClass.intMethodWithParam(13));
    }

}
