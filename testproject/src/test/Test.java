package test;

class Test {

    public static int x = 42;

    public static void main(String[] args) {
        System.out.println(Test.x);
        System.out.println(Test2.x);
        Test2 test2 = new Test2(x);
        test2.foo(23);
        System.out.println(Test2.bar(x, 13));
    }
}