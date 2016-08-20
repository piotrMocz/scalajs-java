package interfaces;

class Interfaces {

    public static void main(String[] args) {
        TestClass1 testClass1 = new TestClass1();
        System.out.println(testClass1.testMethod(1, 2));

        TestClass2 testClass2 = new TestClass2();
        System.out.println(testClass2.testMethod(1, 2));

        TestInterface testInterface = new TestClass1();
        System.out.println(testInterface.testMethod(1, 2));
    }

}