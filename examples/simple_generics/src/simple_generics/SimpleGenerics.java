package simple_generics;

class SimpleGenerics {

    public static void main(String[] args) {
        TestClass<Integer> testClass = new TestClass<>(42);
        int n = testClass.genericField;
        System.out.println(testClass.genericField);
        System.out.println(n);
    }

}