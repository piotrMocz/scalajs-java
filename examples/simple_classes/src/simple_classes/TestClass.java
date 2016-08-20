package simple_classes;

class TestClass {
    int x;
    String y;

    TestClass(int x, String y) {
        this.x = x;
        this.y = y;
    }

    void voidMethodNoParams() {
        System.out.println("Void method with no params.");
        System.out.println(this.y);
    }

    int intMethodWithParam(int param) {
        System.out.println("Int method with a param.");
        return this.x + param;
    }
}