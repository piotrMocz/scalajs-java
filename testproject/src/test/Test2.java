package test;

class Test2 {

    public static int x = 42;

    int y;

    public Test2(int y) {
        this.y = y;
    }

    static int bar(int x, int y) {
        return 2 * (x + y);
    }

    void foo(int z) {
        System.out.println(Test2.x + this.y + z);
    }
}