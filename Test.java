import java.io.Serializable;
import java.util.ArrayList;

class Test extends Object implements Serializable {

    private int field1;
    int[] arr1;
    int[] arr2 = {1,2,3,4,5,6};

    private static class Test2<T> {
        T field1;
        ArrayList<T> field2;

        Test2(T x) {
            field1 = x;
        }

        private T method2(T x) {
            System.out.println(x);
            return null;
        }
    }

    public Test(int x) {
        this.field1 = x;

        arr1 = new int[10];
        arr1[0] = 14;
        int var2 = arr2[1];
    }

    public static void main(String[] args) {
        int var1 = 14;
        ArrayList<String> arr3 = new ArrayList<String>(5);
        arr3.add("a string");
        String var2 = arr3.get(0);
        double var3 = method1(var1, 14.0);

        Test obj1 = new Test(123);

        System.out.println("Hello");

        for (;;) {
            var1++;
            if (var1 > 5) break;
            else continue;
        }
    }

    public static double method1(int x, double y) {
        if (x > y)
            return x * y;
        else
            return x + y;
    }

    public double method2() {
        if (3 > 4) System.out.println("Hello!");

        double sum = 0.0;
        for (int i = 0; i < 10; i++)
            sum += 4.5 * i;

        return sum;
    }

    public void method3(ArrayList<String> arr) {
        this.method2();

        for (String s : arr) {
            System.out.println(s);
        }
    }
}