class Test {
    int x;

    Test(int param) {
        int local;

        this.x = param;
    }

    public static void main(String[] args) {
        double y = 0.0;
        y += 42;

        for (int i = 0; i < 3; i++)
            System.out.println("for loop");

        int j = 5;
        while (j > 0) {
            System.out.println("while loop");
            j--;
        }

        int k = 0;
        do {
            System.out.println(k);
            k++;
        } while (k < 3);

        System.out.println(y++);
        System.out.println(++y);
        System.out.println(2 + 2);
        System.out.println((y = 23.0) + 4.5);
        System.out.println(y);
        System.out.println("Hello world!!!");
    }
}
