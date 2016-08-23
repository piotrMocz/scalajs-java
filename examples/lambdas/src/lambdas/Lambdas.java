package lambdas;

class Lambdas {

    public static void main(String[] args) {
        FuncInterface anonClass = new FuncInterface() {
            public int func(int x) { return 2 * x; }
        };

        FuncInterface lambda = x -> { return 2 * x; };

        System.out.println(anonClass.func(42));
        System.out.println(lambda.func(42));
    }

}