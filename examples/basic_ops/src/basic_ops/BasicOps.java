package basic_ops;

class BasicOps {

    public static void main(String[] args) {

        boolean booleanConstant = true;
        int intConstant = 42;
        double doubleConstant = 9.75;
        String stringConstant = "Hello";

        boolean booleanOpResult = false || (34 <= 17);

        int intOpResult = (2 + intConstant) * 16 / 24;
        double doubleOpResult = (2.0 + 4.0) * doubleConstant / 24.0;
        String stringOpResult = stringConstant + " " + "world";

        int intUnaryOpRes = intConstant++;
        double doubleUnaryOpRes = ++doubleConstant;

        System.out.println(booleanConstant);
        System.out.println(intConstant);
        System.out.println(doubleConstant);
        System.out.println(stringConstant);

        System.out.println(booleanOpResult);
        System.out.println(intOpResult);
        System.out.println(doubleOpResult);
        System.out.println(stringOpResult);

        System.out.println(intUnaryOpRes);
        System.out.println(doubleUnaryOpRes);
    }

}