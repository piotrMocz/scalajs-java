package arrays;

class Arrays {

    public static void main(String[] args) {
        String[] stringArr = {"scala.js", "java", "is", "a", "cool", "compiler"};

        for (int i = 0; i < 6; ++i) {
            System.out.println(stringArr[i]);
        }

        int[][] intArr = new int[2][3];
        for (int i = 0; i < 2; ++i)
            for (int j = 0; j < 3; ++j)
                intArr[i][j] = i + j;

        for (int i = 0; i < 2; ++i)
            for (int j = 0; j < 3; ++j)
                System.out.println(intArr[i][j]);
    }

}