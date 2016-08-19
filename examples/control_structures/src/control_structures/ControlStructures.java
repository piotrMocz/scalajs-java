package control_structures;

class ControlStructures {

    public static void main(String[] args) {

        for (int i = 0; i < 10; ++i) {
            if (i % 2 == 0) {
                System.out.println("even");
            } else {
                System.out.println("odd");
            }
        }

        int i = 0;
        while (i < 3) {
            System.out.println(i);
            i++;
        }

        i = 2;
        do {
            System.out.println(i--);
        } while (i >= 0);
    }

}