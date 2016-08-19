package assignments;

class Assignments {

    public static void main(String[] args) {
        float x = 0f;
        float y = 1.5f, z = 2.5f;
        float w;

        System.out.println(x);
        System.out.println(y);
        System.out.println(z);

        w = z = y = x = 0f;

        System.out.println(x);
        System.out.println(y);
        System.out.println(z);
        System.out.println(w);

        x += 13;
        y -= 2;
        z *= 3;
        w /= 4;

        System.out.println(x);
        System.out.println(y);
        System.out.println(z);
        System.out.println(w);

        x++;
        y--;
        ++z;
        --w;

        System.out.println(x);
        System.out.println(y);
        System.out.println(z);
        System.out.println(w);
    }

}