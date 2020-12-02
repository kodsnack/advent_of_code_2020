package dayOne;

import java.io.*;
import java.util.*;

public class Sum2020 {

    private static ArrayList<Integer> values = new ArrayList<>();

    public static void main(String[] args) {

        Scanner fileReader = null;
        try {
            fileReader = new Scanner(new File("src/dayOne/input"));
        } catch (FileNotFoundException ex) {
            System.out.println("File not found.");
            System.exit(-1);
        }

        while (fileReader.hasNextInt()) {
            values.add(fileReader.nextInt());
        }

        values.sort(Integer::compareTo);

        int index = -1;
        int i;
        for (i = 0; i < values.size(); i++) {
            index = Collections.binarySearch(values, (2020 - values.get(i)));
            if (index > 0) {
                break;
            }
        }

        System.out.println(values.get(index) * values.get(i));
    }
}
