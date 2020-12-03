package dayOne;

import java.io.*;
import java.util.*;

public class TripleSum {

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
        int j = 0;
        boolean exit = false;
        for (i = 0; i < values.size(); i++) {
            for (j = i + 1; j < values.size(); j++) {
                int val = values.get(i) + values.get(j);
                if (val >= 2020) {
                    continue;
                } else {
                    index = Collections.binarySearch(values, (2020 - val));
                    if (index > 0) {
                        exit = true;
                        break;
                    }
                }
            }
            if (exit) {
                break;
            }
        }

        System.out.println(values.get(i) * values.get(j) * values.get(index));
    }
}
