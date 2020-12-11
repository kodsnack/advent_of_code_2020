package se.trito;

import java.util.Collections;
import java.util.List;

public class Dec10a {
    public static void run(String fileName) {
        List<Integer> inputs = FileReaderUtil.toIntList(fileName);
        int counter1 = 1;
        int counter3 = 1;
        Collections.sort(inputs);
        for (int i = 0; i < inputs.size() -1; i++) {
            if (inputs.get(i) == inputs.get(i + 1) - 1) {
                counter1++;
            } else if (inputs.get(i) == inputs.get(i + 1) - 3) {
                counter3++;
            }
        }
        System.out.println("Dec10a: " + counter1 + "*" + counter3 + " = " +  counter1 * counter3);
    }
}
