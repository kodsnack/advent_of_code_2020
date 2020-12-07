package se.trito;

import java.util.List;

public class Dec01b {
    public static void run(String fileName) {
        List<Integer> inputs = FileReaderUtil.toIntList(fileName);

        for (int i = 0; i < inputs.size(); i++) {
            for (int j = i; j < inputs.size(); j++) {
                for (int k = j; k < inputs.size(); k++) {
                    if (inputs.get(i) + inputs.get(j) + inputs.get(k) == 2020) {
                        System.out.println("Dec01b: " +inputs.get(i) * inputs.get(j) * inputs.get(k));
                    }
                }
            }
        }
    }
}
