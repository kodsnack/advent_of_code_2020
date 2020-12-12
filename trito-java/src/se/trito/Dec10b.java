package se.trito;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Dec10b {
    public static void run(String fileName) {
        List<Integer> inputs = FileReaderUtil.toIntList(fileName);
        long total = 1L;
        Collections.sort(inputs);
        System.out.println(Arrays.toString(inputs.toArray()));
        List<Integer> serie = new ArrayList<>();
        for (int i = 0; i < inputs.size(); i++) {
            int value = inputs.get(i);
            if (serie.isEmpty()) {
                serie.add(value);
            } else {
                if (value - 1 == serie.get(serie.size() -1)) {
                    serie.add(value);
                } else {
                    switch (serie.size()) {
                        case 3 -> total = total * 2;
                        case 4 -> total = total * 4;
                        case 5 -> total = total * 7;
                    }
                    serie.clear();
                    serie.add(value);
                }
            }
        }
        System.out.println("Dec10b: " + total);
    }
}
