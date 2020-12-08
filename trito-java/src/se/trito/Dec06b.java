package se.trito;

import java.util.*;

public class Dec06b {
    public static void run(String fileName) {
        Map<Character, Integer> yesAnswers = new HashMap<>();
        int total = 0;
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        int personsInGroup = 0;
        for (String line : inputs) {
            if (line.isEmpty()) {
                if (personsInGroup == 1) {
                    total += yesAnswers.size();
                } else {
                    for (Map.Entry<Character, Integer> entry : yesAnswers.entrySet()) {
                       if (entry.getValue() == personsInGroup) {
                           total++;
                       }
                    }
                }
                yesAnswers = new HashMap<>();
                personsInGroup = 0;
            } else {
                for (Character c : line.toCharArray()) {
                    int value = yesAnswers.getOrDefault(c, 0);
                    yesAnswers.put(c, value + 1);
                }
                personsInGroup++;
            }
        }
        System.out.println("Dec06b: " + total);
    }
}
