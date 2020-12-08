package se.trito;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Dec08a {
    public static void run(String fileName) {
        Set<Integer> visitedIndex = new HashSet<>();
        List<List<String>> inputs = FileReaderUtil.splitOnRegexTo2dStringList(fileName, " ");
        int acc = 0;
        for (int i = 0; i < inputs.size(); i++) {
            if (!visitedIndex.add(i)) {
                break;
            }
            String instruction = inputs.get(i).get(0);
            int number = Integer.parseInt(inputs.get(i).get(1));
            switch (instruction) {
                case "acc":
                    acc += number;
                    break;
                case "jmp":
                    i = i - 1 + number;
                        break;
                case "nop":
                    break;
            }
        }
        System.out.println("Dec08a: " + acc);
    }
}
