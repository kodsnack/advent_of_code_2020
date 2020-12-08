package se.trito;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Dec08b {
    public static void run(String fileName) {
        Set<Integer> visitedIndex = new HashSet<>();
        List<List<String>> inputs = FileReaderUtil.splitOnRegexTo2dStringList(fileName, " ");
        int acc = 0;
        for (int k = 0; k < inputs.size(); k++) {
            if (acc!=0)
                break;
            inputs = FileReaderUtil.splitOnRegexTo2dStringList(fileName, " ");
            if (inputs.get(k).get(0).equals("nop")) {
                inputs.get(k).set(0, "jmp");
            } else if (inputs.get(k).get(0).equals("jmp")) {
                inputs.get(k).set(0, "nop");
            } else {
                continue;
            }
            for (int i = 0; i < inputs.size(); i++) {
                if (!visitedIndex.add(i)) {
                    visitedIndex.clear();
                    acc = 0;
                    break;
                }
                String instruction = inputs.get(i).get(0);
                int number = Integer.parseInt(inputs.get(i).get(1));
                switch (instruction) {
                    case "acc":
                        acc += number;
                        break;
                    case "jmp":
                        i += number - 1;
                        break;
                    case "nop":
                        break;
                }
            }
        }
        System.out.println("Dec08b: " + acc);
    }
}