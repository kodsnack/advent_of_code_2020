package se.trito;

import java.util.Arrays;
import java.util.List;

public class Dec02b {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        int counter = 0;
        for (String input : inputs) {
            List<String> instruction = Arrays.asList(input.split(" "));
            List<String> charRules = Arrays.asList(instruction.get(0).split("-"));

            char rule = instruction.get(1).charAt(0);
            String password = instruction.get(2);
            int charCounter = 0;

            if (password.charAt(Integer.parseInt(charRules.get(0)) - 1) == rule) {
                charCounter++;
            }
            if (Integer.parseInt(charRules.get(0)) != Integer.parseInt(charRules.get(1))
                    & password.charAt(Integer.parseInt(charRules.get(1)) - 1) == rule) {
                charCounter++;
            }

            if (charCounter == 1) {
                counter++;
            }
        }
        System.out.println("Dec02b: " + counter);
    }
}
