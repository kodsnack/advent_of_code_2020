package se.trito;

import java.util.Arrays;
import java.util.List;

public class Dec02a {

    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        int noOfPasswords = 0;
        for (String input : inputs) {
            List<String> instruction = Arrays.asList(input.split(" "));
            List<String> charRules = Arrays.asList(instruction.get(0).split("-"));
            char rule = instruction.get(1).charAt(0);
            String password = instruction.get(2);
            int innerCounter = 0;
            for (int i = 0; i < password.length(); i++) {
                if (password.charAt(i) == rule) {
                    innerCounter++;
                }
            }
            if (innerCounter >= Integer.parseInt(charRules.get(0)) && innerCounter <= Integer.parseInt(charRules.get(1))) {
                noOfPasswords++;
            }
        }
        System.out.println("Dec02a: " + noOfPasswords);
    }
}
