package se.trito;

import java.util.*;

public class Dec04a {

    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        int validCounter = 0;
        Map<String, String> passport = new HashMap<>();
        for (String input : inputs) {
            if (input.isEmpty()) {
                if (isValid(passport)) {
                    validCounter++;
                }
                passport = new HashMap<>();
            } else {
                String[] splitOnSpace = input.split("\\s");
                for (String splits : splitOnSpace) {
                    List<String> split = Arrays.asList(splits.split(":"));
                    passport.put(split.get(0), split.get(1));
                }
            }
        }
        System.out.println("Dec04a: " + validCounter);
    }

    private static boolean isValid(Map<String, String> passport) {
        return passport.containsKey("ecl") && passport.containsKey("pid") && passport.containsKey("eyr") && passport.containsKey("hcl") && passport.containsKey("byr") && passport.containsKey("iyr") && passport.containsKey("hgt");
    }
}

