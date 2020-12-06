package se.trito;

import java.util.*;

public class Dec04b {
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
        System.out.println("Dec04b: " + validCounter);
    }

    private static boolean isValid(Map<String, String> passport) {
        if (passport.containsKey("ecl") && passport.containsKey("pid") && passport.containsKey("eyr") && passport.containsKey("hcl") && passport.containsKey("byr") && passport.containsKey("iyr") && passport.containsKey("hgt")) {
            if (!(Integer.parseInt(passport.get("byr")) >= 1920 && Integer.parseInt(passport.get("byr")) <= 2002)) {
                return false;
            }
            if (!(Integer.parseInt(passport.get("iyr")) >= 2010 && Integer.parseInt(passport.get("iyr")) <= 2020))
                return false;
            if (!(Integer.parseInt(passport.get("eyr")) >= 2020 && Integer.parseInt(passport.get("eyr")) <= 2030))
                return false;
            if (passport.get("hgt").replaceAll("\\D", "").isEmpty()) {
                return false;
            } else {
                if (passport.get("hgt").contains("cm")) {
                    String pass = passport.get("hgt").replaceAll("\\D", "");
                    if (!(Integer.parseInt(pass) >= 150 && Integer.parseInt(pass) <= 193)) {
                        return false;
                    }
                } else if (passport.get("hgt").contains("in")) {
                    String pass = passport.get("hgt").replaceAll("\\D", "");
                    if (!(Integer.parseInt(pass) >= 59 && Integer.parseInt(pass) <= 76)) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            String hairColor = passport.get("hcl");

            if (hairColor.chars().filter(ch -> ch == '#').count() != 1) {
                return false;
            } else {
                String rgb = hairColor.replaceAll("[^a-f^0-9]", "");
                if (rgb.trim().length() != 6) {
                    return false;
                }
            }
            if (passport.get("pid").replaceAll("\\D", "").length() != 9) {
                return false;
            }
            return (passport.get("ecl")).equals("amb") || (passport.get("ecl")).equals("blu") || (passport.get("ecl")).equals("brn") || (passport.get("ecl")).equals("grn")
                    || (passport.get("ecl")).equals("gry") || (passport.get("ecl")).equals("hzl") || (passport.get("ecl")).equals("oth");
        }
        return false;
    }
}
