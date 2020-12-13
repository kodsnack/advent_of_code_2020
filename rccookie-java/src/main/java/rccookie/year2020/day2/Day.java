package rccookie.year2020.day2;

import java.util.Scanner;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    /**
     * --- Day 2: Password Philosophy ---
     * <p>Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.
     * <p>The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.
     * <p>Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.
     * <p>To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.
     * <p>For example, suppose you have the following list:
     * <p>1-3 a: abcde
     * <p>1-3 b: cdefg
     * <p>2-9 c: ccccccccc
     * <p>Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
     * <p>In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
     * <p>How many passwords are valid according to their policies?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart1() throws Exception {
        Scanner sc = inputScanner();
        
        int valid = 0;

        while(sc.hasNextLine()) {
            StringBuilder line = new StringBuilder(sc.nextLine());
            
            int min = Integer.parseInt(line.substring(0, line.indexOf("-")));
            int max = Integer.parseInt(line.substring(line.indexOf("-") + 1, line.indexOf(" ")));
            line.delete(0, line.indexOf(" ") + 1);

            char c = line.charAt(0);
            line.delete(0, line.indexOf(" ") + 1);
            
            int count = 0;
            for(int i=0; i<line.length(); i++) {
                if(line.charAt(i) == c) count++;
            }

            if(count >= min && count <= max) valid++;
        }

        return valid;
    }

    /**
     * --- Part Two ---
     * <p>While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.
     * <p>The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.
     * <p>Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
     * <p>Given the same example list from above:
     * <p>1-3 a: abcde is valid: position 1 contains a and position 3 does not.
     * <p>1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
     * <p>2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
     * <p>How many passwords are valid according to the new interpretation of the policies?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart2() throws Exception {
        Scanner sc = inputScanner();

        int valid = 0;

        while(sc.hasNextLine()) {
            StringBuilder line = new StringBuilder(sc.nextLine());
            
            int a = Integer.parseInt(line.substring(0, line.indexOf("-")));
            int b = Integer.parseInt(line.substring(line.indexOf("-") + 1, line.indexOf(" ")));
            line.delete(0, line.indexOf(" ") + 1);

            char c = line.charAt(0);
            line.delete(0, line.indexOf(" ")); // No +1 means first character is automatically '1'
            
            if(line.length() <= a) continue;
            if(line.charAt(a) == c) {
                if(line.length() <= b || line.charAt(b) != c) valid++;
            }
            else if(line.length() > b && line.charAt(b) == c) valid++;
        }

        return valid;
    }
}
