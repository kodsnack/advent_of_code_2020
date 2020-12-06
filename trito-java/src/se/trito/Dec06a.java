package se.trito;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Dec06a {
    public static void run(String fileName) {
        Set<Character> yes = new HashSet<>();
        int total = 0;
            List<String> inputs = FileReaderUtil.toStringList(fileName);
            for (String line : inputs) {
                if (line.isEmpty()) {
                    total += yes.size();
                    yes = new HashSet<>();
                } else {
                    for (Character c : line.toCharArray()) {
                        yes.add(c);
                    }
                }
            }
        System.out.println("Dec06a: " + total);
    }
}
