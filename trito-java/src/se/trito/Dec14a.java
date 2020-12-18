package se.trito;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Dec14a {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        Map<Integer, String> mem = new HashMap<>();
        String mask ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

        for (String input : inputs) {
            if (input.contains("[")) {
                Integer memPos = Integer.parseInt(input.split(" = ")[0].trim().replaceAll("[a-z]*\\[", "").replaceAll("\\]", ""));
                String number = Integer.toBinaryString(Integer.parseInt(input.split(" = ")[1].trim()));
                String filler = "";
                for (int j = number.length(); j < mask.length(); j++) {
                    filler = filler + "0";
                }
                number = filler + number;
                char[] charArray = new char [mask.length()];
                for (int i = mask.length() -1; i >= 0; i--) {
                    char maskChar = mask.charAt(i);
                        char num = number.charAt(i);
                        if (maskChar == 'X') {
                            charArray[i] = num;
                        } else {
                            charArray[i] = maskChar;
                        }
                }
                mem.put(memPos, String.valueOf(charArray));
            } else {
                mask = input.split(" = ")[1].trim();
            }
        }

        long total = mem.values().stream().map(n -> Long.parseLong(n, 2)).reduce(0L, Long::sum);

        System.out.println("Dec14a: " + total);
    }
}
