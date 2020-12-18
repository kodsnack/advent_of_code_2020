package se.trito;

import java.util.*;

public class Dec14b {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        Map<String, Long> mem = new HashMap<>();
        String mask = "000000000000000000000000000000000000";

        for (String input : inputs) {
            if (input.contains("[")) {
                int memPos = Integer.parseInt(input.split(" = ")[0].trim().replaceAll("[a-z]*\\[", "").replaceAll("\\]", ""));
                long number = Long.parseLong(input.split(" = ")[1].trim());
                String memPosBinary = Integer.toBinaryString(memPos);
                String filler = "";
                for (int i = memPosBinary.length(); i < mask.length(); i++)
                    filler = filler + "0";
                memPosBinary = filler + memPosBinary;
                char[] newAddress = new char[mask.length()];
                List<Character> floaties = new ArrayList<>();
                for (int i = 0; i < mask.length(); i++) {
                    switch (mask.charAt(i)) {
                        case '0' -> newAddress[i] = memPosBinary.charAt(i);
                        case '1' -> newAddress[i] = '1';
                        case 'X' -> {
                            newAddress[i] = 'X';
                            floaties.add('X');
                        }
                    }
                }
                List<String> combinations = getAllAddresses(floaties);
                char[] startAddress = new char[newAddress.length];

                System.arraycopy(newAddress, 0, startAddress, 0, newAddress.length);
                for (String combination : combinations) {
                   System.arraycopy(startAddress, 0, newAddress, 0, startAddress.length);
                    int counter = 0;
                    for (int i = 0; i < newAddress.length; i++) {
                        if (newAddress[i] == 'X') {
                            newAddress[i] = combination.charAt(counter);
                            counter++;
                        }
                    }
                    mem.put(String.copyValueOf(newAddress), number);
                }
            } else {
                mask = input.split(" = ")[1].trim();
            }
        }
        long total = mem.values().stream().reduce(0L, Long::sum);

        System.out.println("Dec14b: 3801988250775 - " + total);
    }

    private static List<String> getAllAddresses(List<Character> floaties) {
        double size = floaties.size();
        List<String> combinations = new ArrayList<>();
        for (double i = 0; i < Math.pow(2, size); i++) {
            String comb = Integer.toBinaryString((int) i);
            String temp = "";
            for (int j = comb.length(); j < size; j++) {
                temp = temp + "0";
            }
            comb = temp + comb;
            combinations.add(comb);
        }
        return combinations;
    }
}
