package se.trito;

import java.util.*;
import java.util.stream.Collectors;

public class Dec07b {
    public static void run(String fileName) {
        int counter = 0;
        List<String> rules = FileReaderUtil.toStringList(fileName);
        Map<String, List<BagData>> bagMap = new HashMap<>();
        rules = rules.stream()
                .map(s -> s.replaceAll(" bags", "bag"))
                .map(s -> s.replaceAll("bag", ""))
                .map(s -> s.replaceAll("\\.", ""))
                .collect(Collectors.toList());

        for (String rule : rules) {
            String bagName = getBagName(rule.trim());
            List<BagData> bgList = getBagData(rule.trim());
            bagMap.put(bagName, bgList);
        }

        counter += countBags("shiny gold", bagMap);

        System.out.println("Dec07b2: " + counter);
    }

    private static int countBags(String bag, Map<String, List<BagData>> bagMap) {
        List<BagData> bgList = bagMap.get(bag);
        int counter = 0;
        for (BagData bg : bgList) {
            counter += bg.number + (bg.number * countBags(bg.name, bagMap));
        }
        return counter;
    }

    private static String getBagName(String rule) {
        String[] splittedOnContains = rule.split("contain");
        return splittedOnContains[0].trim();
    }

    private static List<BagData> getBagData(String rule) {
        String[] splittedOnContains = rule.split("contain")[1].trim().split("\\s*,\\s*");
        List<BagData> bgList = new ArrayList<>();
        for (String b : splittedOnContains) {
            if (!"no other".equals(b.trim())) {
                int number = Integer.parseInt(b.split(" ")[0].trim());
                String value = b.replaceFirst(String.valueOf(number), "").trim();
                bgList.add(new BagData(value, number));
            }
        }
        return bgList;
    }
}
