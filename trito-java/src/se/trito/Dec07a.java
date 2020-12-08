package se.trito;

import java.util.*;
import java.util.stream.Collectors;

public class Dec07a {
    public static void run(String fileName) {
        int counter = 0;
        Queue<String> queue = new LinkedList<>();
        List<String> rules = FileReaderUtil.toStringList(fileName);
        Map<String, List<String>> bagMap = new HashMap<>();
        rules = rules.stream()
                .map(s -> s.replaceAll(" bags", "bag"))
                .map(s -> s.replaceAll("bag", ""))
                .map(s -> s.replaceAll("\\.", ""))
                .map(s -> s.replaceAll("\\d", ""))
                .collect(Collectors.toList());

        for (String rule : rules) {
            String bag = getFirstBag(rule).trim();
            List<String> innerBags = getInnerBags(rule);
            List<String> bagArray = bagMap.getOrDefault(bag, new ArrayList<>());
            bagArray.addAll(innerBags);
            bagMap.put(bag, bagArray);
        }

        for (String rule : rules) {
            queue.clear();
            List<String> innerBags = getInnerBags(rule);
            queue.addAll(innerBags);
            while (!queue.isEmpty()) {
                String first = queue.remove();
                if (first.equals("shiny gold")) {
                    counter++;
                    break;
                }
                if (!first.equals("no other")) {
                    List<String> nestedBags = bagMap.get(first);
                    queue.addAll(nestedBags);
                }
            }
        }
        System.out.println("Dec07a: " + counter);
    }


    private static List<String> getInnerBags(String rule) {
        return Arrays.asList(rule.split("contain")[1].trim().split("\\s*,\\s*"));
    }

    private static String getFirstBag(String rule) {
        return rule.split("contain")[0].trim();
    }
}
