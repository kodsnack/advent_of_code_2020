package se.trito;

import java.util.*;
import java.util.stream.Collectors;

public class Dec16b {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        boolean hasRules = false;
        boolean hasTicket = false;
        Map<String, List<Integer>> rules = new HashMap<>();
        Set<Integer> allowedValues = new HashSet<>();
        List<Integer> myTicket = new ArrayList<>();
        List<List<Integer>> nearbyTickets = new ArrayList<>();
        List<List<Integer>> validNearbyTickets = new ArrayList<>();
        Map<String, List<Integer>> posHeaderPos = new HashMap<>();

        for (String row : inputs) {
            if (row.isEmpty())
                if (!hasRules)
                    hasRules = true;
                else
                    hasTicket = true;
            else if (!hasRules) {
                String rule = row.split(":")[0].trim();
                List<Integer> numbs = new ArrayList<>();
                int seq1 = Integer.parseInt(row.split(":")[1].trim().split("or")[0].trim().split("-")[0]);
                int seq2 = Integer.parseInt(row.split(":")[1].trim().split("or")[0].trim().split("-")[1]);
                int seq3 = Integer.parseInt(row.split(":")[1].trim().split("or")[1].trim().split("-")[0]);
                int seq4 = Integer.parseInt(row.split(":")[1].trim().split("or")[1].trim().split("-")[1]);
                for (int i = seq1; i <= seq2; i++) {
                    numbs.add(i);
                    allowedValues.add(i);
                }
                for (int i = seq3; i <= seq4; i++) {
                    numbs.add(i);
                    allowedValues.add(i);
                }
                rules.put(rule, numbs);
                posHeaderPos.put(rule, new ArrayList<>());
            } else if (!hasTicket) {
                if (!"your ticket:".equals(row)) {
                    myTicket = getTicket(row);
                }
            } else {
                if (!"nearby tickets:".equals(row)) {
                    List<Integer> ticket = getTicket(row);
                    nearbyTickets.add(ticket);
                }
            }
        }

        for (List<Integer> ticket : nearbyTickets) {
            boolean isValid = true;
            for (int i : ticket) {
                if (!allowedValues.contains(i)) {
                    isValid = false;
                    break;
                }
            }
            if (isValid)
                validNearbyTickets.add(ticket);
        }

        for (Map.Entry<String, List<Integer>> entry : rules.entrySet()) {
            List<Integer> validPositions = entry.getValue();
            for (int i = 0; i < myTicket.size(); i++) {
                boolean isValidPos = true;
                for (List<Integer> ticket : validNearbyTickets) {
                    if (!validPositions.contains(ticket.get(i))) {
                        isValidPos = false;
                        break;
                    }
                }
                if (isValidPos) {
                    posHeaderPos.get(entry.getKey()).add(i);
                }
            }
        }
        String[] headerPositions = new String[myTicket.size()];
        for (Map.Entry<String, List<Integer>> entry : posHeaderPos.entrySet()) {
            if (entry.getValue().size() == 1) {
                headerPositions[entry.getValue().get(0)] = entry.getKey();
            }
        }

        int numberOfHeaders = 0;
        while (numberOfHeaders != headerPositions.length) {
            int index = findOne(posHeaderPos, headerPositions);
            numberOfHeaders = countFoundHeaders(headerPositions, numberOfHeaders);
            removeIndex(posHeaderPos, index);
        }

        long total = 1L;
        for (int i = 0; i < headerPositions.length; i++) {
            if ("dep".equals(headerPositions[i].substring(0, 3))) {
                total = total * (long) myTicket.get(i);
            }
        }

        System.out.println("Dec16b: " + total);
    }

    private static void removeIndex(Map<String, List<Integer>> posHeaderPos, int index) {
        for (Map.Entry<String, List<Integer>> entry : posHeaderPos.entrySet()) {
            List<Integer> indexes = entry.getValue().stream().filter(n -> n != index).collect(Collectors.toList());
            posHeaderPos.put(entry.getKey(), indexes);
        }
    }

    private static int countFoundHeaders(String[] headerPositions, int headersSet) {
        for (String header : headerPositions) {
            if (header != null) {
                headersSet++;
            } else {
                headersSet = 0;
                break;
            }
        }
        return headersSet;
    }

    private static int findOne(Map<String, List<Integer>> posHeaderPos, String[] headerPositions) {
        int index = 0;
        for (Map.Entry<String, List<Integer>> entry : posHeaderPos.entrySet()) {
            if (entry.getValue().size() == 1) {
                headerPositions[entry.getValue().get(0)] = entry.getKey();
                index = entry.getValue().get(0);
            }
        }
        return index;
    }

    private static List<Integer> getTicket(String row) {
        List<Integer> ticket = new ArrayList<>();
        String[] mt = row.split(",");
        for (String m : mt)
            ticket.add(Integer.parseInt(m));
        return ticket;
    }
}
