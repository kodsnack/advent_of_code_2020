package se.trito;

import java.util.*;

public class Dec16a {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        boolean hasRules = false;
        boolean hasTicket = false;
        Map<String, List<Integer>> rules = new HashMap<>();
        Set<Integer> allowedValues = new HashSet<>();
        List<Integer> myTicket = new ArrayList<>();
        List<List<Integer>> nearbyTickets = new ArrayList<>();
        int totErrors = 0;
        for (String row : inputs) {
            if (row.isEmpty())
                if (!hasRules)
                    hasRules = true;
                else
                    hasTicket = true;
            else
                if (!hasRules) {
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
            for (int i : ticket) {
                if (!allowedValues.contains(i)) {
                    totErrors += i;
                }
            }
        }
        System.out.println("Dec16a: " + totErrors);
    }

    private static List<Integer> getTicket(String row) {
        List<Integer> ticket = new ArrayList<>();
        String[] mt = row.split(",");
        for (String m : mt)
            ticket.add(Integer.parseInt(m));
        return ticket;
    }
}
