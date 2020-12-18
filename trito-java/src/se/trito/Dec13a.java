package se.trito;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Dec13a {
    public static void run(String fileName) {
        List<List<String>> inputs = FileReaderUtil.splitOnRegexTo2dStringList(fileName, "\\s*,\\s");
        int time = Integer.parseInt(inputs.get(0).get(0));
        int total = 0;

        List<Integer> busses = new ArrayList<>();
        String[] temp = inputs.get(1).get(0).split(",");
        for (String bus : temp) {
            if (!bus.equals("x")) {
                busses.add(Integer.parseInt(bus));
            }
        }
        int lowestWaitTime = Integer.MAX_VALUE;

        for (int busTime : busses) {
            int value = time % busTime;
            int departureTime = -value + time + busTime;

            if (departureTime - time < lowestWaitTime) {
                lowestWaitTime = departureTime - time;
                total = lowestWaitTime * busTime;
            }
        }

        System.out.println("Dec13a: 296 - " + total);
    }
}
