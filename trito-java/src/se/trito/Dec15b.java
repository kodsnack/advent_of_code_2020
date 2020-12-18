package se.trito;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Dec15b {
    public static void run(String fileName) {
        List<Integer> input = FileReaderUtil.splitOnRegexToIntList(fileName, "\\s*,\\s*");
        Map<Integer, List<Integer>> map = new HashMap<>();

        for (int i = 0; i < input.size(); i++) {
            List<Integer> positions = new ArrayList<>();
            positions.add(i + 1);
            map.put(input.get(i), positions);
        }

        int lastSpoken = input.get(input.size() - 1);

        for (int turn = input.size() + 1; turn <= 30000000; turn++) {

            if (map.containsKey(lastSpoken)) {
                if (map.get(lastSpoken).size() == 1) {
                    lastSpoken = 0;
                    map.get(0).add(turn);
                } else {
                    lastSpoken = map.get(lastSpoken).get(map.get(lastSpoken).size() - 1) - map.get(lastSpoken).get(map.get(lastSpoken).size() - 2);
                    if (map.containsKey(lastSpoken)) {
                        map.get(lastSpoken).add(turn);
                    } else {
                        List<Integer> positions = new ArrayList<>();
                        positions.add(turn);
                        map.put(lastSpoken, positions);
                    }
                }
            } else {
                lastSpoken = 0;
                map.get(0).add(turn);
            }
        }
        System.out.println("Dec15b: " + lastSpoken);
    }
}
