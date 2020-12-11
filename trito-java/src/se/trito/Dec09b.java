package se.trito;

import java.util.Collections;
import java.util.List;

public class Dec09b {
    public static void run(String fileName) {
        List<Long> inputs = FileReaderUtil.toLongList(fileName);
        int jump = 25;
        long wrongNumber = 217430975L;
        long total = 0L;

        boolean found = false;
        for (int i = 0; i < inputs.size(); i++) {
            for (int j = i; j < jump + i; j++) {
                for (int k = j + 1; k < jump + i; k++) {
                    List<Long> subList = inputs.subList(j, k);
                    long sum = subList.stream()
                            .mapToLong(Long::longValue)
                            .sum();
                    if (sum == wrongNumber) {
                        found = true;
                        Collections.sort(subList);
                        total = subList.get(0) + subList.get(subList.size() - 1);
                        break;
                    }
                }
                if (found)
                    break;
            }
            if (found)
                break;
        }
        System.out.println("Dec09b: " + total);
    }
}
