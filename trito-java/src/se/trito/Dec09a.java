package se.trito;

import java.util.List;

public class Dec09a {
    public static void run(String fileName) {
        List<Long> inputs = FileReaderUtil.toLongList(fileName);
        int jump = 25;
        Long wrongNumber = 0L;
        for (int i = jump ; i < inputs.size(); i++) {
            boolean isMatching = false;
            for (int j = i - jump; j < i + jump; j++) {
                for (int k = j + 1; k < i + jump; k++) {
                    if (inputs.get(j) + inputs.get(k) == inputs.get(i)) {
                        isMatching = true;
                        break;
                    }
                }
            }
            if (!isMatching) {
                wrongNumber = inputs.get(i);
                break;
            }
        }
        System.out.println("Dec09a: " + wrongNumber);
    }
}