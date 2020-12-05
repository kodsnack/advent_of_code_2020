package se.trito;

import java.util.*;

public class Dec05b {
    public static void run(String fileName) {

        List<Integer> takenSeatsId = new ArrayList<>();
        int result = 0;
        List<String> ids = FileReaderUtil.toStringList(fileName);
        for (String id : ids) {
            int row = getNumber(id, 0, 128);
            int column = getNumber(id.substring(7), 0, 8);
            takenSeatsId.add(getSeatId(row, column));
        }

        for (int i = 0; i < 8; i++) {
            for (int j = 1; j < 127; j++) {
                int id = getSeatId(j, i);
                if (!takenSeatsId.contains(id) && (takenSeatsId.contains(id + 1) && takenSeatsId.contains(id - 1))) {
                    result = id;
                }
            }
        }

        System.out.println("Dec05b: " + result);
    }

    private static int getSeatId(int row, int column) {
        return (row * 8) + column;
    }

    public static int getNumber(String input, int lower, int higher) {
        switch (input.charAt(0)) {
            case 'F', 'L' -> higher = (higher + lower) / 2;
            case 'B', 'R' -> lower = (higher + lower) / 2;
        }
        if (higher != lower + 1)
            return getNumber(input.substring(1), lower, higher);
        return lower;
    }

}
