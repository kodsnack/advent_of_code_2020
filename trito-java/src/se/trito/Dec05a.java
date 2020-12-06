package se.trito;

import java.util.List;

public class Dec05a {
    public static void run(String fileName) {
        int highest = 0;
        List<String> ids = FileReaderUtil.toStringList(fileName);
        for (String id : ids) {
            int row = getNumber(id, 0, 128);
            int column = getNumber(id.substring(7),0,8);
            int seatId = (row * 8) + column;
            if (seatId > highest)
                highest = seatId;
        }
        System.out.println("Dec05a: " + highest);
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
