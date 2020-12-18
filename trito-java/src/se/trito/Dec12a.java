package se.trito;

import java.util.List;

public class Dec12a {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        Node last = new Node(0, 0, 'E');
        for (String input : inputs) {
            char c = input.charAt(0);
            int steps = Integer.parseInt(input.substring(1));
            switch (c) {
                case ('N') -> last.y -= steps;
                case ('S') -> last.y += steps;
                case ('E') -> last.x += steps;
                case ('W') -> last.x -= steps;
                case ('L') -> last.c = getNewDirection(last.c, steps, -1);
                case ('R') -> last.c = getNewDirection(last.c, steps, 1);
                case ('F') -> {
                    if (last.c == 'N')
                        last.y -= steps;
                    else if (last.c == 'W')
                        last.x -= steps;
                    else if (last.c == 'S')
                        last.y += steps;
                    else
                        last.x += steps;
                }
            }
        }
        System.out.println("Dec12a: " + last.getManhattanDistance(new Node(0, 0)));
    }

    private static char getNewDirection(char c, int steps, int rotation) {
        char dir = c;
        switch (c) {
            case ('N') -> {
                if ((steps == 90 && rotation == 1) || (steps == 270 && rotation == -1))
                    dir = 'E';
                else if ((steps == 270 && rotation == 1) || (steps == 90 && rotation == -1))
                    dir = 'W';
                else if (steps == 180)
                    dir = 'S';
            }
            case ('E') -> {
                if ((steps == 90 && rotation == 1) || (steps == 270 && rotation == -1))
                    dir = 'S';
                else if ((steps == 270 && rotation == 1) || (steps == 90 && rotation == -1))
                    dir = 'N';
                else if (steps == 180)
                    dir = 'W';
            }
            case ('S') -> {
                if ((steps == 90 && rotation == 1) || (steps == 270 && rotation == -1))
                    dir = 'W';
                else if ((steps == 270 && rotation == 1) || (steps == 90 && rotation == -1))
                    dir = 'E';
                else if (steps == 180)
                    dir = 'N';
            }
            case ('W') -> {
                if ((steps == 90 && rotation == 1) || (steps == 270 && rotation == -1))
                    dir = 'N';
                else if ((steps == 270 && rotation == 1) || (steps == 90 && rotation == -1))
                    dir = 'S';
                else if (steps == 180)
                    dir = 'E';
            }
        }
        return dir;
    }
}

