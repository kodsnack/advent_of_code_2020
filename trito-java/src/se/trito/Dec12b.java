package se.trito;

import java.util.List;

public class Dec12b {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        Node last = new Node(0, 0);
        Node wayPoint = new Node(10, -1);
        for (String input : inputs) {
            char c = input.charAt(0);
            int steps = Integer.parseInt(input.substring(1));
            switch (c) {
                case ('N') -> wayPoint.y -= steps;
                case ('S') -> wayPoint.y += steps;
                case ('E') -> wayPoint.x += steps;
                case ('W') -> wayPoint.x -= steps;
                case ('L') -> wayPoint = getNewPos(wayPoint, steps, -1);
                case ('R') -> wayPoint = getNewPos(wayPoint, steps, 1);
                case ('F') -> {
                    int x = wayPoint.x * steps + last.x;
                    int y = wayPoint.y * steps + last.y;
                    last.x = x;
                    last.y = y;
                }
            }
        }
        System.out.println("Dec12b: " + last.getManhattanDistance(new Node(0, 0)));
    }

    private static Node getNewPos(Node old, int steps, int rotation) {
        Node node = new Node(old.x, old.y);
        switch (steps) {
            case (90) -> {
                if (rotation == 1) {
                    node.x = old.y * -1;
                    node.y = old.x;
                } else {
                    node.x = old.y;
                    node.y = old.x * -1;
                }
            }
            case (180) -> {
                node.x = old.x * -1;
                node.y = old.y * -1;
            }
            case (270) -> {
                if (rotation == 1) {
                    node.x = old.y;
                    node.y = old.x * -1;
                } else {
                    node.x = old.y * -1;
                    node.y = old.x;
                }
            }
        }
        return node;
    }
}
