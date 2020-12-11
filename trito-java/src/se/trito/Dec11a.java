package se.trito;

import java.util.ArrayList;
import java.util.List;

public class Dec11a {
    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        List<Node> seats = new ArrayList<>();
        int counter = 0;

        fillSeats(inputs, seats);

        while (true) {
            List<Node> oldSeats = deepCopy(seats);
            for (Node node : oldSeats) {
                if (node.c == 'L') {
                    int noOfOccupiedSeats = noOfAdjacentSeatsOccupied(oldSeats, node);
                    if (noOfOccupiedSeats == 0) {
                        int index = oldSeats.indexOf(node);
                        seats.get(index).c = '#';
                    }
                } else if (node.c == '#') {
                    int noOfOccupiedSeats = noOfAdjacentSeatsOccupied(oldSeats, node);
                    if (noOfOccupiedSeats >= 4) {
                        int index = oldSeats.indexOf(node);
                        seats.get(index).c = 'L';
                    }
                }
            }
            if (oldSeats.equals(seats)) {
                counter = countOccupied(seats);
                break;
            }
        }
        System.out.println("Dec11a: " + counter);
    }

    private static int countOccupied(List<Node> seats) {
        int counter = 0;
        for (Node n : seats) {
            if (n.c == '#') {
                counter++;
            }
        }
        return counter;
    }

    private static int noOfAdjacentSeatsOccupied(List<Node> seats, Node node) {
        int counter = 0;
        if (seats.contains(new Node(node.x - 1, node.y, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x - 1, node.y - 1, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x, node.y - 1, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x + 1, node.y - 1, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x + 1, node.y, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x + 1, node.y + 1, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x, node.y + 1, '#'))) {
            counter++;
        }
        if (seats.contains(new Node(node.x - 1, node.y + 1, '#'))) {
            counter++;
        }
        return counter;
    }

    private static List<Node> deepCopy(List<Node> old) {
        List<Node> clone = new ArrayList<>();
        for (Node n : old) {
            clone.add(new Node(n.x, n.y, n.c));
        }
        return clone;
    }

    private static void fillSeats(List<String> inputs, List<Node> seats) {
        for (int i = 0; i < inputs.size(); i++) {
            for (int j = 0; j < inputs.get(i).length(); j++) {
                Node seat = new Node(j, i, inputs.get(i).charAt(j));
                seats.add(seat);
            }
        }
    }
}
