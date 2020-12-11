package se.trito;

import java.util.ArrayList;
import java.util.List;

public class Dec11b {
    private static final int Y_COUNT = 90;
    private static final int X_COUNT = 98;

    public static void run(String fileName) {
        List<String> inputs = FileReaderUtil.toStringList(fileName);
        List<Node> seats = new ArrayList<>();
        int counter = 0;

        fillSeats(inputs, seats);

        while(true){
            List<Node> oldSeats = deepCopy(seats);
            for(Node node : oldSeats) {
                if (node.c == 'L') {
                    int noOfOccupiedSeats = noOfAdjacentSeatsOccupied(oldSeats, node);
                    if (noOfOccupiedSeats == 0) {
                        int index = oldSeats.indexOf(node);
                        seats.get(index).c ='#';
                    }
                } else if (node.c == '#') {
                    int noOfOccupiedSeats = noOfAdjacentSeatsOccupied(oldSeats, node);
                    if (noOfOccupiedSeats >= 5) {
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
        System.out.println("Dec11b: " + counter);
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

    private static Node findSeat(Node node, int dx, int dy, List<Node> seats) {
        Node nodeOccupied = new Node(node.x + dx, node.y + dy, '#');
        Node nodeFree = new Node(node.x + dx, node.y + dy, 'L');
        if (nodeOccupied.x < 0 || nodeOccupied.x > X_COUNT || nodeOccupied.y < 0 || nodeOccupied.y > Y_COUNT ||
                nodeFree.x < 0 || nodeFree.x > X_COUNT || nodeFree.y < 0 || nodeFree.y > Y_COUNT){
            return new Node(0,0);
        }
        if (seats.contains(nodeOccupied)) {
            return new Node(node.x + dx, node.y + dy, '#');
        } else if (seats.contains(nodeFree)) {
            return new Node(node.x + dx, node.y + dy, 'L');
        }
        return findSeat(new Node(node.x + dx, node.y + dy, '.'), dx, dy, seats);
    }

    private static int noOfAdjacentSeatsOccupied(List<Node> seats, Node node) {
        int counter = 0;
        if(findSeat(node, -1, 0, seats).c == '#'){
            counter++;
        }
        if (findSeat(node, -1, -1, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, 0, -1, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, 1, -1, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, 1, 0, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, 1, 1, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, 0, 1, seats).c == '#') {
            counter++;
        }
        if (findSeat(node, -1, 1, seats).c == '#') {
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
