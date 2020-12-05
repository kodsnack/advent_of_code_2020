package se.trito;

import java.util.Objects;

public class Node {

    public int x;
    public int y;
    public char c;

    public Node(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Node(int x, int y, char c) {
        this.x = x;
        this.y = y;
        this.c = c;
    }

    public int getManhattanDistance(Node externNode) {
        return Math.abs(externNode.x - this.x) + Math.abs(externNode.y - this.y);
    }

    public double getHypothenus(Node externNode) {
        int adjacent = Math.abs(externNode.x - this.x);
        int opposite = Math.abs(externNode.y - this.y);
        return Math.sqrt((adjacent*adjacent)+(opposite*opposite));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Node node = (Node) o;
        return x == node.x &&
                y == node.y &&
                c == node.c;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}
