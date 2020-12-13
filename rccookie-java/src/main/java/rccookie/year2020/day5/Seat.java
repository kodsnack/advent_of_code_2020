package rccookie.year2020.day5;

public class Seat {
    public final int row, column, id;

    public Seat(int row, int column) {
        this.row = row;
        this.column = column;
        id = 8 * row + column;
    }

    @Override
    public String toString() {
        return "[" + row + "/" + column + "(" + id + ")]";
    }
}
