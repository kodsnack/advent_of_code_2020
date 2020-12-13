package rccookie.year2020.day5;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    /**
     * --- Day 5: Binary Boarding ---
     * <p>You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.
     * <p>You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input); perhaps you can find your seat through process of elimination.
     * <p>Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".
     * <p>The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.
     * <p>For example, consider just the first seven characters of FBFBBFFRLR:
     * <p>Start by considering the whole range, rows 0 through 127.
     * <p>F means to take the lower half, keeping rows 0 through 63.
     * <p>B means to take the upper half, keeping rows 32 through 63.
     * <p>F means to take the lower half, keeping rows 32 through 47.
     * <p>B means to take the upper half, keeping rows 40 through 47.
     * <p>B keeps rows 44 through 47.
     * <p>F keeps rows 44 through 45.
     * <p>The final F keeps the lower of the two, row 44.
     * <p>The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.
     * <p>For example, consider just the last 3 characters of FBFBBFFRLR:
     * <p>Start by considering the whole range, columns 0 through 7.
     * <p>R means to take the upper half, keeping columns 4 through 7.
     * <p>L means to take the lower half, keeping columns 4 through 5.
     * <p>The final R keeps the upper of the two, column 5.
     * <p>So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
     * <p>Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.
     * <p>Here are some other boarding passes:
     * <p>BFFFBBFRRR: row 70, column 7, seat ID 567.
     * <p>FFFBBBFRRR: row 14, column 7, seat ID 119.
     * <p>BBFFBBFRLL: row 102, column 4, seat ID 820.
     * <p>As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart1() throws Exception {
        final List<Seat> seats = getSeats();
        int maxId = -1;
        for(Seat seat : seats) if(seat.id > maxId) maxId = seat.id;
        return maxId;
    }

    /**
     * --- Part Two ---
     * <p>Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
     * <p>It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing from your list as well.
     * <p>Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.
     * <p>What is the ID of your seat?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart2() throws Exception {
        final List<Integer> ids = getSeats().stream().map(seat -> seat.id).collect(Collectors.toList());
        ids.sort(null);
        for(int i=ids.get(0); i<ids.get(ids.size() - 1); i++) {
            if(ids.contains(i)) continue;
            return i;
        }
        return -1;
    }


    private List<Seat> getSeats() {
        List<Seat> seats = new ArrayList<>();
        for(String s : inputInLines()) {
            int min = 0, max = 127;
            for(int i=0; min != max; i++) {
                if(s.charAt(i) == 'F') max -= (max - min + 1) / 2;
                else min += (max - min + 1) / 2;
            }
            int row = min;
            min = 0;
            max = 7;
            for(int i=7; min != max; i++) {
                if(s.charAt(i) == 'L') max -= (max - min + 1) / 2;
                else min += (max - min + 1) / 2;
            }
            seats.add(new Seat(row, min));
        }
        return seats;
    }
}
