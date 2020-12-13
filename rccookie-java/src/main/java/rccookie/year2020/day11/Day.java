package rccookie.year2020.day11;

import java.util.Arrays;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    public static final char FLOOR = '.', FREE = 'L', OCCUPIED = '#';

    private final String[] input;
    private final int width, height;

    {
        input = inputInLines();
        height = input != null ? input.length : 0;
        width = height != 0 ? input[0].length() : 0;
    }

    /**
     * <h2>--- Day 11: Seating System ---</h2><p>Your plane lands with plenty of time to spare. The final leg of your journey is a ferry that goes directly to the tropical island where you can finally start your vacation. As you reach the waiting area to board the ferry, you realize you're so early, nobody else has even arrived yet!</p>
     * <p>By modeling the process people use to choose (or abandon) their seat in the waiting area, you're pretty sure you can predict the best place to sit. You make a quick map of the seat layout (your puzzle input).</p>
     * <p>The seat layout fits neatly on a grid. Each position is either floor (<code>.</code>), an empty seat (<code>L</code>), or an occupied seat (<code>#</code>). For example, the initial seat layout might look like this:</p>
     * <pre><code>L.LL.LL.LL
     * LLLLLLL.LL
     * L.L.L..L..
     * LLLL.LL.LL
     * L.LL.LL.LL
     * L.LLLLL.LL
     * ..L.L.....
     * LLLLLLLLLL
     * L.LLLLLL.L
     * L.LLLLL.LL
     * </code></pre>
     * <p>Now, you just need to model the people who will be arriving shortly. Fortunately, people are entirely predictable and always follow a simple set of rules. All decisions are based on the <em>number of occupied seats</em> adjacent to a given seat (one of the eight positions immediately up, down, left, right, or diagonal from the seat). The following rules are applied to every seat simultaneously:</p>
     * <ul>
     * <li>If a seat is <em>empty</em> (<code>L</code>) and there are <em>no</em> occupied seats adjacent to it, the seat becomes <em>occupied</em>.</li>
     * <li>If a seat is <em>occupied</em> (<code>#</code>) and <em>four or more</em> seats adjacent to it are also occupied, the seat becomes <em>empty</em>.</li>
     * <li>Otherwise, the seat's state does not change.</li>
     * </ul>
     * <p><span title="Floor... floor never changes.">Floor (<code>.</code>) never changes</span>; seats don't move, and nobody sits on the floor.</p>
     * <p>After one round of these rules, every seat in the example layout becomes occupied:</p>
     * <pre><code>#.##.##.##
     * #######.##
     * #.#.#..#..
     * ####.##.##
     * #.##.##.##
     * #.#####.##
     * ..#.#.....
     * ##########
     * #.######.#
     * #.#####.##
     * </code></pre>
     * <p>After a second round, the seats with four or more occupied adjacent seats become empty again:</p>
     * <pre><code>#.LL.L#.##
     * #LLLLLL.L#
     * L.L.L..L..
     * #LLL.LL.L#
     * #.LL.LL.LL
     * #.LLLL#.##
     * ..L.L.....
     * #LLLLLLLL#
     * #.LLLLLL.L
     * #.#LLLL.##
     * </code></pre>
     * <p>This process continues for three more rounds:</p>
     * <pre><code>#.##.L#.##
     * #L###LL.L#
     * L.#.#..#..
     * #L##.##.L#
     * #.##.LL.LL
     * #.###L#.##
     * ..#.#.....
     * #L######L#
     * #.LL###L.L
     * #.#L###.##
     * </code></pre>
     * <pre><code>#.#L.L#.##
     * #LLL#LL.L#
     * L.L.L..#..
     * #LLL.##.L#
     * #.LL.LL.LL
     * #.LL#L#.##
     * ..L.L.....
     * #L#LLLL#L#
     * #.LLLLLL.L
     * #.#L#L#.##
     * </code></pre>
     * <pre><code>#.#L.L#.##
     * #LLL#LL.L#
     * L.#.L..#..
     * #L##.##.L#
     * #.#L.LL.LL
     * #.#L#L#.##
     * ..L.L.....
     * #L#L##L#L#
     * #.LLLLLL.L
     * #.#L#L#.##
     * </code></pre>
     * <p>At this point, something interesting happens: the chaos stabilizes and further applications of these rules cause no seats to change state! Once people stop moving around, you count <em><code>37</code></em> occupied seats.</p>
     * <p>Simulate your seating area by applying the seating rules repeatedly until no seats change state. <em>How many seats end up occupied?</em></p>
     * 
     * <p>This method will return the result for the personal input.
     */
    @Override
    public long resultPart1() throws Exception {
        String[] oldSeating, newSeating = input;

        do {
            oldSeating = newSeating;
            newSeating = new String[height];

            for(int y=0; y<height; y++) {
                StringBuilder line = new StringBuilder(width);
                for(int x=0; x<width; x++) {
                    char old = oldSeating[y].charAt(x);
                    if(old == FREE && directlyOccupiedCount(x, y, oldSeating) == 0) line.append(OCCUPIED);
                    else if(old == OCCUPIED && directlyOccupiedCount(x, y, oldSeating) >= 4) line.append(FREE);
                    else line.append(old);
                }
                newSeating[y] = line.toString();
            }
        } while (!Arrays.deepEquals(oldSeating, newSeating));

        int count = 0;
        for(String line : newSeating) for(char c : line.toCharArray()) if(c == OCCUPIED) count++;
        return count;
    }


    private final int directlyOccupiedCount(int x, int y, String[] seats) {
        int count = 0;
        for(int i=-1; i<=1; i++) for(int j=-1; j<=1; j++) {
            if(i == 0 && j == 0) continue;
            if(occupied(x+i, y+j, seats)) count++;
        }
        return count;
    }

    /**
     * <h2 id="part2">--- Part Two ---</h2><p>As soon as people start to arrive, you realize your mistake. People don't just care about adjacent seats - they care about <em>the first seat they can see</em> in each of those eight directions!</p>
     * <p>Now, instead of considering just the eight immediately adjacent seats, consider the <em>first seat</em> in each of those eight directions. For example, the empty seat below would see <em>eight</em> occupied seats:</p>
     * <pre><code>.......#.
     * ...#.....
     * .#.......
     * .........
     * ..#L....#
     * ....#....
     * .........
     * #........
     * ...#.....
     * </code></pre>
     * <p>The leftmost empty seat below would only see <em>one</em> empty seat, but cannot see any of the occupied ones:</p>
     * <pre><code>.............
     * .L.L.#.#.#.#.
     * .............
     * </code></pre>
     * <p>The empty seat below would see <em>no</em> occupied seats:</p>
     * <pre><code>.##.##.
     * #.#.#.#
     * ##...##
     * ...L...
     * ##...##
     * #.#.#.#
     * .##.##.
     * </code></pre>
     * <p>Also, people seem to be more tolerant than you expected: it now takes <em>five or more</em> visible occupied seats for an occupied seat to become empty (rather than <em>four or more</em> from the previous rules). The other rules still apply: empty seats that see no occupied seats become occupied, seats matching no rule don't change, and floor never changes.</p>
     * <p>Given the same starting layout as above, these new rules cause the seating area to shift around as follows:</p>
     * <pre><code>L.LL.LL.LL
     * LLLLLLL.LL
     * L.L.L..L..
     * LLLL.LL.LL
     * L.LL.LL.LL
     * L.LLLLL.LL
     * ..L.L.....
     * LLLLLLLLLL
     * L.LLLLLL.L
     * L.LLLLL.LL
     * </code></pre>
     * <pre><code>#.##.##.##
     * #######.##
     * #.#.#..#..
     * ####.##.##
     * #.##.##.##
     * #.#####.##
     * ..#.#.....
     * ##########
     * #.######.#
     * #.#####.##
     * </code></pre>
     * <pre><code>#.LL.LL.L#
     * #LLLLLL.LL
     * L.L.L..L..
     * LLLL.LL.LL
     * L.LL.LL.LL
     * L.LLLLL.LL
     * ..L.L.....
     * LLLLLLLLL#
     * #.LLLLLL.L
     * #.LLLLL.L#
     * </code></pre>
     * <pre><code>#.L#.##.L#
     * #L#####.LL
     * L.#.#..#..
     * ##L#.##.##
     * #.##.#L.##
     * #.#####.#L
     * ..#.#.....
     * LLL####LL#
     * #.L#####.L
     * #.L####.L#
     * </code></pre>
     * <pre><code>#.L#.L#.L#
     * #LLLLLL.LL
     * L.L.L..#..
     * ##LL.LL.L#
     * L.LL.LL.L#
     * #.LLLLL.LL
     * ..L.L.....
     * LLLLLLLLL#
     * #.LLLLL#.L
     * #.L#LL#.L#
     * </code></pre>
     * <pre><code>#.L#.L#.L#
     * #LLLLLL.LL
     * L.L.L..#..
     * ##L#.#L.L#
     * L.L#.#L.L#
     * #.L####.LL
     * ..#.#.....
     * LLL###LLL#
     * #.LLLLL#.L
     * #.L#LL#.L#
     * </code></pre>
     * <pre><code>#.L#.L#.L#
     * #LLLLLL.LL
     * L.L.L..#..
     * ##L#.#L.L#
     * L.L#.LL.L#
     * #.LLLL#.LL
     * ..#.L.....
     * LLL###LLL#
     * #.LLLLL#.L
     * #.L#LL#.L#
     * </code></pre>
     * <p>Again, at this point, people stop shifting around and the seating area reaches equilibrium. Once this occurs, you count <em><code>26</code></em> occupied seats.</p>
     * <p>Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium is reached, <em>how many seats end up occupied?</em></p>
     * 
     * <p>This method will return the result for the personal input.
     */
    @Override
    public long resultPart2() throws Exception {
        String[] oldSeating, newSeating = input;

        do {
            oldSeating = newSeating;
            newSeating = new String[height];

            for(int y=0; y<height; y++) {
                StringBuilder line = new StringBuilder(width);
                for(int x=0; x<width; x++) {
                    char old = oldSeating[y].charAt(x);
                    if(old == FREE && visiblyOccupiedCount(x, y, oldSeating) == 0) line.append(OCCUPIED);
                    else if(old == OCCUPIED && visiblyOccupiedCount(x, y, oldSeating) >= 5) line.append(FREE);
                    else line.append(old);
                }
                newSeating[y] = line.toString();
            }
        } while (!Arrays.deepEquals(oldSeating, newSeating));

        int count = 0;
        for(String line : newSeating) for(char c : line.toCharArray()) if(c == OCCUPIED) count++;
        return count;
    }


    private final int visiblyOccupiedCount(int x, int y, String[] seats) {
        int count = 0;
        for(int i=1; i<width;  i++) {
            char state = seatState(x+i, y, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<width;  i++) {
            char state = seatState(x-i, y, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<height; i++) {
            char state = seatState(x, y+i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<height; i++) {
            char state = seatState(x, y-i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }

        int max = Math.max(width, height);
        for(int i=1; i<max; i++) {
            char state = seatState(x+i, y+i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<max; i++) {
            char state = seatState(x+i, y-i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<max; i++) {
            char state = seatState(x-i, y+i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        for(int i=1; i<max; i++) {
            char state = seatState(x-i, y-i, seats);
            if(state == FLOOR) continue;
            if(state == OCCUPIED) count++;
            break;
        }
        return count;
    }

    private boolean occupied(int x, int y, String[] seats) {
        return seatState(x, y, seats) == OCCUPIED;
    }

    private char seatState(int x, int y, String[] seats) {
        try {
            return seats[y].charAt(x);
        } catch(Exception e) { }
        return FREE;
    }
}
