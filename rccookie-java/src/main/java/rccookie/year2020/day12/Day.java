package rccookie.year2020.day12;

import com.github.rccookie.common.geometry.IntVector2D;
import com.github.rccookie.common.geometry.Transform2D;
import com.github.rccookie.common.geometry.Vector2D;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    public static final Vector2D N = Vector2D.UNIT_VECTOR_Y, S = N.inverted(), E = Vector2D.UNIT_VECTOR_X, W = E.inverted();

    private final String[] input;

    {
        input = inputInLines();
    }

    /**
     * <h2>--- Day 12: Rain Risk ---</h2><p>Your ferry made decent progress toward the island, but the storm came in <span title="At least it wasn't a Category Six!">faster than anyone expected</span>. The ferry needs to take <em>evasive actions</em>!</p>
     * <p>Unfortunately, the ship's navigation computer seems to be malfunctioning; rather than giving a route directly to safety, it produced extremely circuitous instructions. When the captain uses the <a href="https://en.wikipedia.org/wiki/Public_address_system" target="_blank">PA system</a> to ask if anyone can help, you quickly volunteer.</p>
     * <p>The navigation instructions (your puzzle input) consists of a sequence of single-character <em>actions</em> paired with integer input <em>values</em>. After staring at them for a few minutes, you work out what they probably mean:</p>
     * <ul>
     * <li>Action <em><code>N</code></em> means to move <em>north</em> by the given value.</li>
     * <li>Action <em><code>S</code></em> means to move <em>south</em> by the given value.</li>
     * <li>Action <em><code>E</code></em> means to move <em>east</em> by the given value.</li>
     * <li>Action <em><code>W</code></em> means to move <em>west</em> by the given value.</li>
     * <li>Action <em><code>L</code></em> means to turn <em>left</em> the given number of degrees.</li>
     * <li>Action <em><code>R</code></em> means to turn <em>right</em> the given number of degrees.</li>
     * <li>Action <em><code>F</code></em> means to move <em>forward</em> by the given value in the direction the ship is currently facing.</li>
     * </ul>
     * <p>The ship starts by facing <em>east</em>. Only the <code>L</code> and <code>R</code> actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is <code>N10</code>, the ship would move north 10 units, but would still move east if the following action were <code>F</code>.)</p>
     * <p>For example:</p>
     * <pre><code>F10
     * N3
     * F7
     * R90
     * F11
     * </code></pre>
     * <p>These instructions would be handled as follows:</p>
     * <ul>
     * <li><code>F10</code> would move the ship 10 units east (because the ship starts by facing east) to <em>east 10, north 0</em>.</li>
     * <li><code>N3</code> would move the ship 3 units north to <em>east 10, north 3</em>.</li>
     * <li><code>F7</code> would move the ship another 7 units east (because the ship is still facing east) to <em>east 17, north 3</em>.</li>
     * <li><code>R90</code> would cause the ship to turn right by 90 degrees and face <em>south</em>; it remains at <em>east 17, north 3</em>.</li>
     * <li><code>F11</code> would move the ship 11 units south to <em>east 17, south 8</em>.</li>
     * </ul>
     * <p>At the end of these instructions, the ship's <a href="https://en.wikipedia.org/wiki/Manhattan_distance" target="_blank">Manhattan distance</a> (sum of the absolute values of its east/west position and its north/south position) from its starting position is <code>17 + 8</code> = <em><code>25</code></em>.</p>
     * <p>Figure out where the navigation instructions lead. <em>What is the Manhattan distance between that location and the ship's starting position?</em></p>
     * 
     * <p>This method will return the result for the personal input.
     */
    @Override
    public long resultPart1() throws Exception {
        Transform2D target = new Transform2D();
        for(String line : input) {
            char action = line.charAt(0);
            int value = Integer.parseInt(line.substring(1));
            switch(action) {
                case 'N': target.location.add(N.scaled(value)); break;
                case 'S': target.location.add(S.scaled(value)); break;
                case 'W': target.location.add(W.scaled(value)); break;
                case 'E': target.location.add(E.scaled(value)); break;
                case 'L': target.rotation += value; break;
                case 'R': target.rotation -= value; break;
                case 'F': target.location.add(Vector2D.angledVector(target.rotation, value)); break;
            }
            target.rotation %= 360;
            if(target.rotation < 0) target.rotation += 360;
        }
        target.location.round(); // To make sure that rounding works properly
        return (long)(Math.abs(target.location.x()) + Math.abs(target.location.y()));
    }

    /**
     * <h2 id="part2">--- Part Two ---</h2><p>Before you can give the destination to the captain, you realize that the actual action meanings were printed on the back of the instructions the whole time.</p>
     * <p>Almost all of the actions indicate how to move a <em>waypoint</em> which is relative to the ship's position:</p>
     * <ul>
     * <li>Action <em><code>N</code></em> means to move the waypoint <em>north</em> by the given value.</li>
     * <li>Action <em><code>S</code></em> means to move the waypoint <em>south</em> by the given value.</li>
     * <li>Action <em><code>E</code></em> means to move the waypoint <em>east</em> by the given value.</li>
     * <li>Action <em><code>W</code></em> means to move the waypoint <em>west</em> by the given value.</li>
     * <li>Action <em><code>L</code></em> means to rotate the waypoint around the ship <em>left</em> (<em>counter-clockwise</em>) the given number of degrees.</li>
     * <li>Action <em><code>R</code></em> means to rotate the waypoint around the ship <em>right</em> (<em>clockwise</em>) the given number of degrees.</li>
     * <li>Action <em><code>F</code></em> means to move <em>forward</em> to the waypoint a number of times equal to the given value.</li>
     * </ul>
     * <p>The waypoint starts <em>10 units east and 1 unit north</em> relative to the ship. The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.</p>
     * <p>For example, using the same instructions as above:</p>
     * <ul>
     * <li><code>F10</code> moves the ship to the waypoint 10 times (a total of <em>100 units east and 10 units north</em>), leaving the ship at <em>east 100, north 10</em>. The waypoint stays 10 units east and 1 unit north of the ship.</li>
     * <li><code>N3</code> moves the waypoint 3 units north to <em>10 units east and 4 units north of the ship</em>. The ship remains at <em>east 100, north 10</em>.</li>
     * <li><code>F7</code> moves the ship to the waypoint 7 times (a total of <em>70 units east and 28 units north</em>), leaving the ship at <em>east 170, north 38</em>. The waypoint stays 10 units east and 4 units north of the ship.</li>
     * <li><code>R90</code> rotates the waypoint around the ship clockwise 90 degrees, moving it to <em>4 units east and 10 units south of the ship</em>. The ship remains at <em>east 170, north 38</em>.</li>
     * <li><code>F11</code> moves the ship to the waypoint 11 times (a total of <em>44 units east and 110 units south</em>), leaving the ship at <em>east 214, south 72</em>. The waypoint stays 4 units east and 10 units south of the ship.</li>
     * </ul>
     * <p>After these operations, the ship's Manhattan distance from its starting position is <code>214 + 72</code> = <em><code>286</code></em>.</p>
     * <p>Figure out where the navigation instructions actually lead. <em>What is the Manhattan distance between that location and the ship's starting position?</em></p>
     * 
     * <p>This method will return the result for the personal input.
     */
    @Override
    public long resultPart2() throws Exception {
        Vector2D waypoint = new IntVector2D(E.scaled(10).add(N));
        Vector2D ship = new IntVector2D();
        for(String line : input) {
            char action = line.charAt(0);
            int value = Integer.parseInt(line.substring(1));
            switch(action) {
                case 'N': waypoint.add(N.scaled(value)); break;
                case 'S': waypoint.add(S.scaled(value)); break;
                case 'W': waypoint.add(W.scaled(value)); break;
                case 'E': waypoint.add(E.scaled(value)); break;
                case 'L': waypoint.rotate(value); break;
                case 'R': waypoint.rotate(-value); break;
                case 'F': ship.add(waypoint.scaled(value)); break;
            }
        }
        return (long)(Math.abs(ship.x()) + Math.abs(ship.y()));
    }
}
