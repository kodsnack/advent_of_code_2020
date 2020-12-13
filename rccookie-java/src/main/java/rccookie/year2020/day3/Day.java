package rccookie.year2020.day3;

import com.github.rccookie.common.util.Console;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    private static final char TREE = '#';

    /**
     * --- Day 3: Toboggan Trajectory ---
     * <p>With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.
     * <p>Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:
     * <p>..##.......
     * <p>#...#...#..
     * <p>.#....#..#.
     * <p>..#.#...#.#
     * <p>.#...##..#.
     * <p>..#.##.....
     * <p>.#.#.#....#
     * <p>.#........#
     * <p>#.##...#...
     * <p>#...##....#
     * <p>.#..#...#.#
     * <p>These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:
     * <p>..##.........##.........##.........##.........##.........##.......  --->
     * <p>#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
     * <p>.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
     * <p>..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
     * <p>.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
     * <p>..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
     * <p>.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
     * <p>.#........#.#........#.#........#.#........#.#........#.#........#
     * <p>#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
     * <p>#...##....##...##....##...##....##...##....##...##....##...##....#
     * <p>.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
     * <p>You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
     * <p>The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
     * <p>From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
     * <p>The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
     * <p>..##.........##.........##.........##.........##.........##.......  --->
     * <p>#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
     * <p>.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
     * <p>..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
     * <p>.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
     * <p>..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
     * <p>.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
     * <p>.#........#.#........X.#........#.#........#.#........#.#........#
     * <p>#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
     * <p>#...##....##...##....##...#X....##...##....##...##....##...##....#
     * <p>.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
     * <p>In this example, traversing the map using this slope would cause you to encounter 7 trees.
     * <p>Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart1() throws Exception {
        return numberOfTreeHits(3, 1);
    }

    @Override
    public long resultPart2() throws Exception {
        int[] hits = {
            numberOfTreeHits(1, 1),
            numberOfTreeHits(3, 1),
            numberOfTreeHits(5, 1),
            numberOfTreeHits(7, 1),
            numberOfTreeHits(1, 2),
        };
        long result = 1;
        for(int i=0; i<hits.length; i++) result *= hits[i];

        Console.map("Hits", hits);
        return result;
    }

    /**
     * --- Part Two ---
     * <p>Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.
     * <p>Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
     * <p>Right 1, down 1.
     * <p>Right 3, down 1. (This is the slope you already checked.)
     * <p>Right 5, down 1.
     * <p>Right 7, down 1.
     * <p>Right 1, down 2.
     * <p>In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
     * <p>What do you get if you multiply together the number of trees encountered on each of the listed slopes?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    private int numberOfTreeHits(int vx, int vy) {
        final String[] input = inputInLines();
        if(input.length == 0) return 0;
        final int width = input[0].length();
        int count = 0;
        for(int i=0; i<input.length/vy; i++) {
            if(input[i * vy].charAt((i * vx) % width) == TREE) count++;
        }
        return count;
    }
}
