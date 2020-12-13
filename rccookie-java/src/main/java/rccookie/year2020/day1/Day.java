package rccookie.year2020.day1;

import com.github.rccookie.common.util.Console;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    /**
     * --- Day 1: Report Repair ---
     * <p>After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
     * <p>The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
     * <p>To save your vacation, you need to get all fifty stars by December 25th.
     * <p>Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
     * <p>Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
     * <p>Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     * <p>For example, suppose your expense report contained the following:
     * <p>1721
     * <p>979
     * <p>366
     * <p>299
     * <p>675
     * <p>1456
     * <p>In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
     * <p>Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart1() {
        final String[] input = inputInLines();
        int[] numbers = new int[input.length];
        for(int i=0; i<numbers.length; i++) numbers[i] = Integer.parseInt(input[i]);

        for(int i=0; i<numbers.length-1; i++) {
            for(int j=i+1; j<numbers.length; j++) {
                if(numbers[i] + numbers[j] == 2020) {
                    Console.log(numbers[i], numbers[j]);
                    return numbers[i] * numbers[j];
                }
            }
        }
        return -1;
    }

    /**
     * --- Part Two ---
     * <p>The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.
     * <p>Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.
     * <p>In your expense report, what is the product of the three entries that sum to 2020?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart2() {
        final String[] input = inputInLines();
        int[] numbers = new int[input.length];
        for(int i=0; i<numbers.length; i++) numbers[i] = Integer.parseInt(input[i]);

        for(int i=0; i<numbers.length-2; i++) {
            for(int j=i+1; j<numbers.length-1; j++) {
                for(int k=j+1; k<numbers.length; k++) {
                    if(numbers[i] + numbers[j] + numbers[k] == 2020) {
                        Console.log(numbers[i], numbers[j], numbers[k]);
                        return numbers[i] * numbers[j] * numbers[k];
                    }
                }
            }
        }
        return -1;
    }
}
