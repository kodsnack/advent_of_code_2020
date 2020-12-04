package dayThree;

import java.io.*;
import java.util.*;

public class TobogganTwo {

    private static ArrayList<String> rows = new ArrayList<>();

    public static void main(String[] args) {

        Scanner fileReader = null;
        try {
            fileReader = new Scanner(new File("src/dayThree/input"));
        } catch (FileNotFoundException ex) {
            System.out.println("File not found.");
            System.exit(-1);
        }

        while (fileReader.hasNextLine()) {
            rows.add(fileReader.nextLine());
        }

        System.out.println(treesHit(1, 1) * treesHit(1, 3) * treesHit(1, 5) * treesHit(1, 7) * treesHit(2, 1));
    }

    private static long treesHit(int down, int right) {
        int x = 0, y = 0;
        long trees = 0;

        while (y < rows.size()) {
            if (x + right >= rows.get(0).length()) {
                for (int i = 0; i < rows.size(); i++) {
                    rows.set(i, rows.get(i) + rows.get(i));
                }
            }

            if (rows.get(y).charAt(x) == '#') {
                trees++;
            }

            x += right;
            y += down;
        }

        return trees;
    }
}