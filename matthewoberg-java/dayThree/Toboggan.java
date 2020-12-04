package dayThree;

import java.io.*;
import java.util.*;

public class Toboggan {

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

        int x = 0, y = 0, trees = 0;

        while (y != rows.size()) {
            if (x + 3 >= rows.get(0).length()) {
                for (int i = 0; i < rows.size(); i++) {
                    rows.set(i, rows.get(i) + rows.get(i));
                }
            }

            if (rows.get(y).charAt(x) == '#') {
                trees++;
            }

            x += 3;
            y++;
        }

        System.out.println(trees);
    }
}
