package dayTwo;

import java.io.*;
import java.util.*;

public class Passwords {

    public static void main(String[] args) {

        int validCount = 0;

        Scanner fileReader = null;
        try {
            fileReader = new Scanner(new File("src/dayTwo/input"));
        } catch (FileNotFoundException ex) {
            System.out.println("File not found.");
            System.exit(-1);
        }

        while (fileReader.hasNextLine()) {
            if (isValid(fileReader.nextLine())) {
                validCount++;
            }
        }

        System.out.println(validCount);
    }

    private static boolean isValid(String line) {
        int min = Integer.parseInt(line.substring(0, line.indexOf('-')));
        int max = Integer.parseInt(line.substring(line.indexOf('-') + 1, line.indexOf(' ')));
        char character = line.charAt(line.indexOf(':') - 1);
        String password = line.substring(line.indexOf(':') + 2);

        int occurrences = 0;

        while (password.indexOf(character) != -1) {
            occurrences++;
            password = password.substring(password.indexOf(character) + 1);
        }

        return !(occurrences < min || occurrences > max);
    }
}
