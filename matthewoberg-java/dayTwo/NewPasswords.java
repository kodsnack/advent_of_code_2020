package dayTwo;

import java.io.*;
import java.util.*;

public class NewPasswords {

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
        int posOne = Integer.parseInt(line.substring(0, line.indexOf('-'))) - 1;
        int posTwo = Integer.parseInt(line.substring(line.indexOf('-') + 1, line.indexOf(' '))) - 1;
        char character = line.charAt(line.indexOf(':') - 1);
        String password = line.substring(line.indexOf(':') + 2);

        return password.charAt(posOne) == character ^ password.charAt(posTwo) == character;
    }
}
