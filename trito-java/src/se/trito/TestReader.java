package se.trito;

import java.util.Arrays;
import java.util.List;

public class TestReader {
    private static final String TEST_FILE_1 = "trito-java/files/test1.txt";
    private static final String TEST_FILE_2 = "trito-java/files/test2.txt";
    private static final String TEST_FILE_3 = "trito-java/files/test3.txt";
    public static void run() {
        List<Integer> inputInInts = FileReaderUtil.toIntList(TEST_FILE_1);
        List<String> inputInStrings = FileReaderUtil.toStringList(TEST_FILE_1);
        List<String> inputsInStringsSplitted = FileReaderUtil.splitOnRegexToStringList(TEST_FILE_2, "\\s*,\\s*");
        List<Integer> inputsInIntsSplitted = FileReaderUtil.splitOnRegexToIntList(TEST_FILE_2, "\\s*,\\s*");
        List<List<String>> inputsIn2dStringList = FileReaderUtil.splitOnRegexTo2dStringList(TEST_FILE_3, ",");
        List<List<Integer>> inputsIn2dIntList = FileReaderUtil.splitOnRegexTo2dIntList(TEST_FILE_3, ",");
        String input = FileReaderUtil.toString(TEST_FILE_3);
        System.out.println(" ---- TEST READER ----");
        System.out.println("Text to ints: " + Arrays.toString(inputInInts.toArray()));
        System.out.println("Text to strings: " + Arrays.toString(inputInStrings.toArray()));
        System.out.println("Text to Strings, splitted on ,: " + Arrays.toString(inputsInStringsSplitted.toArray()));
        System.out.println("Text to ints, splitted on ,: " + Arrays.toString(inputsInIntsSplitted.toArray()));
        for (List<String> line : inputsIn2dStringList)
            System.out.println("Text in 2d string list: " +Arrays.toString(line.toArray()));
        for (List<Integer> line : inputsIn2dIntList)
            System.out.println("Text in 2d int list: " +Arrays.toString(line.toArray()));
        System.out.println("String in testfile: " + input);
    }
}
