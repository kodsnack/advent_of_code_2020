package se.trito;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class FileReaderUtil {

    public static String toString(String fileName) {
        try {
            return Files.readString(Path.of(fileName));
        } catch (IOException ex) {
            System.err.println("Error; " + ex);
        }
        return null;
    }

    public static List<String> toStringList(String fileName) {
        List<String> inputs = new ArrayList<>();
        try {
            inputs = Files.readAllLines(Path.of(fileName));
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        return inputs;
    }

    public static List<Integer> toIntList(String fileName) {
        List<Integer> inputs = new ArrayList<>();
        try {
            inputs = Files.readAllLines(Path.of(fileName))
                    .stream()
                    .map(Integer::parseInt)
                    .collect(Collectors.toList());
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        return inputs;
    }

    public static List<String> splitOnRegexToStringList(String filename, String regex) {
        List<String> inputs = new ArrayList<>();
        try {
            inputs = Arrays.asList(Files.readString(Path.of(filename)).split(regex));
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        return inputs;
    }

    public static List<Integer> splitOnRegexToIntList(String filename, String regex) {
        List<Integer> inputs = new ArrayList<>();
        try {
            inputs = Arrays.stream(Files.readString(Path.of(filename)).split(regex))
                    .map(Integer::parseInt)
                    .collect(Collectors.toList());
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        return inputs;
    }

    public static List<List<String>> splitOnRegexTo2dStringList(String fileName, String regex) {
        List<List<String>> inputs = new ArrayList<>();
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Path.of(fileName));
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        for (String line : lines) {
            inputs.add(Arrays.asList(line.split(regex)));
        }
        return inputs;
    }

    public static List<List<Integer>> splitOnRegexTo2dIntList(String fileName, String regex) {
        List<List<Integer>> inputs = new ArrayList<>();
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Path.of(fileName));
        } catch (IOException ex) {
            System.err.println("Error: " + ex);
        }
        for (String line : lines) {
            inputs.add(Arrays.stream(line.split(regex))
            .map(Integer::parseInt)
            .collect(Collectors.toList()));
        }
        return inputs;
    }
}