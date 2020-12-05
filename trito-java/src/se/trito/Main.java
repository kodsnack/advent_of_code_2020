package se.trito;

public class Main {

    public static void main(String[] args) {
        TestReader.run();
        TestClasses.run();

        System.out.println("-----------------------------");

        Dec01a.run("trito-java/files/dec01.txt");
        Dec01b.run("trito-java/files/dec01.txt");
        Dec02a.run("trito-java/files/dec02.txt");
        Dec02b.run("trito-java/files/dec02.txt");
        Dec03a.run("trito-java/files/dec03.txt");
        Dec03b.run("trito-java/files/dec03.txt");
        Dec04a.run("trito-java/files/dec04.txt");
        Dec04b.run("trito-java/files/dec04.txt");
        Dec05a.run("trito-java/files/dec05.txt");
        Dec05b.run("trito-java/files/dec05.txt");
    }
}
