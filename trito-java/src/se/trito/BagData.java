package se.trito;

public class BagData {

    public String name;
    public int number;

    public BagData(String name, int number) {
        this.name = name;
        this.number = number;
    }

    @Override
    public String toString() {
        return "BagData{" +
                "name='" + name + '\'' +
                ", number=" + number +
                '}';
    }
}
