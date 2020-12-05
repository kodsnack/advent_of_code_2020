package se.trito;

import java.util.ArrayList;
import java.util.List;

public class Dec03b {
    public static void run(String fileName) {

        List<String> inputs = FileReaderUtil.toStringList(fileName);
        Long totalCount = 1L;
        int iPos = 0;

        List<Node> nodes = new ArrayList<>();
        nodes.add(new Node(1, 1));
        nodes.add(new Node(3, 1));
        nodes.add(new Node(5, 1));
        nodes.add(new Node(7, 1));
        nodes.add(new Node(1, 2));

        for (Node node : nodes) {
            int jPos = node.y;
            int iSteps = node.x;
            int jSteps = node.y;
            int counter = 0;
            for (int j = jPos; j < inputs.size(); j = j + jSteps) {
                iPos = (iPos + iSteps) % inputs.get(0).length();
                if (inputs.get(j).charAt(iPos) == '#') {
                    counter++;
                }
            }
            iPos = inputs.get(0).length();
            totalCount = totalCount * counter;
        }
        System.out.println("Dec03b: " + totalCount);
    }
}
