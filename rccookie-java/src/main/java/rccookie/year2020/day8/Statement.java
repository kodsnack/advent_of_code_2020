package rccookie.year2020.day8;

public class Statement {

    public static int ACCUMULATION = 0;
    
    private int callCount = 0;

    private final int accumulation;
    private final int nextIndex;

    public Statement(String function, int argument, int index) {
        accumulation = "acc".equals(function) ? argument : 0;
        nextIndex = index + ((accumulation == 0 && "jmp".equals(function)) ? argument : 1);
    }

    public int getCallCount() {
        return callCount;
    }

    public Statement process(Statement[] statements) {
        callCount++;
        ACCUMULATION += accumulation;
        if(nextIndex == statements.length) return null;
        return statements[nextIndex];
    }

    @Override
    public String toString() {
        return "acc " + accumulation + ", jump to " + nextIndex;
    }
}
