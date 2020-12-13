package rccookie.year2020.day7;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class BagRegulation {
    public Set<String> containedBy = new HashSet<>();
    public final Set<ContainedBagRegulation> containedBagRegulations;

    public BagRegulation(Set<ContainedBagRegulation> containedBagRegulations) {
        this.containedBagRegulations = Collections.unmodifiableSet(containedBagRegulations);
    }

    @Override
    public String toString() {
        return "[Contained by: " + containedBy + ", Contains: " + containedBagRegulations + ']';
    }


    static class ContainedBagRegulation {
        public final String name;
        public final int count;

        public ContainedBagRegulation(String name, int count) {
            this.name = name;
            this.count = count;
        }

        @Override
        public String toString() {
            return count + "x " + name;
        }
    }
}
