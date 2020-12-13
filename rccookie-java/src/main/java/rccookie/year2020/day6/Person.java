package rccookie.year2020.day6;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class Person {
    public final Set<Character> ticked;

    public Person(char... ticked) {
        Set<Character> set = new HashSet<>();
        for(char c : ticked) set.add(c);
        this.ticked = Collections.unmodifiableSet(set);
    }

    public Person(Set<Character> ticked) {
        this.ticked = Collections.unmodifiableSet(ticked);
    }

    @Override
    public boolean equals(Object obj) {
        if(obj == this) return true;
        if(!(obj instanceof Person)) return false;
        return Objects.equals(ticked, ((Person)obj).ticked);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(ticked);
    }

    @Override
    public String toString() {
        return ticked.toString();
    }
}
