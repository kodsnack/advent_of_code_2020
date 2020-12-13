package rccookie.year2020.day6;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;

public class Group {
    public final Set<Person> members;
    public final Set<Character> ticked;
    public final int count;

    public Group(Set<Person> members, Set<Character> ticked) {
        this.members = Collections.unmodifiableSet(members);
        this.ticked = Collections.unmodifiableSet(ticked);
        this.count = ticked.size();
    }

    @Override
    public boolean equals(Object obj) {
        if(obj == this) return true;
        if(!(obj instanceof Group1)) return false;
        return Objects.equals(members, ((Group1)obj).members);
    }

    @Override
    public int hashCode() {
        return Objects.hash(members, ticked);
    }

    @Override
    public String toString() {
        return members.toString();
    }
}
