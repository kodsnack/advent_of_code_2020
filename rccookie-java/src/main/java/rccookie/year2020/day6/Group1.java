package rccookie.year2020.day6;

import java.util.HashSet;
import java.util.Set;

public class Group1 extends Group {

    public Group1(Set<Person> members) {
        super(members, getTicked(members));
    }

    private static final Set<Character> getTicked(Set<Person> members) {
        Set<Character> ticked = new HashSet<>();
        for(Person p : members) ticked.addAll(p.ticked);
        return ticked;
    }
}
