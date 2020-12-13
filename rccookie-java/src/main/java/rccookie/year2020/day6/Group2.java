package rccookie.year2020.day6;

import java.util.HashSet;
import java.util.Set;

public class Group2 extends Group {

    public Group2(Set<Person> members) {
        super(members, getTicked(members));
    }

    private static final Set<Character> getTicked(Set<Person> members) {
        Set<Character> ticked = new HashSet<>();
        if(members.isEmpty()) return ticked;
        Person[] array = members.toArray(new Person[0]);
        charLoop:
        for(char c : array[0].ticked) {
            for(int i=1; i<array.length; i++) if(!array[i].ticked.contains(c)) continue charLoop;
            ticked.add(c);
        }
        return ticked;
    }
}
