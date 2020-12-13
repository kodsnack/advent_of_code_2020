package rccookie.year2020.day4;

import java.util.HashMap;

public class Passport extends HashMap<String, String> {

    private static final long serialVersionUID = -5704764822441243133L;

    public boolean validKeys() {
        return !(size() < 7 || size() == 7 && containsKey("cid"));
    }

    public boolean valid() {
        if(!validKeys()) return false;

        for(String key : keySet()) {
            String s = get(key);
            int n = getInt(key);
            switch(key) {
                case "byr":
                    if(n < 1920 || n > 2002) return false;
                    break;
                case "iyr":
                    if(n < 2010 || n > 2020) return false;
                    break;
                case "eyr":
                    if(n < 2020 || n > 2030) return false;
                    break;
                case "hgt":
                    if(s.endsWith("cm")) {
                        n = Integer.parseInt(s.substring(0, s.length() - 2));
                        if(n < 150 || n > 193) return false;
                    }
                    else if(s.endsWith("in")) {
                        n = Integer.parseInt(s.substring(0, s.length() - 2));
                        if(n < 59 || n > 76) return false;
                    }
                    else return false;
                    break;
                case "hcl":
                    if(!s.startsWith("#")) return false;
                    s = s.substring(1);
                    for(char c : s.toCharArray()) {
                        if(!(c == '0' ||
                             c == '1' ||
                             c == '2' ||
                             c == '3' ||
                             c == '4' ||
                             c == '5' ||
                             c == '6' ||
                             c == '7' ||
                             c == '8' ||
                             c == '9' ||
                             c == 'a' ||
                             c == 'b' ||
                             c == 'c' ||
                             c == 'd' ||
                             c == 'e' ||
                             c == 'f')) return false;
                    }
                    break;
                case "ecl":
                    if(!(s.equals("amb") ||
                         s.equals("blu") ||
                         s.equals("brn") ||
                         s.equals("gry") ||
                         s.equals("grn") ||
                         s.equals("hzl") ||
                         s.equals("oth"))) return false;
                    break;
                case "pid":
                    if(s.length() != 9) return false;
                    for(char c : s.toCharArray()) {
                        if(!(c == '0' ||
                             c == '1' ||
                             c == '2' ||
                             c == '3' ||
                             c == '4' ||
                             c == '5' ||
                             c == '6' ||
                             c == '7' ||
                             c == '8' ||
                             c == '9')) return false;
                    }
                    break;
            }
        }

        return true;
    }

    public int getInt(String key) {
        try{
            return Integer.parseInt(get(key));
        } catch(NumberFormatException e) { return 0; }
    }
}
