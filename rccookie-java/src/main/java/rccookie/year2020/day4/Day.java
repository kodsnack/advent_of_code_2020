package rccookie.year2020.day4;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Day extends com.github.rccookie.adventofcode.util.Day {

    /**
     * --- Day 4: Passport Processing ---
     * <p>You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your passport. While these documents are extremely similar, North Pole Credentials aren't issued by a country and therefore aren't actually valid documentation for travel in most of the world.
     * <p>It seems like you're not the only one having problems, though; a very long line has formed for the automatic passport scanners, and the delay could upset your travel itinerary.
     * <p>Due to some questionable network security, you realize you might be able to solve both of these problems at the same time.
     * <p>The automatic passport scanners are slow because they're having trouble detecting which passports have all required fields. The expected fields are as follows:
     * <p>byr (Birth Year)
     * <p>iyr (Issue Year)
     * <p>eyr (Expiration Year)
     * <p>hgt (Height)
     * <p>hcl (Hair Color)
     * <p>ecl (Eye Color)
     * <p>pid (Passport ID)
     * <p>cid (Country ID)
     * <p>Passport data is validated in batch files (your puzzle input). Each passport is represented as a sequence of key:value pairs separated by spaces or newlines. Passports are separated by blank lines.
     * <p>Here is an example batch file containing four passports:
     * <p>ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
     * <p>byr:1937 iyr:2017 cid:147 hgt:183cm
     * <p>
     * <p>iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
     * <p>hcl:#cfa07d byr:1929
     * <p>
     * <p>hcl:#ae17e1 iyr:2013
     * <p>eyr:2024
     * <p>ecl:brn pid:760753108 byr:1931
     * <p>hgt:179cm
     * <p>
     * <p>hcl:#cfa07d eyr:2025 pid:166559648
     * <p>iyr:2011 ecl:brn hgt:59in
     * <p>The first passport is valid - all eight fields are present. The second passport is invalid - it is missing hgt (the Height field).
     * <p>The third passport is interesting; the only missing field is cid, so it looks like data from North Pole Credentials, not a passport at all! Surely, nobody would mind if you made the system temporarily ignore missing cid fields. Treat this "passport" as valid.
     * <p>The fourth passport is missing two fields, cid and byr. Missing cid is fine, but missing any other field is not, so this passport is invalid.
     * <p>According to the above rules, your improved system would report 2 valid passports.
     * <p>Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart1() throws Exception {
        int valid = 0;
        for(Passport p : parsePassports()) {
            if(p.validKeys()) valid++;
        }
        return valid;
    }

    /**
     * --- Part Two ---
     * <p>The line is moving more quickly now, but you overhear airport security talking about how passports with invalid data are getting through. Better add some data validation, quick!
     * <p>You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:
     * <p>byr (Birth Year) - four digits; at least 1920 and at most 2002.
     * <p>iyr (Issue Year) - four digits; at least 2010 and at most 2020.
     * <p>eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
     * <p>hgt (Height) - a number followed by either cm or in:
     * <p>If cm, the number must be at least 150 and at most 193.
     * <p>If in, the number must be at least 59 and at most 76.
     * <p>hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
     * <p>ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
     * <p>pid (Passport ID) - a nine-digit number, including leading zeroes.
     * <p>cid (Country ID) - ignored, missing or not.
     * <p>Your job is to count the passports where all required fields are both present and valid according to the above rules. Here are some example values:
     * <p>byr valid:   2002
     * <p>byr invalid: 2003
     * <p>
     * <p>hgt valid:   60in
     * <p>hgt valid:   190cm
     * <p>hgt invalid: 190in
     * <p>hgt invalid: 190
     * <p>
     * <p>hcl valid:   #123abc
     * <p>hcl invalid: #123abz
     * <p>hcl invalid: 123abc
     * <p>
     * <p>ecl valid:   brn
     * <p>ecl invalid: wat
     * <p>
     * <p>pid valid:   000000001
     * <p>pid invalid: 0123456789
     * <p>Here are some invalid passports:
     * <p>eyr:1972 cid:100
     * <p>hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
     * <p>
     * <p>iyr:2019
     * <p>hcl:#602927 eyr:1967 hgt:170cm
     * <p>ecl:grn pid:012533040 byr:1946
     * <p>
     * <p>hcl:dab227 iyr:2012
     * <p>ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
     * <p>
     * <p>hgt:59cm ecl:zzz
     * <p>eyr:2038 hcl:74454a iyr:2023
     * <p>pid:3556412378 byr:2007
     * <p>Here are some valid passports:
     * <p>pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
     * <p>hcl:#623a2f
     * <p>
     * <p>eyr:2029 ecl:blu cid:129 byr:1989
     * <p>iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
     * <p>
     * <p>hcl:#888785
     * <p>hgt:164cm byr:2001 iyr:2015 cid:88
     * <p>pid:545766238 ecl:hzl
     * <p>eyr:2022
     * <p>
     * <p>iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
     * <p>Count the number of valid passports - those that have all required fields and valid values. Continue to treat cid as optional. In your batch file, how many passports are valid?
     * <p><b>This method will print the result for the personal input in the console.</b>
     */
    @Override
    public long resultPart2() throws Exception {
        int valid = 0;
        for(Passport p : parsePassports()) {
            if(p.valid()) valid++;
        }
        return valid;
    }

    private List<Passport> parsePassports() throws Exception {
        List<Passport> passports = new ArrayList<>();
        passports.add(new Passport());

        Scanner sc = inputScanner();

        while(sc. hasNextLine()) {
            String line = sc.nextLine();
            if(line.replaceAll(" ", "").length() == 0) {
                passports.add(new Passport());
                continue;
            }
            for(String pair : line.split(" ")) {
                int colonIndex = pair.indexOf(':');
                passports.get(passports.size() - 1).put(pair.substring(0, colonIndex), pair.substring(colonIndex + 1));
            }
        }

        return passports;
    }
}
