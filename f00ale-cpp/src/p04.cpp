#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_set>

std::tuple<std::string, std::string> p04(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> v;
    std::unordered_set<std::string> ecls { "amb", "blu", "brn", "gry", "grn", "hzl", "oth"};
    {
        int f = 0;
        int l = 0;
        std::string code, data;
        bool hascid = false;
        bool hascolon = false;
        bool invalid = false;

        for(const auto c : input) {
            if((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '#') {
                if(!hascolon) code += c;
                else data += c;
            } else if (c == ':') {
                hascolon = true;
            } else if(c == ' ' || c == '\n' ) {
                if(hascolon) {
                    f++;
                    if (code == "byr") {
                        int tmp = std::stoi(data);
                        if (tmp < 1920 || tmp > 2002) invalid = true;
                    } else if (code == "iyr") {
                        int tmp = std::stoi(data);
                        if (tmp < 2010 || tmp > 2020) invalid = true;
                    } else if (code == "eyr") {
                        int tmp = std::stoi(data);
                        if (tmp < 2020 || tmp > 2030) invalid = true;
                    } else if(code == "hgt") {
                        int tmp = std::stoi(data);
                        if(data.find("cm") != std::string::npos) {
                            if (tmp < 150 || tmp > 193) invalid = true;
                        } else if(data.find("in") != std::string::npos) {
                            if (tmp < 59 || tmp > 76) invalid = true;
                        } else {
                            invalid = true;
                        }
                    } else if(code == "hcl") {
                        if(data.size() != 7) invalid = true;
                        else if(data[0] != '#') invalid = true;
                        else {
                            for(int i = 1; i < 7; i++) if(char q = data[i]; (q < '0' || q > '9') && (q < 'a' || q > 'f')) invalid = true;
                        }
                    } else if(code == "pid") {
                        if(data.size() != 9) invalid = true;
                        else {
                            for(int i = 0; i < 9; i++) if(char q = data[i]; (q < '0' || q > '9')) invalid = true;

                        }
                    } else if(code == "ecl") {
                        if(ecls.find(data) == ecls.end()) invalid = true;
                    } else if(code == "cid") {
                        hascid = true;
                    }

                }

                code.clear();
                data.clear();
                hascolon = false;
            }

            if(c == '\n') {
                if(!l) {
                    if((f == 7 && !hascid) || f == 8) {
                        ans1++;
                        if(!invalid) {
                            ans2++;
                        }
                    }
                    f = 0;
                    hascid = false;
                    invalid = false;
                }
                l = 0;

            } else {
                l++;
            }
        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
