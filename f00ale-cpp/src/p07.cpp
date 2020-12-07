#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <deque>

std::tuple<std::string, std::string> p07(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::deque<std::string> tocheck;
    std::map<std::string, std::vector<std::pair<std::string, int>>> map;

    std::set<std::string> seen;
    {
        std::string line;
        for (const auto c : input) {
            if (c == '\n') {
                if (!line.empty()) {
                    auto p = line.find(" bags");
                    auto outer = line.substr(0, p);
                    while ((p = line.find_first_of("0123456789", p)) != std::string::npos) {
                        auto e = line.find(" bag", p);
                        auto inner = line.substr(p + 2, e - p - 2);
                        map[outer].emplace_back(inner, line[p] - '0'); // assume single-digit numbers
                        p++;
                    }
                }
                line.clear();
            } else {
                line.push_back(c);
            }

        }
    }

    tocheck.emplace_back("shiny gold");
    while (!tocheck.empty()) {
        auto curr = tocheck.front();
        tocheck.pop_front();
        seen.insert(curr);
        for (auto &&[o, in] : map) {
            if (seen.find(o) != seen.end()) continue;
            for (auto &&[bag, cnt] : in) {
                if (bag == curr) {
                    tocheck.push_back(o);

                }
            }
        }

    }
    ans1 = seen.size() - 1; // subtract the shiny gold itself

    std::deque<std::pair<std::string, int>> tc2;

    tc2.emplace_back("shiny gold", 1);
    while (!tc2.empty()) {
        auto[curr, mult] = tc2.front();
        tc2.pop_front();

        auto &&q = map[curr];
        for (auto &&[b, cnt] : q) {
            ans2 += mult * cnt;
            tc2.emplace_back(b, mult * cnt);
        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
