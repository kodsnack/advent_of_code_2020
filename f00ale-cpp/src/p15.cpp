#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>

std::tuple<std::string, std::string> p15(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> v;

    {
        int num = 0;
        bool havenum = false;

        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                havenum = true;
            } else {
                if (havenum) {
                    v.push_back(num);
                }
                num = 0;
                havenum = false;
            }

        }
    }

    for (const size_t target : {2020, 30000000}) {

        std::vector<std::tuple<int, int>> map(target);
        int last = 0;
        size_t num = 0;
        for (auto i : v) {
            num++;
            map[i] = {num, num};
            last = i;
        }

        while (num < target) {
            num++;

            const auto[l1, l2] = map[last];
            size_t speak = l2-l1;

            auto& [f, b] = map[speak];
            f = b ? b : num;
            b = num;
            last = speak;
        }
        if (ans1) ans2 = last;
        else ans1 = last;
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
