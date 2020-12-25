#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p25(const std::string &input) {
    int64_t ans1 = 0;
    std::vector<uint64_t> v;

    {
        uint64_t num = 0;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else {
                if (num) {
                    v.push_back(num);
                }
                num = 0;
            }
        }
    }

    constexpr uint64_t mod = 20201227;

    int loop = 0;

    uint64_t val = 1;
    while (val != v[0]) {
        val = val * 7 % mod;
        loop++;
    }

    ans1 = 1;
    for (int j = 0; j < loop; j++) {
        ans1 = ans1 * v[1] % mod;
    }

    return {std::to_string(ans1), "-"};
}
