#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p09(const std::string &input) {
    uint64_t ans1 = 0;
    uint64_t ans2 = 0;
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

    for (size_t i = 25; i < v.size(); i++) {
        bool found = false;
        for (auto i1 = i - 25; i1 < i && !found; i1++) {
            for (auto i2 = i1 + 1; i2 < i && !found; i2++) {
                if (v[i1] + v[i2] == v[i]) found = true;
            }
        }
        if (!found) {
            ans1 = v[i];
            break;
        }
    }

    for (size_t i1 = 0; i1 < v.size(); i1++) {
        uint64_t sum = v[i1];
        uint64_t smallest = sum, largest = sum;
        for (size_t i2 = i1 + 1; i2 < v.size(); i2++) {
            auto cur = v[i2];
            sum += cur;
            smallest = std::min(smallest, cur);
            largest = std::max(largest, cur);
            if (sum == ans1) {
                ans2 = smallest + largest;
                break;
            } else if (sum > ans1) break;
        }
        if (ans2) break;
    }


    return {std::to_string(ans1), std::to_string(ans2)};
}
