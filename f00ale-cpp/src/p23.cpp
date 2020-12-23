#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p23(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    const std::vector<int> o = [](const std::string &input) {
        std::vector<int> o;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                o.emplace_back(c - '0');
            }
        }
        return o;
    }(input);

    std::vector<std::tuple<unsigned int, int>> probs = {{9,         100},
                                               {1'000'000, 10'000'000}};

    for (const auto[max, iter] : probs) {
        auto o2 = o;
        bool p1 = true;

        if (o2.size() < max) {
            auto me = *std::max_element(o2.begin(), o2.end());
            while (o2.size() < max) o2.push_back(++me);
            p1 = false;
        }

        std::vector<int> v(o2.size());
        for (size_t i = 0; i < o2.size() - 1; i++) v[o2[i] - 1] = o2[i + 1] - 1;

        v[o2.back() - 1] = o2[0] - 1;
        auto c = o2.front() - 1;

        for (int i = 0; i < iter; i++) {
            auto n1 = v[c];
            auto n2 = v[n1];
            auto n3 = v[n2];
            auto n4 = v[n3];

            v[c] = n4; // remove 3

            auto d = c;
            while (d == c || d == n1 || d == n2 || d == n3) {
                d--;
                if (d < 0) d = max-1;
            }

            v[n3] = v[d];
            v[d] = n1;

            c = v[c];

        }

        if (p1) {
            auto s = v[0];
            while (s != 0) {
                ans1 *= 10;
                ans1 += (s + 1);
                s = v[s];
            }
        } else {
            int64_t f1 = v[0] + 1;
            int64_t f2 = v[v[0]] + 1;
            ans2 = f1 * f2;
        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
