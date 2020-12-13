#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

template<typename T>
constexpr T chinese_remainder(const std::vector<std::tuple<T, T>> &coeff) {
    T ret = 0;
    T M = 1;
    for (auto &a : coeff) M *= std::get<1>(a);

    std::vector<T> b(coeff.size());

    for (size_t i = 0; i < coeff.size(); i++) {
        auto tmp = M / std::get<1>(coeff[i]);
        for (T bi = 1; bi < std::get<1>(coeff[i]); bi++) {
            if (tmp * bi % std::get<1>(coeff[i]) == 1) {
                b[i] = bi % M;
                break;
            }
        }
    }

    for (size_t i = 0; i < coeff.size(); i++) {
        T tmp = std::get<0>(coeff[i]) * b[i] % M;
        tmp *= M / std::get<1>(coeff[i]);
        tmp %= M;
        ret += tmp;
        ret %= M;
    }
    return ret;
}

std::tuple<std::string, std::string> p13(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::tuple<int64_t, int64_t>> v;

    int ts = 0;
    {
        int num = 0;
        std::string str;
        int idx = 0;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else if (c >= 'a' && c <= 'z') {
                str.push_back(c);
            } else {
                if (num) {
                    if (!ts) ts = num;
                    else {
                        v.emplace_back(num - idx % num, num);
                        idx++;
                    }
                } else if (str == "x") {
                    idx++;
                }
                num = 0;
                str.clear();
            }

        }
    }
    int diff = 100000000;
    for (auto[_, i] : v) {
        if (!i) continue;
        auto p = ts / i;
        auto t = (p + 1) * i;
        if (t - ts < diff) {
            diff = t - ts;
            ans1 = i * diff;
        }
    }

    ans2 = chinese_remainder(v);

    return {std::to_string(ans1), std::to_string(ans2)};
}
