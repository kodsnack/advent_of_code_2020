#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::vector<size_t>
match(const int rule, const std::string &s, const std::vector<std::tuple<char, std::vector<std::vector<int>>>> &rules,
      const size_t idx = 0) {
    if (idx > s.size()) {
        return {};
    }

    auto &[c, v] = rules[rule];
    if (c) {
        if (c == s[idx]) {
            return {1};
        } else return {};
    } else {
        std::vector<size_t> ret;
        for (auto &iv : v) {
            std::vector<size_t> mls{0};
            for (auto &r : iv) {
                decltype(mls) next;
                for (auto ml : mls) {
                    auto tmp = match(r, s, rules, idx + ml);
                    for (auto t : tmp) {
                        next.push_back(t + ml);
                    }
                }
                mls.swap(next);
            }
            for (auto ml : mls) ret.push_back(ml);
        }

        return ret;
    }

}

std::tuple<std::string, std::string> p19(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    std::vector<std::tuple<char, std::vector<std::vector<int>>>> rules;
    std::vector<std::string> strings;
    {
        int num = 0;
        std::string str;
        bool have_colon = false;
        bool have_num = false;
        std::vector<int> curlist;
        std::vector<std::vector<int>> listlist;
        size_t rulenum = 0;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else if (have_num) {
                if (have_colon) {
                    curlist.push_back(num);
                } else {
                    rulenum = num;
                }
                num = 0;
                have_num = false;
            }

            if (c == ':') {
                have_colon = true;
            } else if (c == '|') {
                listlist.push_back(curlist);
                curlist.clear();
            } else if (c == '\"') {
            } else if (c >= 'a' && c <= 'z') {
                str.push_back(c);
            } else {
                if (c == '\n') {
                    if (!curlist.empty()) {
                        listlist.push_back(curlist);
                    }
                    if (have_colon) {
                        if (rulenum >= rules.size()) rules.resize(rulenum + 1);
                        rules[rulenum] = {str.empty() ? 0 : str[0], listlist};
                    } else if (!str.empty()) {
                        strings.push_back(str);
                    }

                    have_colon = false;

                    str.clear();

                    curlist.clear();
                    listlist.clear();
                }

            }

        }
    }

    for (auto problem : {1, 2}) {
        int64_t ans = 0;
        if (problem == 2) {
            rules[8] = {0, {{42}, {42, 8}}};
            rules[11] = {0, {{42, 31}, {42, 11, 31}}};
        }

        for (auto &s : strings) {
            auto m = match(0, s, rules);
            for (auto l : m) {
                if (l == s.size()) {
                    ans++;
                    break;
                }
            }
        }

        if (problem == 1) ans1 = ans;
        else ans2 = ans;
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
