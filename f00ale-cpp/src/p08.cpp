#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p08(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    enum class cmds {
        jmp, acc, nop
    };
    std::vector<std::tuple<cmds, int>> v;
    {
        int num = 0;
        bool neg = false;
        std::string cmd;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else if (c >= 'a' && c <= 'z') {
                cmd.push_back(c);
            } else if (c == '+' || c == '-') {
                neg = (c == '-');
            } else if (c == '\n') {
                if (!cmd.empty()) {
                    if (neg) num *= -1;
                    if (cmd == "jmp") v.emplace_back(cmds::jmp, num);
                    else if (cmd == "acc") v.emplace_back(cmds::acc, num);
                    else if (cmd == "nop") v.emplace_back(cmds::nop, num);
                    else std::cout << "unknown: " << cmd << std::endl;
                }
                num = 0;
                neg = false;
                cmd.clear();
            }

        }
    }

    bool have_a1 = false;
    for (size_t i = 0; i < v.size(); i++) {
        bool search_a1 = false;
        const auto[c, n] = v[i];
        if (c == cmds::acc) { if (have_a1) continue; else search_a1 = true; }
        else if (c == cmds::jmp) v[i] = {cmds::nop, n};
        else if (c == cmds::nop) v[i] = {cmds::jmp, n};

        std::vector<int> cnts(v.size());
        size_t pc = 0;
        int a = 0;
        while (pc < v.size() && !cnts[pc]) {
            cnts[pc]++;
            auto &&[cmd, num] = v[pc];
            switch (cmd) {
                case cmds::nop:
                    pc++;
                    break;
                case cmds::acc:
                    pc++;
                    a += num;
                    break;
                case cmds::jmp:
                    pc += num;
                    break;

            }
        }
        if (pc >= v.size()) ans2 = a;
        else if (search_a1) {
            have_a1 = true;
            ans1 = a;
        }
        // change back
        v[i] = {c, n};
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
