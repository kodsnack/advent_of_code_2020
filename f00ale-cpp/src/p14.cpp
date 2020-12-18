#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_map>

std::tuple<std::string, std::string> p14(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    enum class cmd {
        mem, mask
    };
    std::vector<std::tuple<cmd, uint64_t, uint64_t>> cmds;
    {
        uint64_t num = 0, num1 = 0, num2 = 0;
        std::string str;
        std::string ma;
        bool havecmd = false;
        bool curmask = false;
        for (const auto c : input) {
            if (!havecmd) {
                if (c >= 'a' && c <= 'z') {
                    str.push_back(c);
                } else if (!str.empty()) {
                    if (str == "mask") curmask = true;
                    else if (str == "mem") curmask = false;
                    else std::cout << "err: " << str << std::endl;
                    havecmd = true;
                    str.clear();
                }
            } else if (curmask) {
                if ((c >= '0' && c <= '9') || c == 'X') {
                    str.push_back(c);
                } else if (c == '\n') {
                    uint64_t ones = 0u;
                    uint64_t xs = 0u;
                    for (auto d : str) {
                        ones <<= 1u;
                        xs <<= 1u;

                        switch (d) {
                            case '1':
                                ones |= 1u;
                                break;
                            case '0':
                                break;
                            case 'X':
                                xs |= 1u;
                                break;
                        }
                    }
                    cmds.emplace_back(cmd::mask, ones, xs);
                    str.clear();
                    havecmd = false;
                }
            } else {
                if (c >= '0' && c <= '9') {
                    num *= 10;
                    num += c - '0';
                } else if (c == '\n') {
                    if (num1) {
                        cmds.emplace_back(cmd::mem, num1, num);
                    }
                    num = num1 = num2 = 0;
                    havecmd = false;
                } else {
                    if (num) {
                        if (num1) num2 = num;
                        else num1 = num;
                    }
                    num = 0;
                }

            }

        }
    }


    {
        std::unordered_map<uint64_t, uint64_t> memory;

        uint64_t c1 = 0, cX = 0;
        for (auto[c, m1, m2] : cmds) {
            if (c == cmd::mask) {
                c1 = m1;
                cX = m2;
            } else {
                memory[m1] = (m2 & cX) | c1;
            }

        }

        for (auto[a, v] : memory) { ans1 += v; }
    }

    {
        std::unordered_map<uint64_t, uint64_t> memory;
        uint64_t floating = 0, overwrite = 0;
        for (auto[c, m1, m2] : cmds) {
            if (c == cmd::mask) {
                overwrite = m1;
                floating = m2;
            } else {
                std::vector<uint64_t> mems;
                auto v = m1 | overwrite;
                auto f = floating;
                mems.push_back(v & ~f);
                uint64_t cb = 1;
                while (f) {
                    if (f & 1u) {
                        auto cpy = mems;
                        for (auto i : mems) {
                            cpy.push_back(i | cb);
                        }
                        mems.swap(cpy);
                    }
                    f >>= 1u;
                    cb <<= 1u;
                }
                for (auto m : mems) {
                    memory[m] = m2;
                }
            }

        }

        for (auto[a, v] : memory) { ans2 += v; }
    }
    return {std::to_string(ans1), std::to_string(ans2)};
}
