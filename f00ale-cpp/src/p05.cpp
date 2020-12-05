#include "aoc.h"
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

std::tuple<std::string, std::string> p05(const std::string & input) {
    uint32_t ans1 = 0;
    uint32_t ans2 = 1024;
    char v[1024] = {0, };
    {
        uint32_t id = 0;
        for(const auto c : input) {
            switch(c) {
                case 'B':
                case 'R':
                    id <<= 1u;
                    id |= 1u;
                    break;
                case 'F':
                case 'L':
                    id <<= 1u;
                    break;
                case '\n':
                    if(id) {
                        ans1 = std::max(id, ans1);
                        ans2 = std::min(id, ans2);
                        v[id] = 1;
                        id = 0;
                    }
                    break;
                default:
                    break;
            }
        }
    }

    while(v[ans2]) ans2++;

    return {std::to_string(ans1), std::to_string(ans2)};
}
