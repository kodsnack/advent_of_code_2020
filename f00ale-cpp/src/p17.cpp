#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>

std::tuple<std::string, std::string> p17(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::string> start;

    {
        std::string str;
        for (const auto c : input) {
            if (c == 'L' || c == '.' || c == '#') {
                str.push_back(c);
            } else if (c == '\n') {
                if (!str.empty()) {
                    start.push_back(str);
                }
                str.clear();
            }

        }
    }


    {
        std::vector<std::vector<std::string>> levels{start};

        for (int iteration = 0; iteration < 6; iteration++) {
            // expand
            {
                decltype(levels) next;
                std::vector<std::string> empty;
                for (size_t i = 0; i < levels[0].size() + 2; i++) {
                    empty.emplace_back(levels[0][0].length() + 2, '.');
                }
                next.push_back(empty);
                for (auto &v : levels) {
                    next.emplace_back();
                    next.back().emplace_back(levels[0][0].length() + 2, '.');
                    for (auto &s : v) {
                        next.back().emplace_back('.' + s + '.');
                    }
                    next.back().emplace_back(levels[0][0].length() + 2, '.');
                }
                next.push_back(empty);

                levels.swap(next);
            }

            auto next = levels;

            for (size_t z = 0; z < levels.size(); z++) {
                for (size_t y = 0; y < levels[z].size(); y++) {
                    for (size_t x = 0; x < levels[z][y].length(); x++) {
                        int cnt = 0;
                        for (int dz = -1; dz <= 1; dz++) {
                            if (z + dz >= levels.size()) continue;
                            for (int dy = -1; dy <= 1; dy++) {
                                if (y + dy >= levels[z + dz].size()) continue;
                                for (int dx = -1; dx <= 1; dx++) {
                                    if (!(dz || dy || dx)) continue;
                                    if (x + dx >= levels[z + dz][y + dy].length()) continue;

                                    cnt += levels[z + dz][y + dy][x + dx] == '#';

                                }
                            }
                        }

                        if (levels[z][y][x] == '#') next[z][y][x] = (cnt == 2 || cnt == 3) ? '#' : '.';
                        else next[z][y][x] = (cnt == 3) ? '#' : '.';

                    }
                }
            }


            levels.swap(next);

        }

        for (auto &v : levels) for (auto &s : v) for (auto c : s) ans1 += c == '#';
    }


    {
        std::vector<std::vector<std::string>> tmp{start};
        std::vector<std::vector<std::vector<std::string>>> levels{tmp};

        for (int iteration = 0; iteration < 6; iteration++) {
            // expand
            {
                decltype(levels) next4;
                std::vector<std::vector<std::string>> empty4;
                std::vector<std::string> empty3;
                for (size_t i = 0; i < levels[0][0].size() + 2; i++) {
                    empty3.emplace_back(levels[0][0][0].length() + 2, '.');
                }
                for (size_t i = 0; i < levels[0].size() + 2; i++) {
                    empty4.push_back(empty3);
                }

                next4.push_back(empty4);

                for(auto & l3 : levels) {
                    next4.emplace_back();
                    auto & next3 = next4.back();
                    next3.push_back(empty3);
                    for(auto & v : l3) {
                        next3.emplace_back();
                        next3.back().emplace_back(l3[0][0].length() + 2, '.');
                        for (auto &s : v) {
                            next3.back().emplace_back('.' + s + '.');
                        }
                        next3.back().emplace_back(l3[0][0].length() + 2, '.');
                    }
                    next3.push_back(empty3);
                }

                next4.push_back(empty4);

                levels.swap(next4);
            }

            auto next = levels;

            for (size_t w = 0; w < levels.size(); w++) {
                for (size_t z = 0; z < levels[w].size(); z++) {
                    for (size_t y = 0; y < levels[w][z].size(); y++) {
                        for (size_t x = 0; x < levels[w][z][y].length(); x++) {
                            int cnt = 0;
                            for (int dw = -1; dw <= 1; dw++) {
                                if (w + dw >= levels.size()) continue;
                                for (int dz = -1; dz <= 1; dz++) {
                                if (z + dz >= levels[w+dw].size()) continue;
                                for (int dy = -1; dy <= 1; dy++) {
                                    if (y + dy >= levels[w+dw][z + dz].size()) continue;
                                    for (int dx = -1; dx <= 1; dx++) {
                                        if (!(dw || dz || dy || dx)) continue;
                                        if (x + dx >= levels[w+dw][z + dz][y + dy].length()) continue;

                                        cnt += levels[w + dw][z + dz][y + dy][x + dx] == '#';
                                    }
                                }
                            }

                            }

                            if (levels[w][z][y][x] == '#') next[w][z][y][x] = (cnt == 2 || cnt == 3) ? '#' : '.';
                            else next[w][z][y][x] = (cnt == 3) ? '#' : '.';

                        }
                    }
                }
            }

            levels.swap(next);

        }

        for (auto &v : levels) for (auto &s : v) for (auto &s1 : s) for( auto c : s1) ans2 += c == '#';
    }


    return {std::to_string(ans1), std::to_string(ans2)};
}
