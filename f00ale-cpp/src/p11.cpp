#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p11(const std::string &input) {
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

    for (auto p : {1, 2}) {
        auto v = start;
        while (true) {
            auto next = v;
            for (size_t r = 0; r < v.size(); r++) {
                for (size_t c = 0; c < v[r].size(); c++) {
                    if (v[r][c] == '.') continue;
                    else if (v[r][c] == 'L' || v[r][c] == '#') {
                        int cnt = 0;

                        for (int dr = -1; dr <= 1; dr++) {
                            for (int dc = -1; dc <= 1; dc++) {
                                if (!dc && !dr) continue;

                                int d = 1;
                                while (c + dc * d < v[r].size() && r + dr * d < v.size()) {
                                    cnt += (v[r + dr * d][c + dc * d] == '#') ? 1 : 0;
                                    if (v[r + dr * d][c + dc * d] != '.' || p == 1) break;
                                    d++;
                                }

                            }
                        }

                        if (v[r][c] == 'L' && cnt == 0) next[r][c] = '#';
                        else if (v[r][c] == '#' && cnt >= 3 + p) next[r][c] = 'L';
                    } else std::cout << "err" << std::endl;
                }
            }
            if (next == v) break;
            v.swap(next);
        }

        for (auto &&s : v) {
            auto cnt = std::count(s.begin(), s.end(), '#');
            if (p == 1) ans1 += cnt;
            else ans2 += cnt;
        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
