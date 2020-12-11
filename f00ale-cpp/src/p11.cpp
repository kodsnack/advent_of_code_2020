#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <set>
std::tuple<std::string, std::string> p11(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::string> start;

    {
        std::string str;
        for(const auto c : input) {
            if(c == 'L' || c == '.' || c == '#') {
                str.push_back(c);
            } else if (c == '\n'){
                if(!str.empty()) {
                    start.push_back(str);
                }
                str.clear();
            }

        }
    }

    for(auto p : {1, 2}) {

        auto v = start;
        std::set<decltype(v)> seen;
        while (true) {
            if (seen.count(v)) {
                break;
            }
            seen.insert(v);
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
                                while (c + dc * d >= 0 && r + dr * d >= 0 && c + dc * d < v[r].size() &&
                                       r + dr * d < v.size()) {
                                    cnt += (v[r + dr * d][c + dc * d] == '#') ? 1 : 0;
                                    if(v[r + dr * d][c + dc * d] != '.' || p == 1) break;
                                    d++;
                                }

                            }
                        }


                        if (v[r][c] == 'L' && cnt == 0) next[r][c] = '#';
                        if (v[r][c] == '#' && cnt >= 3+p) next[r][c] = 'L';
                    } else std::cout << "err" << std::endl;
                }
            }
            v = next;
        }

        for (auto &&s : v) {
            for (auto c : s) if (c == '#') p == 1 ? ans1++ : ans2++;
        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
