#include "aoc.h"
#include <vector>
#include <algorithm>

std::tuple<std::string, std::string> p03(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::vector<char>> v;

    {
        bool done = false;
        std::vector<char> line;
        while(!done) {
            char c;
            is.get(c);
            if(!is.good()) {
                done = true;
                c = '\n';
            }

            if(c == '.' || c == '#') {
                line.push_back(c);
            } else if(c == '\n') {
                if(!line.empty()) {
                    v.push_back(line);
                    line.clear();
                }
            }

        }
    }

    std::vector<int> dxs = {1, 3, 5, 7, -1};
    ans2 = 1;
    for(auto dx : dxs) {
        int dy = 1;
        if(dx < 0) {
            dy = 2;
            dx = -dx;
        }
        int x = 0;
        int64_t tmp = 0;
        for (auto it = v.begin(); it < v.end(); it+= dy) {
            auto & l = *it;
            auto cx = x % l.size();
            if (l[cx] == '#') tmp++;
            x += dx;
        }
        if(dx == 3) ans1 = tmp;
        ans2 *= tmp;
    }
    return {std::to_string(ans1), std::to_string(ans2)};
}
