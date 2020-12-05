#include "aoc.h"
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

std::tuple<std::string, std::string> p05(std::istream & is) {
    int ans1 = 0;
    int ans2 = 1024;
    char v[1024] = {0, };
    {
        bool done = false;
        int id = 0;
        while(!done) {
            char c;
            is.get(c);
            if(!is.good()) {
                done = true;
                c = '\n';
            }

            switch(c) {
                case 'B':
                case 'R':
                    id <<= 1;
                    id |= 1;
                    break;
                case 'F':
                case 'L':
                    id <<= 1;
                    break;
                case '\n':
                    if(id) {
                        ans1 = std::max(id, ans1);
                        ans2 = std::min(id, ans2);
                        v[id] = 1;
                        id = 0;
                    }
                    break;
            }

        }
    }

    while(v[ans2]) ans2++;

    return {std::to_string(ans1), std::to_string(ans2)};
}
