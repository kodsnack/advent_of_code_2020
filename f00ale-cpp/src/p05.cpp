#include "aoc.h"
#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

std::tuple<std::string, std::string> p05(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<int> v;
    v.reserve(1000);

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
                        v.push_back(id);
                        id = 0;
                    }
                    break;
            }

        }
    }

    std::sort(v.begin(), v.end());
    for(size_t i = 0; i < v.size()-1; i++) {
        if(v[i] + 1 != v[i+1]) ans2 = v[i]+1;
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
