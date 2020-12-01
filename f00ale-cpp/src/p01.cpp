#include "aoc.h"
#include <vector>
#include <algorithm>

std::tuple<std::string, std::string> p01(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> v;

    {
        bool done = false;
        int num = 0;
        while(!done) {
            char c;
            is.get(c);
            if(!is.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c-'0';
            } else {
                if(num) {
                    v.push_back(num);
                }
                num = 0;
            }

        }
    }

    std::sort(v.begin(), v.end());

    for(auto it1 = v.begin(); it1 != v.end(); it1++) {
        for(auto it2 = it1 + 1; it2 != v.end(); it2++) {
            auto a1 = *it1 + *it2;
            if(a1 == 2020) ans1 = *it1 * *it2;
            if(a1 > 2020) break;
            for(auto it3 = it2 + 1; it3 != v.end(); it3++) {
                auto a2 = a1 + *it3;
                if(a2 == 2020) ans2 = *it1 * *it2 * *it3;
                if(a2 > 2020) break;
            }

        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
