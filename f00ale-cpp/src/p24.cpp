#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>
std::tuple<std::string, std::string> p24(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> v;
    std::map<std::tuple<int,int,int>, int> map;
    {
        std::string str;
        int x = 0, y = 0, z = 0;
        bool f = false;
        for(const auto c : input) {
            if(c == 'n' || c == 's') {
                str.push_back(c);
            } else if(c == 'e' || c == 'w' ) {
                str.push_back(c);

                if(str == "w") { y++; z--; }
                else if(str == "e") { y--; z++; }
                else if(str == "nw") { x++; z--; }
                else if(str == "se") { x--; z++; }
                else if(str == "ne") { x++; y--; }
                else if(str == "sw") { x--; y++; }
                else std::cout << "err: " << str << std::endl;
                str.clear();
                f = true;
            } else if(c == '\n') {
                if(f) {
                    map[{x, y, z}]++;
                }
                f = false;
                x = y = z = 0;
            }

        }
    }

    for(auto & [a,b] : map) {
        ans1 += b % 2;
    }

    for(int i = 0; i < 100; i++) {
        std::vector<std::tuple<int,int,int>> blacks;
        for(auto & [a,b] : map) if(b%2) blacks.push_back(a);

        decltype(map) neighs, nm;

        for(const auto & [x,y,z] : blacks) {
            neighs[{x,y+1,z-1}]++;
            neighs[{x,y-1,z+1}]++;
            neighs[{x+1,y,z-1}]++;
            neighs[{x-1,y,z+1}]++;
            neighs[{x+1,y-1,z}]++;
            neighs[{x-1,y+1,z}]++;
        }

        for(const auto & c : blacks) {
            if(neighs[c] == 0 || neighs[c] > 2) {}
            else nm[c] = 1;
        }

        for(auto [c,n] : neighs) {
            if(n == 2 && map[c] % 2 == 0) nm[c] = 1;
        }

        map.swap(nm);
    }
    // now map only contains black tiles
    ans2 = map.size();

    return {std::to_string(ans1), std::to_string(ans2)};
}
