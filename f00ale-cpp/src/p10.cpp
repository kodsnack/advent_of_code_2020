#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p10(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> v;

    {
        int num = 0;
        for(const auto c : input) {
            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else {
                if(num) {
                    v.push_back(num);
                }
                num = 0;
            }

        }
    }
    std::sort(v.begin(), v.end());
    int i1 = 1, i3 = 1;

    for(size_t i = 0; i < v.size()-1; i++) {
        if(v[i+1]-v[i] == 1) i1++;
        else if(v[i+1]-v[i] == 3) i3++;
        else { std::cout << "err" << std::endl;}
    }
    ans1 = i1*i3;

    std::vector<int64_t> w(v.size());
    for(int i=0;i<3;i++) {
        if(v[i]<=3) w[i]=1;
    }
    for(size_t i = 0; i < v.size(); i++) {
        for(int j = 1; j <= 3; j++)
        if(i+j < v.size()) {
            if(v[i+j] - v[i] <= 3) w[i+j] += w[i];
        }
    }
    ans2 = w.back();
    return {std::to_string(ans1), std::to_string(ans2)};
}
