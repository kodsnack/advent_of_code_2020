#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <deque>
#include <set>


bool play2(std::deque<int> & deck1, std::deque<int> & deck2) {
    std::set<std::tuple<std::deque<int>, std::deque<int>>> seen;

    while(!deck1.empty() && !deck2.empty()) {
        std::tuple<std::deque<int>, std::deque<int>> key{deck1, deck2};
        if(seen.count(key)) { return true; }
        seen.insert(key);

        auto c1 = deck1.front();
        deck1.pop_front();
        auto c2 = deck2.front();
        deck2.pop_front();

        if(deck1.size() >= static_cast<size_t>(c1) && deck2.size() >= static_cast<size_t>(c2)) {
            std::deque<int> cpy1(deck1.begin(), deck1.begin()+c1);
            std::deque<int> cpy2(deck2.begin(), deck2.begin()+c2);

            if(play2(cpy1,cpy2)) {
                deck1.push_back(c1);
                deck1.push_back(c2);
            } else {
                deck2.push_back(c2);
                deck2.push_back(c1);
            }
        } else {
            if(c1 > c2) {
                deck1.push_back(c1);
                deck1.push_back(c2);
            } else {
                deck2.push_back(c2);
                deck2.push_back(c1);
            }

        }

    }

    return !deck1.empty();
}

std::tuple<std::string, std::string> p22(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::deque<int> deck1, deck2;

    {
        int num = 0;
        std::deque<int> tmp;
        for(const auto c : input) {
            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else {
                if(c==':') {
                    num = 0;
                    if(!tmp.empty()) {
                        if(deck1.empty()) deck1.swap(tmp);
                        else deck2.swap(tmp);
                    }
                    tmp.clear();
                }
                if(num) {
                    tmp.push_back(num);
                }
                num = 0;
            }
        }
        if(!tmp.empty()) deck2.swap(tmp);
    }

    auto cpy1 = deck1;
    auto cpy2 = deck2;

    while(!deck1.empty() && !deck2.empty()) {
        auto c1 = deck1.front(); deck1.pop_front();
        auto c2 = deck2.front(); deck2.pop_front();
        if(c1 > c2) {
            deck1.push_back(c1);
            deck1.push_back(c2);
        } else {
            deck2.push_back(c2);
            deck2.push_back(c1);
        }
    }

    if(deck1.empty()) {
        int64_t cnt = deck2.size();
        for(auto i : deck2) ans1 += i * cnt--;
    } else {
        int64_t cnt = deck1.size();
        for(auto i : deck1) ans1 += i * cnt--;
    }

    play2(cpy1, cpy2);

    if(cpy1.empty()) {
        int64_t cnt = cpy2.size();
        for(auto i : cpy2) ans2 += i * cnt--;
    } else {
        int64_t cnt = cpy1.size();
        for(auto i : cpy1) ans2 += i * cnt--;
    }



    return {std::to_string(ans1), std::to_string(ans2)};
}
