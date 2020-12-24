#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>
#include <set>

std::tuple<std::string, std::string> p21(const std::string &input) {
    int64_t ans1 = 0;
    std::string ans2;
    std::vector<int> v;
    std::vector<std::vector<std::string>> ing, all;
    {
        std::string str;
        std::vector<std::string> ti, ta;
        bool have_paren = false;

        for (const auto c : input) {
            if (c >= 'a' && c <= 'z') {
                str.push_back(c);
            } else {
                if (!str.empty()) {
                    if (have_paren) {
                        if (str != "contains") ta.emplace_back(std::move(str));
                    } else ti.emplace_back(std::move(str));
                    str.clear();
                }

                if (c == '\n') {
                    if (!ti.empty() || !ta.empty()) {
                        ing.emplace_back(std::move(ti));
                        all.emplace_back(std::move(ta));
                    }
                    ti.clear();
                    ta.clear();
                    have_paren = false;
                } else if (c == '(') {
                    have_paren = true;
                }
                str.clear();
            }

        }
    }

    std::set<std::string> allergens;

    for(auto & a : all) for(auto & s : a) allergens.insert(s);

    std::map<std::string, std::vector<std::set<std::string>>> a2r;
    for (size_t i = 0; i < ing.size(); i++) {
        for (auto &sa : all[i]) {
            a2r[sa].emplace_back(ing[i].begin(), ing[i].end());
        }
    }

    std::map<std::string, std::string> found;

    while (found.size() < allergens.size()) {
        for (auto &[a, v] : a2r) {
            std::set<std::string> poss;
            for(auto & se : v) for(auto & s: se) {
                /*if(found.count(s) == 0)*/ poss.insert(s);
            }
            for(auto & [f,q] : found) poss.erase(f);

            for(auto & s : poss) {
                bool inall = true;
                for(auto & se : v) {
                    inall = se.count(s);
                    if(!inall) break;
                }

                if(!inall) {
                    for (auto &se : v) {
                        se.erase(s);
                    }
                }
            }

            if(v[0].size() == 1) {
                auto ta = *v[0].begin();
                found[ta] = a;

                for(auto & [_,v2] : a2r) for(auto & s : v2) s.erase(ta);
            }
        }
    }

    for(auto & il : ing) {
        for(auto & i : il) {
            if(found.count(i) != 1) ans1++;
        }
    }

    decltype(found) revmap;
    for(auto & [a,b] : found) {
        revmap[b] = a;
    }
    for(auto & [a,b] : revmap) {
        if(!ans2.empty()) ans2 += ',';
        ans2 += b;
    }

    return {std::to_string(ans1), ans2};
}
