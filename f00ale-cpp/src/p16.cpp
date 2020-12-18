#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_set>

std::tuple<std::string, std::string> p16(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<int> my_ticket;
    std::vector<std::vector<int>> other_tickets;
    std::vector<std::tuple<int,int,int,int,std::string>> ranges;
    {
        int num = 0;
        std::string str;
        bool have_colon = false;
        bool have_num = false;
        std::vector<int> nums;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else {
                if(have_num) {
                    nums.push_back(num);
                    have_num = false;
                    num = 0;
                }
                if(c == ':') {
                    have_colon = true;
                } else if (((c >= 'a' && c <= 'z') || c == ' ') && !have_colon) {
                    str.push_back(c);
                } else if(c == '\n') {
                    if (have_colon && nums.size() == 4) {
                        ranges.emplace_back(nums[0], nums[1],nums[2], nums[3],str);
                    } else if(!nums.empty()) {
                        if(my_ticket.empty()) my_ticket = nums;
                        else other_tickets.push_back(nums);
                    }
                    nums.clear();
                    str.clear();
                    have_colon = false;
                }
            }
        }
    }

    decltype(other_tickets) valid_tickets;
    std::vector<std::unordered_set<int>> possibilities;
    for(auto t : my_ticket) {
        std::ignore = t;
        possibilities.emplace_back();
        for(size_t i = 0; i < ranges.size(); i++) possibilities.back().insert(i);
    }

    for(const auto & t : other_tickets) {
        bool valid = true;
        for(auto n : t) {
            bool found = false;
            for(auto & [min1,max1,min2,max2,_] : ranges) {
                if((n >= min1 && n <= max1)||(n>=min2 && n <= max2)) found = true;
            }
            if(!found) {
                ans1 += n;
                valid = false;
            }
        }
        if(valid) valid_tickets.push_back(t);
    }

    for(const auto & t : valid_tickets) {
        for(size_t tidx = 0; tidx < t.size(); tidx++) {
            auto n = t[tidx];
            for(size_t ridx = 0; ridx < ranges.size(); ridx++) {
                auto [min1,max1,min2,max2,_] = ranges[ridx];
                if(!((n >= min1 && n <= max1)||(n>=min2 && n <= max2))) {
                    possibilities[ridx].erase(tidx);
                }
            }
        }
    }

    bool change = true;
    while(change) {
        auto newposs = possibilities;
        for(auto & p : possibilities) {
            if(p.size() == 1) {
                auto sieve = *p.begin();
                //std::cout << "remove " << sieve << std::endl;
                for(auto & np : newposs) {
                    if(np.size() != 1) np.erase(sieve);

                }
            }
        }
        change = newposs != possibilities;
        possibilities.swap(newposs);
    }

    ans2 = 1;
    for(size_t idx = 0; idx < possibilities.size(); idx++) {
        auto & s = possibilities[idx];
        auto &str =  std::get<4>(ranges[idx]);
        if(s.size() != 1) {std::cout << " err "<< std::endl; }
        auto i = *s.begin();


        if(str.substr(0,9) == "departure")
        ans2 *= my_ticket[i];

    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
