#include "aoc.h"
#include <vector>
#include <algorithm>

std::tuple<std::string, std::string> p06(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    {
        uint32_t any_yes = 0, all_yes = ~0u, cur = 0;
        auto handlegroup = [&]() {
            int cnt = 0;
            int cnt2 = 0;
            while(any_yes) {
                if(any_yes & 1u) cnt++;
                any_yes >>= 1u;
            }
            while(all_yes) {
                if(all_yes & 1u) cnt2++;
                all_yes >>= 1u;
            }
            ans1 += cnt;
            ans2 += cnt2;
            all_yes = ~0u;
        };
        
        for(const auto c : input) {
            if(c >= 'a' && c <= 'z') {
                cur |= (1u << (c-'a'));
            } else if(c == '\n') {
                if(!cur && any_yes) {
                    handlegroup();
                } else {
                    any_yes |= cur;
                    all_yes &= cur;
                    cur = 0;
                }
            }
        }

        if(any_yes) handlegroup();
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
