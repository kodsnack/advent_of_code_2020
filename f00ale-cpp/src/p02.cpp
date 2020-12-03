#include "aoc.h"
#include <vector>
#include <algorithm>

std::tuple<std::string, std::string> p02(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::tuple<int,int,char, std::string>> v;

    {
        bool done = false;
        int num1 = 0, num2 = 0;
        char ch = 0;
        int num = 0;
        int len = 0;
        int cnt = 0;
        int cnt2 = 0;
        while(!done) {
            char c;
            is.get(c);
            if(!is.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else if (c >= 'a' && c <= 'z') {
                if(!ch) {
                    ch = c;
                } else {
                    len++;
                    if(ch == c) {
                        cnt++;
                        if(len == num1 || len == num2) cnt2++;
                    }
                }
            } else if(c == '\n') {
                if(num1 && num2 && ch && len) {
                    if(cnt >= num1 && cnt <= num2) ans1++;
                    if(cnt2 == 1) ans2++;

                }
                cnt = cnt2 = 0;
                num = num1 = num2 = 0;
                ch = 0;
                len = 0;
            } else {
                if(num && num1) {
                    num2 = num;
                } else if (num) {
                    num1 = num;
                }
                num = 0;
            }

        }
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
