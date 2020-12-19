#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <stack>

std::tuple<std::string, std::string> p18(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::string> data;

    {
        std::string str;
        for (const auto c : input) {
            if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || c == '+' || c == '*' || c == '(' || c == ')') {
                str.push_back(c);
            } else if (c == '\n') {
                if (!str.empty()) data.push_back(str);
                str.clear();
            }
        }
    }

    auto cntr = [](const auto &v, auto &&weight) {
        int64_t ret = 0;
        for (const auto &s : v) {
            std::stack<char> stack;
            std::string postfix;
            for (auto c : s) {
                switch (c) {
                    case '(':
                        stack.push(c);
                        break;
                    case ')':
                        while (stack.top() != '(') {
                            postfix.push_back(stack.top());
                            stack.pop();
                        }
                        stack.pop();
                        break;
                    case '+':
                    case '*':
                        while (!stack.empty() && stack.top() != '(' && weight(stack.top()) >= weight(c)) {
                            postfix.push_back(stack.top());
                            stack.pop();
                        }
                        stack.push(c);
                        break;
                    case '0'...'9':
                        postfix.push_back(c);
                        break;
                    default:
                        std::cout << "err " << c << std::endl;
                        break;
                }
            }
            while (!stack.empty()) {
                if (stack.top() != '(') {
                    postfix.push_back(stack.top());
                }
                stack.pop();
            }

            std::stack<int64_t> st;
            for (auto c : postfix) {
                if (c >= '0' && c <= '9') {
                    st.push(c - '0');
                } else {
                    auto num1 = st.top();
                    st.pop();
                    auto num2 = st.top();
                    st.pop();
                    if (c == '+') st.push(num1 + num2);
                    else st.push(num1 * num2);
                }
            }
            ret += st.top();
        }
        return ret;
    };

    ans1 = cntr(data, [](char) { return 1; });
    ans2 = cntr(data, [](char c) { return c == '+' ? 2 : 1; });

    return {std::to_string(ans1), std::to_string(ans2)};
}
