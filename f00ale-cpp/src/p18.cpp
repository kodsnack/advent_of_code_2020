#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <stack>

std::tuple<std::string, std::string> p18(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::string> v;

    {
        std::string str;
        for (const auto c : input) {
            if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || c == '+' || c == '*' || c == '(' || c == ')') {
                str.push_back(c);
            } else if (c == '\n') {
                if (!str.empty()) v.push_back(str);
                str.clear();
            }
        }
    }

    for (const auto &s : v) {
        int64_t num = 0;
        bool add = true;
        std::stack<std::tuple<int64_t, bool>> stack;
        for (auto c : s) {
            switch (c) {
                case '(':
                    stack.emplace(num, add);
                    add = true;
                    num = 0;
                    break;
                case ')': {
                    auto[on, oa] = stack.top();
                    stack.pop();
                    if (oa) num += on;
                    else num *= on;
                }
                    break;
                case '+':
                    add = true;
                    break;
                case '*':
                    add = false;
                    break;
                case '0'...'9': {
                    int n = c - '0';
                    if (add) num += n;
                    else num *= n;
                }
                    break;
                default:
                    std::cout << "err " << c << std::endl;
                    break;
            }
        }
        ans1 += num;
    }

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
                    while (!stack.empty() && stack.top() == '+') {
                        postfix.push_back(stack.top());
                        stack.pop();
                    }
                    stack.push(c);
                    break;
                case '*':
                    while (!stack.empty() && stack.top() != '(') {
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
        ans2 += st.top();
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
