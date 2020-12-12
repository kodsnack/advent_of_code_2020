#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>

std::tuple<std::string, std::string> p12(const std::string &input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::tuple<char, int>> v;

    {
        int num = 0;
        char d = 0;
        for (const auto c : input) {
            if (c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else if (c >= 'A' && c <= 'Z') {
                d = c;
            } else {
                if (d) {
                    v.emplace_back(d, num);
                }
                num = 0;
                d = 0;
            }

        }
    }

    auto rot90 = [](int &x, int &y) {
        int tmp = x;
        x = -y;
        y = tmp;
    };
    auto rot180 = [](int &x, int &y) {
        x = -x;
        y = -y;
    };
    auto rot270 = [](int &x, int &y) {
        int tmp = x;
        x = y;
        y = -tmp;
    };

    {
        int n = 0, e = 0;
        int dn = 0, de = 1;
        for (const auto &[d, num] : v) {
            switch (d) {
                case 'N':
                    n += num;
                    break;
                case 'S':
                    n -= num;
                    break;
                case 'E':
                    e += num;
                    break;
                case 'W':
                    e -= num;
                    break;
                case 'F':
                    n += num * dn;
                    e += num * de;
                    break;
                case 'L':
                case 'R': {
                    int r = d == 'R' ? 360 - num : num;
                    switch (r) {
                        case 90:
                            rot90(de, dn);
                            break;
                        case 180:
                            rot180(de, dn);
                            break;
                        case 270:
                            rot270(de, dn);
                            break;
                        default:
                            std::cout << "rot " << num << std::endl;
                            break;
                    }
                }
                    break;

                default:
                    std::cout << "err " << d << std::endl;
                    break;
            }
        }

        ans1 = std::abs(n) + std::abs(e);
    }

    {
        int n = 0, e = 0;
        int wn = 1, we = 10;

        for (const auto &[d, num] : v) {
            switch (d) {
                case 'N':
                    wn += num;
                    break;
                case 'S':
                    wn -= num;
                    break;
                case 'E':
                    we += num;
                    break;
                case 'W':
                    we -= num;
                    break;
                case 'F':
                    n += num * wn;
                    e += num * we;
                    break;
                case 'L':
                case 'R': {
                    int r = d == 'R' ? 360 - num : num;
                    switch (r) {
                        case 90:
                            rot90(we, wn);
                            break;
                        case 180:
                            rot180(we, wn);
                            break;
                        case 270:
                            rot270(we, wn);
                            break;
                            break;
                        default:
                            std::cout << "rot " << num << std::endl;
                            break;
                    }
                }
                    break;
                default:
                    std::cout << "err " << d << std::endl;
                    break;
            }
        }
        ans2 = std::abs(n) + std::abs(e);
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
