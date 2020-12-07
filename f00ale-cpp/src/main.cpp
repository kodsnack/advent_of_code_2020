#include <iostream>
#include <fstream>
#include <functional>
#include <chrono>
#include <iomanip>

#include "aoc.h"

int main() {
    std::vector<std::function<std::tuple<std::string,std::string>(const std::string&)>> problems
            { p01, p02, p03, p04, p05, p06, p07 };

    std::chrono::microseconds total_time{0};

    for(size_t i = 0; i < problems.size(); i++)
    {
        auto num = i + 1;
        std::string filename = std::string("data/p") + (num < 10 ? "0" : "") + std::to_string(num) + ".txt";
        std::ifstream input(filename);
        if(input.good()) {
            std::string str;
            bool done = false;
            while(!done) {
                char buffer[1024];
                auto cnt = input.readsome(buffer, 1024);
                str.append(buffer, cnt);
                done = !(input.good() && cnt > 0);
            }
            if(str.empty() || str.back() != '\n') str.push_back('\n'); // always end with newline

            auto start = std::chrono::high_resolution_clock::now();
            auto ret = problems[i](str);
            auto end = std::chrono::high_resolution_clock::now();

            auto timeus = std::chrono::duration_cast<std::chrono::microseconds>(end-start);
            total_time += timeus;
            std::cout << std::right << std::setw(2) << num << ": " << std::setw(15) << std::get<0>(ret) <<
                    " " << std::setw(15) << std::get<1>(ret)
                    << " " << std::setw(10) << timeus.count() << "us" << std::endl;

        } else {
            std::cout << "Problem opening " << filename << std::endl;
        }
    }

    std::cout << std::left << std::setw(36) << "Total: " << std::right << std::setw(10) << total_time.count() << "us" << std::endl;

}
