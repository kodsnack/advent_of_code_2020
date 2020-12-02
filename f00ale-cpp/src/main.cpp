#include <iostream>
#include <fstream>
#include <functional>
#include <chrono>
#include <iomanip>

#include "aoc.h"

int main() {
    std::vector<std::function<std::tuple<std::string,std::string>(std::istream&)>> problems
            { p01, p02 };

    std::chrono::microseconds total_time{0};

    for(size_t i = 0; i < problems.size(); i++)
    {
        auto num = i + 1;
        std::string filename = std::string("data/p") + (num < 10 ? "0" : "") + std::to_string(num) + ".txt";
        std::fstream input(filename);
        if(input.good()) {
            auto start = std::chrono::high_resolution_clock::now();
            auto ret = problems[i](input);
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
