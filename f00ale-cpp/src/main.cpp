#include <iostream>
#include <fstream>
#include <functional>
#include <chrono>
#include <iomanip>

#include "aoc.h"

int main(int argc, char **argv) {
    std::vector<std::function<std::tuple<std::string,std::string>(const std::string&)>> problems
            { p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13,
              p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25 };

    std::chrono::microseconds total_time{0};
    long idx = 0;
    if(argc > 1) idx = strtol(argv[1], nullptr, 10);

    for(size_t i = 0; i < problems.size(); i++)
    {
        auto num = i + 1;
        if(idx > 0 && num != static_cast<size_t>(idx)) continue;
        if(idx < 0 && num != problems.size()) continue;

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

            std::string check = "unknown";
            std::string answerfile = std::string("data/a") + (num < 10 ? "0" : "") + std::to_string(num) + ".txt";
            std::ifstream answers(answerfile);
            if(answers.good()) {
                std::string a1, a2;
                std::getline(answers, a1);
                std::getline(answers, a2);
                if(ret == std::tuple{a1, a2}) check = "OK";
                else if(std::get<0>(ret) == a1) check = "2 wrong";
                else if(std::get<1>(ret) == a2) check = "1 wrong";
                else check = "both wrong";
            }

            auto timeus = std::chrono::duration_cast<std::chrono::microseconds>(end-start);
            total_time += timeus;
            std::cout << std::right << std::setw(2) << num << ": " << std::setw(15) << std::get<0>(ret) <<
                    " " << std::setw(55) << std::get<1>(ret)
                    << " " << std::setw(10) << timeus.count() << "us " << check << std::endl;

        } else {
            std::cout << "Problem opening " << filename << std::endl;
        }
    }

    std::cout << std::left << std::setw(76) << "Total: " << std::right << std::setw(10) << total_time.count() << "us" << std::endl;

}
