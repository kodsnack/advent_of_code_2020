//
//  main.cpp
//  AdventOfCode201223_1
//
//  Created by Kjell-Olov Högdal on 2020-12-23.
//

#include <iostream>
#include <string>
#include <algorithm>

char const* pExample1 = R"(32415)";
char const* pExample2 = R"(389125467)";
char const* pPuzzleInput = R"(463528179)";

using Dummy = int;

int main(int argc, const char * argv[]) {
    // Try using a simple string!!
    // std::string sCups {pExample2};
    // Use puzzle input
    std::string sCups {pPuzzleInput};
    for (int i = 0; i<100; ++i) {
        std::cout << "\nCups : " << sCups;
        /*
         1) The crab picks up the three cups that are immediately clockwise of the current cup. They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
         */
        std::string sPicked = sCups.substr(1,3);
        sCups.erase(1,3);
        std::cout << "\npicked : " << sPicked;
        std::cout << "\nRemaining cups : " << sCups;
        /*
         2) The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
         */
        std::string::size_type pos {std::string::npos};
        char next = sCups[0];
        do {
            next -= 1;
            if (next <= '0') next = '9';
            pos =  sCups.find(next);
        } while (pos == std::string::npos);
        std::cout << "\nSelected : " << sCups.substr(pos,1);
        /*
         3) The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
         */
        sCups.insert(pos+1, sPicked);
        std::cout << "\npicked reinserted : " << sCups;
        /*
         4) The crab selects a new current cup: the cup which is immediately clockwise of the current cup.
         */
        std::rotate(sCups.begin(), sCups.begin()+1,sCups.end());
        std::cout << "\nCups for next round : " << sCups;
    }
    // 5 (8) 3  7  4  1  9  2  6
    // 837419265
    while (sCups[0] != '1') {
        std::rotate(sCups.begin(), sCups.begin()+1, sCups.end());
    }
    // 67384529
    // 67384529
    std::cout << "\nAnswer is : " << sCups.substr(1,sCups.size()-1);
    std::cout << "\n\n";
    return 0;
}
