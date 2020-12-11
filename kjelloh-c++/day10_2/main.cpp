// Watch live coding on Youtube https://youtu.be/SG6AT-tihAQ
//  main.cpp
//  AdventOfCode201210_2
//
//  Created by Kjell-Olov Högdal on 2020-12-11.
//
#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm>

enum e_input_part {
   e_example_1
  ,e_example_2
  ,e_puzzle_data
};
const e_input_part input_part = e_puzzle_data;
extern char const* pExample1Data;
extern char const* pExample2Data;
extern char const* pData;
using SteppingStone = unsigned int; // I.e., original problem "adaptor"
using WalkPathCounter = std::uint64_t;
using SteppingStones = std::vector<SteppingStone>;
using PathCountStashes = std::map<SteppingStone,WalkPathCounter>;

/**
 In original puzzle this would be Connector-er or something along those lines ;)
 Connecting adaptors to reach from 0 jolts outlet to built in adaptor corresponds
 to this "walker" finding paths to step on stones to reach the "end stone"
 */
class SteppingStoneWalker {
public:
    SteppingStoneWalker(SteppingStones& stepping_stones)
    : m_stepping_stones{stepping_stones} { m_path_count_stashes[0] = 1;}
    
    bool operator++() {
        ++m_index;
        std::cout << "\noperator++";
        std::cout << "\n\tindex " << m_index << " is stone " << m_stepping_stones[m_index];
        std::cout << "\n\tpaths_count_stash[" << m_stepping_stones[m_index] << "] is " << m_path_count_stashes[m_stepping_stones[m_index]];
        // Count number of ways to take the next step
        // stach number of ways to reach current stepping_stone into the stones reachable
        // Then step to the nearest stepping stone
        SteppingStones reachable_stones {};
        std::cout << "\n\treachable_stones ";
        for (int index = m_index+1; index <= m_index+3;index++) {
            // stones 1,2 or 3 units away are reachable
            if (m_stepping_stones[index] - m_stepping_stones[m_index] <= 3) {
                reachable_stones.push_back(m_stepping_stones[index]);
                std::cout << "," << m_stepping_stones[index];
            }
        }
        // Now add our stash to the stach of reachable stepping stones
        for (auto const& st : reachable_stones) {
            m_path_count_stashes[st] += m_path_count_stashes[m_stepping_stones[m_index]];
            std::cout << "\n\tstash[" << st << "] += stash[" << m_stepping_stones[m_index] << "] = " << m_path_count_stashes[st];
        };
        return (m_index < m_stepping_stones.size()-1);
    }
    WalkPathCounter current_paths_count() {return m_path_count_stashes[m_stepping_stones[m_index]];}
private:
    int m_index {-1};
    PathCountStashes m_path_count_stashes;
    SteppingStones m_stepping_stones;
};

int main(int argc, const char * argv[]) {
    std::basic_istringstream<char> in_example1{pExample1Data};
    std::basic_istringstream<char> in_example2{pExample2Data};
    std::basic_istringstream<char> in_full{pData};
        
    std::basic_istringstream<char> in {};
    switch (input_part) {
        case e_example_1:
            in = std::move(in_example1); // Expect 8 valid permutations
            break;
        case e_example_2:
            in = std::move(in_example2); // Expect 19208 valid permutations
            break;
        case e_puzzle_data:
            in = std::move(in_full);     // Expect ?? :)
            break;
    }
    SteppingStones stepping_stones {}; stepping_stones.push_back(0);
    SteppingStone stepping_stone {};
    while (in >> stepping_stone) {
        stepping_stones.push_back(stepping_stone);
    }
    std::sort(stepping_stones.begin(), stepping_stones.end());
    std::cout << "\nStepping Stones:";
    for (auto const& st : stepping_stones) {
        std::cout << "," << st;
    }
    SteppingStoneWalker walker {stepping_stones};
    while (++walker);
    std::cout << "\n\nNumber of ways to walk to built in device adapter = " << walker.current_paths_count();
    std::cout<< "\n\nBYE:";
    return 0;
}

char const* pExample1Data = R"(16
10
15
5
1
11
7
19
6
12
4)";
char const* pExample2Data = R"(28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3)";
char const* pData = R"(70
102
148
9
99
63
40
52
91
39
55
28
54
22
95
61
118
35
14
21
129
82
137
45
7
87
81
25
3
108
41
11
145
18
65
80
115
29
136
42
97
104
117
141
62
121
23
96
24
128
48
1
112
8
34
144
134
116
58
147
51
84
17
126
64
68
135
10
77
105
127
73
111
90
16
103
109
98
146
123
130
69
133
110
30
122
15
74
33
38
83
92
2
53
140
4)";

