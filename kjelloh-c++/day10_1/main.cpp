//
//  main.cpp
//  AdventOfCode201210_1
//
//  Created by Kjell-Olov Högdal on 2020-12-10.
//

#include <iostream>
#include <sstream>
#include <set>
#include <vector>
#include <numeric>
#include <iterator>
#include <map>

extern char const* pData;

using InDataType = unsigned int;

template <class C>
void stream_container(C c) {
    for (auto const& m : c) {
        std::cout << "," << m;
    }
}

int main(int argc, const char * argv[]) {
    std::basic_istringstream<char> in {pData};
    // Assume all devices are connectable "as-is"?
    std::set<InDataType> s {};
    InDataType x;
    while (in >> x) {s.insert(x);}
    // Now s is a set so it is sorted :)
    std::cout << "\nDevice Path ";
    stream_container(s);
    std::vector<InDataType> diffs {};
    std::adjacent_difference(s.begin(), s.end(), std::back_inserter(diffs));
    // Add the 3 distance to the build in adapter :)
    diffs.push_back(3);
    std::cout << "\nDiffs ";
    stream_container(diffs);
    std::map<InDataType,InDataType> dist {};
    dist = std::accumulate(diffs.begin(), diffs.end(), std::map<InDataType,InDataType>{},[](auto& acc_map,auto x){
        ++acc_map[x]; // count the diff ;)
        return acc_map;
    });
    if (dist.size() == 2) {
        std::cout << "\n\n1 dist counts:" << dist[1] << " 3 dist counts:" << dist[3] << " product:" << dist[1]*dist[3];
    }
    
    std::cout << "\n\n";
    return 0;
}

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
