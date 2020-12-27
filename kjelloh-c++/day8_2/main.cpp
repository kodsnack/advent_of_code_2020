//
//  main.cpp
//  AdventOfCode201208_2
//
//  Created by Kjell-Olov Högdal on 2020-12-27.
//

#include <iostream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>
#include <functional>
#include <set>

extern char const* pExample;
extern char const* pData;

enum eOp {
     eOp_undefined
    ,eOp_acc
    ,eOp_jmp
    ,eOp_nop
    ,eOp_Unknown
    
};
using Address = unsigned int;
using AddressDiff = int;
using Name = std::string;
using Argument = AddressDiff;
struct Instruction {
    Name op;
    Argument arg;
};
using Program = std::vector<Instruction>;
eOp op_enum_of_string(std::string const sOp) {
    eOp result {eOp_Unknown};
    if (sOp == "acc") {
        result = eOp_acc;
    }
    else if (sOp == "jmp") {
        result = eOp_jmp;
    }
    else if (sOp == "nop") {
        result = eOp_nop;
    }
    return result;
};
// Represent Code as a graph!
using Vertex = Address;
using Vertices = std::set<Vertex>;
struct Edge {
    Name op;
    std::pair<Vertex,Vertex> vs;
    AddressDiff diff;
};
using Edges = std::vector<Edge>;
using EdgeIndex = Edges::size_type;
struct Graph {
    Vertices vertices;
    Edges edges;
};

// Turn "program" into graph with weighted edges as the "instructions" and vertices the memory addresses
// Note: As it turned out this made the problem m o r e complicated (but I learned a lot)
Graph to_graph(Program const& program) {
    Graph result {};
    // Assemble vertices
    for (int i = 0;i<program.size();++i) {
        result.vertices.insert(i);
    }
    // Assemble edges
    for (int i = 0;i<program.size();++i) {
        Instruction const& instruction {program[i]};
        switch (op_enum_of_string(program[i].op)) {
            case eOp_undefined: break; // No error handling (assume correct program)
            case eOp_acc: {
                Edge edge {instruction.op,{i,i+1},instruction.arg};
                result.edges.push_back(edge);
            } break;
            case eOp_jmp: {
                Edge edge {instruction.op,{i,i+instruction.arg},instruction.arg};
                result.edges.push_back(edge);
            } break;
            case eOp_nop: {
                Edge edge {instruction.op,{i,i+1},instruction.arg};
                result.edges.push_back(edge);
            } break;
            case eOp_Unknown: break; // No error handling (assume correct program)
        }
    }
    return result;
}

void print_graph(std::string sLabel, Graph const& graph) {
    std::cout << "\n\nGraph <" << sLabel << "> vertex_count:" << graph.vertices.size() << " edge_count:" << graph.edges.size() ;
    std::cout << "\n\t<Vertices>";
    for (auto const& vertex : graph.vertices) {
        std::cout << " " << vertex;
    }
    std::cout << "\n\t<Edges>";
    for (auto const& edge : graph.edges) {
        std::cout << "\n\t" << edge.vs.first << " -- " << edge.op << " " << edge.diff << " --> " << edge.vs.second;
    }
}

// Returns the sub-graph connected with provided start vertex
Graph forward_connected_graph(Vertex start_vertex, Graph const& graph) {
    Graph result {}; // Start with empty graph

    // Walk the edges until we reach a visited vettex
    auto vertex = start_vertex;
    while (std::none_of(result.vertices.begin(), result.vertices.end(), [vertex](Vertex visited_vertex) {
        return (visited_vertex == vertex);
    })) {
        if (vertex < graph.vertices.size()) {
            // Walk from vertex to next vertex
            result.vertices.insert(vertex);
            for (auto const& edge : graph.edges) {
                if (edge.vs.first == vertex) {
                    vertex = edge.vs.second; // Walk the edge :)
                    result.edges.push_back(edge);
                    break;
                }
            }
        }
        else break; // failed
    }
    return result;
}

// Return the sub graph that ends at provided vertex
Graph reverse_connected_graph(Vertex const end_vertex,Graph const& graph) {
    Graph result;
    Vertex curr_vertex = end_vertex;
    while ((curr_vertex != 0) and (std::none_of(result.vertices.begin(), result.vertices.end(), [curr_vertex](Vertex const& vx) {
        return (vx == curr_vertex);
    }))) {
        result.vertices.insert(curr_vertex);
        // Walk edges backwards from provided end vertex
        // OUUPS! We can get to current vertex in multipple ways!
        // OUUPS! The sub-graph may be cyclic!
        Edges curr_edges {};
        std::copy_if(graph.edges.begin(), graph.edges.end(), std::back_inserter(curr_edges), [curr_vertex](Edge const& edge){
            return (edge.vs.second == curr_vertex); // ends at curr_vertex
        });
        // Now proceed with edges that ends at vertex
        if (curr_edges.size()>0) {
            for (auto const& edge : curr_edges) {
                curr_vertex = edge.vs.first; // backwards
                result.edges.push_back(edge);
                auto sub = reverse_connected_graph(curr_vertex, graph); // recurse
                result.vertices.insert(sub.vertices.begin(),sub.vertices.end());
                result.edges.insert(result.edges.end(), sub.edges.begin(),sub.edges.end());
            }
        }
        else {
            curr_vertex = 0; // sub-graph ends here
        }
    }
    return result;
}

bool edge_will_connect(Edge const& proposed_edge, Graph const& program_graph, Graph const& terminating_graph) {
    return std::any_of(terminating_graph.vertices.begin() , terminating_graph.vertices.end(), [&proposed_edge](Vertex vx) {
        return (vx == proposed_edge.vs.second); // proposed edge links to bertex in terminating graph :)
    });
}

using Number = unsigned int;
Number sum_acc_edges(Vertex start_vertex, Graph const& graph) {
    Number result {0};
    // Walk the graph
    Vertex vertex {start_vertex};
    while (vertex < graph.vertices.size()+1) {
        auto edge_iter = std::find_if(graph.edges.begin(), graph.edges.end(), [vertex](Edge const& edge) {
            return (edge.vs.first == vertex);
        });
        if (edge_iter != graph.edges.end()) {
            if (edge_iter->op == "acc") result += edge_iter->diff;
            vertex = edge_iter->vs.second;
        }
        else {
            break;
        }
    }
    return result;
}

int main(int argc, const char * argv[]) {
    std::basic_istringstream<char> in {pData};
    std::string sEntry;
    Program program;
    while (std::getline(in, sEntry)) {
        // split on ' '
        auto split_it = std::find(sEntry.begin(), sEntry.end(),' ');
        std::string sOp(sEntry.begin(),split_it);
        std::string sArgument(split_it,sEntry.end());
        Argument argument = std::stoi(sArgument);
        program.push_back({sOp,argument});
    }
    auto full_graph = to_graph(program);
    print_graph("Full Program", full_graph);
    auto terminating_graph = reverse_connected_graph(full_graph.vertices.size(),full_graph);
    print_graph("Terminating Program", terminating_graph);
    auto program_graph = forward_connected_graph(0, full_graph);
    print_graph("Active Program", program_graph);
    // Now, we have the problem of finding what edge in program_graph to "fix" to have it connect to terminating_graph :)
    EdgeIndex edge_index {0};
    Edge proposed_edge {};
    for (;edge_index<program_graph.edges.size();++edge_index) {
        Edge edge {program_graph.edges[edge_index]};
        proposed_edge = edge;
        if (edge.op == "jmp") {
            proposed_edge.op = "nop";
            proposed_edge.vs.second = edge.vs.first + 1;
        }
        else if (edge.op == "nop") {
            proposed_edge.op = "jmp";
            proposed_edge.vs.second = edge.vs.first + edge.diff;
        }
        else {
            // skip
        }
        if (edge_will_connect(proposed_edge, program_graph, terminating_graph)) {
            break;
        }
    }
    if (edge_index < program_graph.edges.size()) {
        // Fix the full program :)
        auto edge_iter = std::find_if(full_graph.edges.begin(), full_graph.edges.end(), [&proposed_edge](Edge const& edge){
            return (edge.vs.first == proposed_edge.vs.first);
        });
        if (edge_iter != full_graph.edges.end()) {
            std::cout << "\n\nReplaced " << edge_iter->vs.first << " -- " << edge_iter->op << " " << edge_iter->diff << " --> " << edge_iter->vs.second;
            std::cout << "\nWith     " << proposed_edge.vs.first << " -- " << proposed_edge.op << " " << proposed_edge.diff << " --> " << proposed_edge.vs.second;
            *edge_iter = proposed_edge;
            auto acc = sum_acc_edges(0,full_graph);
            std::cout << "\n\nResult Part 2 = " << acc;
        }
        else {
            // Failed to fix the graph
            std::cout << "\n\nFailed to fix the program 8-|";
        }
    }
    else {
        // Failed to fix the graph
        std::cout << "\n\nFailed to fix the program 8-|";
    }
    
    std::cout << "\n\n";
    return 0;
}

char const* pExample = R"(nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6)";

char const* pData = R"(jmp +301
acc +27
nop +299
jmp +168
acc +0
acc +48
acc -5
jmp +420
jmp +155
acc -16
acc -15
nop +582
acc -5
jmp +490
acc +10
nop +300
acc -2
acc -1
jmp +252
jmp +2
jmp +234
acc +36
acc +50
jmp +564
jmp +102
jmp +473
acc +37
acc +1
acc +20
jmp +355
acc +28
acc -14
acc +22
jmp +329
acc +44
jmp +1
jmp +23
nop +312
jmp +251
acc +12
acc +39
acc +33
jmp -21
acc +28
acc +14
acc +42
acc +33
jmp +502
jmp +123
acc +24
jmp +11
acc -8
nop +218
nop +516
jmp +244
jmp -36
nop +192
acc -11
jmp +100
jmp +299
jmp +400
acc +29
acc +13
jmp +1
acc +45
jmp +458
acc +6
acc +15
jmp +542
acc +38
acc -15
acc -15
acc -11
jmp +474
acc +0
acc +1
jmp +55
acc +14
acc +13
acc +37
acc -18
jmp +95
jmp -73
acc +25
jmp -54
jmp +413
acc -17
acc +25
acc +22
jmp +405
jmp +151
jmp +1
acc -14
acc +33
acc +30
jmp -86
acc +32
acc -3
acc +26
acc +7
jmp +493
acc +44
jmp +1
nop +357
acc +47
jmp +412
jmp +321
acc -5
jmp +12
acc +18
acc -1
acc +29
jmp +430
acc +35
acc +34
jmp +1
acc -6
jmp +184
acc -12
jmp -36
acc +22
acc -5
acc +14
acc +0
jmp +198
jmp +293
nop +76
acc +8
acc +13
jmp +464
jmp +309
acc -19
acc +27
acc -10
jmp +29
acc -17
jmp -77
acc +49
nop +224
acc +0
jmp +77
acc +35
acc +3
jmp +317
nop +27
acc +6
jmp +164
acc +10
acc +41
acc -2
acc -8
jmp +347
acc +14
nop +148
jmp +302
acc +21
acc +8
jmp +282
acc +3
acc +12
jmp +138
acc -10
jmp +311
acc -6
acc -10
acc +13
jmp +116
acc +31
acc +39
acc +28
jmp +244
nop +59
acc +16
jmp +1
jmp +436
jmp -31
acc -12
acc +43
jmp -135
acc -1
acc +35
acc +50
acc +0
jmp +398
jmp -83
acc +40
acc +26
acc -8
jmp +393
acc +28
acc -7
acc +43
jmp +231
jmp -22
acc -1
acc -18
acc +19
jmp -111
acc +38
acc +21
acc +7
jmp +134
acc +8
acc +18
acc -9
acc -7
jmp +24
acc +5
acc +0
jmp -46
acc +2
acc -12
acc -17
acc +36
jmp -88
nop -71
jmp +181
jmp -15
jmp +52
acc +15
acc +0
jmp -32
acc -6
nop +166
acc +10
acc +38
jmp +123
acc +9
jmp -151
jmp +231
jmp +1
acc +34
jmp +186
jmp +96
acc +48
acc +9
jmp +198
acc +7
acc +35
acc +22
jmp +82
nop +1
nop -156
nop -49
jmp +91
acc +8
acc -17
jmp -53
acc +29
nop +283
acc -2
nop +50
jmp +290
jmp +296
jmp +219
jmp +268
jmp -119
nop +353
jmp -157
acc +21
acc +30
jmp +345
acc -9
jmp -119
acc +0
jmp -196
acc +33
acc +0
acc +1
jmp -248
acc +15
jmp -44
acc +9
acc +46
acc +50
jmp +257
acc -6
acc -16
jmp +320
acc +35
nop -4
jmp -5
acc +30
acc +27
jmp +1
jmp +296
acc +8
acc +40
jmp +210
acc -14
acc +34
acc +42
jmp +173
acc +16
acc +47
acc +11
acc +32
jmp -206
jmp -39
acc +45
jmp +247
acc -17
nop +261
nop -254
acc +48
jmp +62
acc +50
acc +26
acc +1
jmp +130
acc -14
nop +47
acc -9
jmp -276
jmp -104
jmp +135
acc +40
jmp -296
jmp +11
acc +2
acc -17
jmp -238
acc +34
acc +37
jmp -166
nop -205
acc -4
acc +22
jmp +56
acc +1
nop -210
nop -30
acc -18
jmp -250
jmp -107
acc +45
acc +50
acc +3
acc +3
jmp -63
acc +35
jmp +1
acc -5
nop +255
jmp +254
jmp +210
acc +10
acc +7
jmp +207
acc +17
acc +25
nop -22
jmp +62
acc +35
acc +18
acc +22
acc +10
jmp -186
acc +24
acc +32
jmp -31
jmp -131
jmp -337
acc +41
acc -10
acc +42
jmp +207
acc -16
acc -14
nop -225
acc -15
jmp +70
nop -303
acc -10
acc +11
acc +17
jmp +234
acc -8
acc +33
jmp -131
acc -9
acc -12
acc +31
jmp -25
nop -277
acc +22
jmp -273
acc +19
jmp -244
acc -8
nop +220
acc +48
jmp -261
acc +23
acc +11
acc -16
jmp -47
acc +50
acc -9
acc +23
jmp -38
jmp +146
nop -168
jmp -88
acc +37
acc +36
acc +43
acc -7
jmp +147
jmp +1
acc +42
jmp -352
acc +39
jmp +76
acc +47
jmp +88
acc -2
jmp -102
acc +20
jmp +144
acc +47
acc +25
jmp -55
nop -65
jmp -375
acc +8
jmp +161
acc +46
acc +5
acc +16
jmp +53
acc +27
acc +1
jmp -6
jmp -207
acc -6
acc +27
nop +126
jmp -197
jmp -110
jmp +123
acc +13
acc +31
nop +22
acc +41
jmp -127
acc -7
nop -386
acc +0
jmp -65
jmp -306
acc +44
acc +19
acc +42
acc +29
jmp +92
acc +42
nop -156
jmp -56
jmp -346
nop +95
acc -6
acc -19
jmp -292
jmp -443
acc -12
acc -18
jmp +102
nop +35
acc +44
acc +27
nop -122
jmp +97
jmp -382
jmp -85
acc -9
nop -324
jmp -422
acc -9
acc +25
acc +38
acc -3
jmp -298
acc -2
acc +26
acc +14
jmp -252
acc +4
jmp +75
acc +17
nop -208
jmp -235
acc +19
jmp -322
acc +14
acc -3
jmp +124
jmp -221
jmp -9
acc +0
acc +45
acc -3
jmp -376
acc +20
acc -3
acc +17
acc +19
jmp -400
acc -16
acc +25
jmp -37
jmp -317
acc +31
acc +19
acc +24
acc +9
jmp -181
acc +35
jmp -488
acc -13
acc +26
acc -2
jmp -338
acc -1
acc +17
acc +44
nop -262
jmp -86
acc +17
acc -1
acc +23
jmp +79
acc -5
acc +18
jmp +1
acc +12
jmp -127
acc +1
acc +35
acc -10
acc +14
jmp -352
acc +39
nop +67
jmp -290
acc -13
jmp +41
jmp -150
jmp -121
acc +7
jmp -331
acc +42
nop -389
acc +4
jmp -7
acc -17
acc -8
acc -4
jmp -209
acc +42
acc +39
acc +43
jmp -306
acc -18
acc -16
acc +13
jmp -414
acc +3
jmp -442
nop +41
acc -12
jmp -194
jmp -503
acc -18
acc +35
acc -4
acc +18
jmp -393
nop -348
acc -7
jmp -521
acc +48
acc -19
acc -3
acc +44
jmp +2
jmp -126
nop -474
acc -9
acc -2
acc +35
jmp -587
jmp -328
acc -14
nop -468
acc +39
jmp -157
jmp -538
acc +0
nop -264
acc +19
nop -266
jmp -91
acc +20
acc +14
jmp -329
acc -11
acc +8
jmp -219
jmp -320
acc +10
acc +49
nop -603
acc +49
jmp -344
nop -356
nop -93
acc +27
acc +24
jmp -482
nop -126
nop -345
acc +6
acc +3
jmp +1)";
