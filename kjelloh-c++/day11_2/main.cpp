//  Watch live coding of this on youtube https://youtu.be/-qYdL8L5hl8
//  main.cpp
//  AdventOfCode201211_2
//
//  Created by Kjell-Olov Högdal on 2020-12-12.
//

#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <utility>
#include <vector>

//extern char const* pExample1[];
extern char const* pExample2_1[];
extern char const* pExample2_2[];
extern char const* pData;

using Index = int;
using Count = unsigned int;
using RowIndex = Index;
using ColumnIndex = Index;
using Cell = char;
using MatrixRow = std::map<ColumnIndex,Cell>;
using Matrix = std::map<RowIndex,MatrixRow>;

void print_matrix(Matrix& m) {
    for (RowIndex i {0}; i < m.size();++i) {
        std::cout << "\n";
        for (ColumnIndex j {0}; j < m[0].size();++j) std::cout << m[i][j];
    }
}

void print_matrices_diff(Matrix& m,Matrix& n) {
    for (RowIndex i {0}; i < m.size();++i) {
        std::cout << "\n\t";
        std::string sRow1 {};
        std::string sRow2 {};
        bool is_equal {true};
        for (ColumnIndex j {0}; j < m[0].size();++j) {
            is_equal = (is_equal and (m[i][j] == n[i][j]));
            sRow1 += m[i][j];
            sRow2 += n[i][j];
        }
        std::cout << "\n\t1:" << sRow1;
        std::cout << "\n\t2:" << sRow2;
        if (!is_equal) std:: cout << " *";
    }
}

bool equal_matrices(Matrix& m,Matrix& n) {
    bool result {true};
    for (RowIndex i {0}; i < m.size();++i) {
        std::string sRow1 {};
        std::string sRow2 {};
        for (ColumnIndex j {0}; j < m[0].size();++j) {
            result = (result and (m[i][j] == n[i][j]));
            sRow1 += m[i][j];
            sRow2 += n[i][j];
        }
    }
    return result;
}

struct SeaterAutomata {
    Matrix m;
    Matrix n {};
    
    bool in_bounds(RowIndex r,ColumnIndex c) {return (r < m.size()) and (c < m[0].size());}
        
    Count adjacent_occupied_count(RowIndex r,ColumnIndex c) {
        Count result {0};
        for (RowIndex i {r-1}; i <= (r+1);i++) {
            for (ColumnIndex j {c-1}; j <= (c+1);j++) {
                if (((i!=r) or (j!=c)) and in_bounds(i, j)) {
                    result += (m[i][j] == '#')?1:0;
                }
            }
        }
        return result;
    }
    
    struct Slope {
        Index x_inc;
        Index y_inc;
    };
    
    std::vector<Slope> slopes {
         {-1,0} // north
        ,{-1,1}  // north_east
        ,{0,1}  // east
        ,{1,1}  // south east
        ,{1,0}  // south
        ,{1,-1} // south west
        ,{0,-1} // west
        ,{-1,-1} // nort west
    };
    
    Cell find_seat(RowIndex r,ColumnIndex c, Slope slope) {
        Cell result {'.'};
        Index x {r + slope.x_inc};
        Index y {c + slope.y_inc};
        while (in_bounds(x, y) and (result == '.')) {
            result = m[x][y];
            x += slope.x_inc;
            y += slope.y_inc;
        }
        return result;
    }
    
    bool find_occupied(RowIndex r,ColumnIndex c, Slope slope) {
        return (find_seat(r, c, slope) == '#');
    }

    bool find_free(RowIndex r,ColumnIndex c, Slope slope) {
        return (find_seat(r, c, slope) == 'L');
    }
    
    Count adjacent_count_Seat(RowIndex r,ColumnIndex c,Cell cell) {
        Count result {0};
        // find seat defined by Cell for all required slopes
        for (auto const& slope : slopes) {
            result += (find_seat(r, c, slope) == cell)?1:0;
        }
        return result;
    }
    
    Count adjacent_occupied_count_p2(RowIndex r,ColumnIndex c) {
        Count result {0};
        // find occupied for all required slopes
        for (auto const& slope : slopes) {
            result += (find_occupied(r, c, slope))?1:0;
        }
        return result;
    }
    
    bool operator++() {
        std::cout << ".";
        // transform the seat matrix according to rules
        /*
         - If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
         - If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
         - Otherwise, the seat's state does not change.
         */
        bool result {false};
        for (RowIndex i {0}; i < m.size();++i) for (ColumnIndex j {0}; j < m[0].size();++j) {
            switch (m[i][j]) {
                case 'L': n[i][j] = (adjacent_occupied_count_p2(i,j) == 0)?'#':'L';
                break;
                case '.': n[i][j] = m[i][j]; // Nop
                break;
                case '#': {
                    n[i][j] = (adjacent_occupied_count_p2(i,j) >= 5)?'L':'#';
                    Cell cell = n[i][j];
                }
                break;
            }
        }
        result = (equal_matrices(m, n));
        m = n;
        return result;
    }
    
    Count occupied_seat_count() {
        Count result {0};
        for (RowIndex i {0}; i < m.size();++i) for (ColumnIndex j {0}; j < m[0].size();++j) {
            result += (m[i][j] == '#');
        }
        return result;
    }
};

Matrix from_string_literal(char const* pString) {
    Matrix result;
    std::basic_istringstream<char> in {pString};
    std::string sEntry;
    Index i {0};
    while (in >> sEntry) {
        Index j {0};
        for (auto const& c : sEntry) result[i][j++] = c;
        i++;
    }
    return result;
}

int main(int argc, const char * argv[]) {

    // Count test data
    if (false) {
        for (int i = 0; i < 3; ++i) {
            Matrix m = from_string_literal(pExample2_1[i]);
            Count result {0};
            bool passed {false};
            std::cout << "\n";
            print_matrix(m);
            SeaterAutomata seater {m};
            switch (i) {
                case 0: {
                    // Cell {4,3} should see 8 occupied seats
                    result = seater.adjacent_occupied_count_p2(4, 3);
                    passed = ( result==8);
                }
                break;
                case 1: {
                    // leftmost empty seat {1,1} would only see one empty seat
                    result = seater.adjacent_count_Seat(1, 1, 'L');
                    passed = (result== 1);
                }
                break;
                case 2: {
                    // The empty seat {3,3} would see no occupied seats
                    result = seater.adjacent_occupied_count_p2(3, 3);
                    passed = (result== 0);
                }
                break;
            }
            std::cout << "\nTest " << i << " result = " << result;
            if (passed) {
                std::cout << " PASSED :)";
            }
            else {
                std::cout << " FAILED 8-|";
            }
        }
    }
    
    // Test automata on testdata
    if (false) {
        Matrix init_m {from_string_literal(pExample2_2[0])};
        std::cout << "\nInit Matrix";
        print_matrix(init_m);
        SeaterAutomata test_automata {init_m};
        for (int i = 1;i<7;i++) {
            bool result {false};
            ++test_automata;
            Matrix m {from_string_literal(pExample2_2[i])};
            print_matrices_diff(test_automata.m, m);
            result = equal_matrices(test_automata.m, m);
            std::cout << "\nTest:" << i;
            if (result) {
                std::cout << " PASSED :)";
            }
            else {
                std::cout << " FAILED 8-|";
                break;
            }
        }
    }
    
    // Puzzle data
    if (true) {
        Matrix initial_seating {from_string_literal(pData)};
        print_matrix(initial_seating);
        SeaterAutomata seater_automata {initial_seating};
        while (++seater_automata == false);
        std::cout << "\n\nStable Seating Occupied Seat Count = " << seater_automata.occupied_seat_count();
    }

    std::cout << "\n\n";
    return 0;
}

char const* pExample2_1[] = {
    // empty seat below would see eight occupied seats
    R"(.......#.
    ...#.....
    .#.......
    .........
    ..#L....#
    ....#....
    .........
    #........
    ...#.....)"
    // leftmost empty seat below would only see one empty seat
    ,R"(.............
    .L.L.#.#.#.#.
    .............)"
    // The empty seat below would see no occupied seats:
    ,R"(.##.##.
    #.#.#.#
    ##...##
    ...L...
    ##...##
    #.#.#.#
    .##.##.)"
    ,R"()"
};

char const* pExample2_2[] = {
    // Initial seating example
    R"(L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL)"
    // First iteration
    ,R"(#.##.##.##
    #######.##
    #.#.#..#..
    ####.##.##
    #.##.##.##
    #.#####.##
    ..#.#.....
    ##########
    #.######.#
    #.#####.##)"
    // second iteration
    ,R"(#.LL.LL.L#
    #LLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLL#
    #.LLLLLL.L
    #.LLLLL.L#)"
    // Third iteration
    ,R"(#.L#.##.L#
    #L#####.LL
    L.#.#..#..
    ##L#.##.##
    #.##.#L.##
    #.#####.#L
    ..#.#.....
    LLL####LL#
    #.L#####.L
    #.L####.L#)"
    // Fourth Iteration
    ,R"(#.L#.L#.L#
    #LLLLLL.LL
    L.L.L..#..
    ##LL.LL.L#
    L.LL.LL.L#
    #.LLLLL.LL
    ..L.L.....
    LLLLLLLLL#
    #.LLLLL#.L
    #.L#LL#.L#)"
    // Fifth Iteration
    ,R"(#.L#.L#.L#
    #LLLLLL.LL
    L.L.L..#..
    ##L#.#L.L#
    L.L#.#L.L#
    #.L####.LL
    ..#.#.....
    LLL###LLL#
    #.LLLLL#.L
    #.L#LL#.L#)"
    // Sixth Iteration (26 occupied seats
    ,R"(#.L#.L#.L#
    #LLLLLL.LL
    L.L.L..#..
    ##L#.#L.L#
    L.L#.LL.L#
    #.LLLL#.LL
    ..#.L.....
    LLL###LLL#
    #.LLLLL#.L
    #.L#LL#.L#)"
};

char const* pExample1[] = {
    // initial seat
    R"(L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL)"
    // First Iteration
,R"(#.##.##.##
    #######.##
    #.#.#..#..
    ####.##.##
    #.##.##.##
    #.#####.##
    ..#.#.....
    ##########
    #.######.#
    #.#####.##)"
    // Second Iteration
,R"(#.LL.L#.##
    #LLLLLL.L#
    L.L.L..L..
    #LLL.LL.L#
    #.LL.LL.LL
    #.LLLL#.##
    ..L.L.....
    #LLLLLLLL#
    #.LLLLLL.L
    #.#LLLL.##)"
    // Third Iteration
,R"(#.##.L#.##
    #L###LL.L#
    L.#.#..#..
    #L##.##.L#
    #.##.LL.LL
    #.###L#.##
    ..#.#.....
    #L######L#
    #.LL###L.L
    #.#L###.##)"
    // Fourth Iteration
    ,R"(#.#L.L#.##
    #LLL#LL.L#
    L.L.L..#..
    #LLL.##.L#
    #.LL.LL.LL
    #.LL#L#.##
    ..L.L.....
    #L#LLLL#L#
    #.LLLLLL.L
    #.#L#L#.##)"
    // Fifth Iteration
    ,R"(#.#L.L#.##
    #LLL#LL.L#
    L.#.L..#..
    #L##.##.L#
    #.#L.LL.LL
    #.#L#L#.##
    ..L.L.....
    #L#L##L#L#
    #.LLLLLL.L
    #.#L#L#.##)"
};

char const* pData = R"(LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLL.LL.L.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLL.L.LLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLL..LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLL..L.LLLLLLLLLLLLLLL.LL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLL
LLLLLLLLLL.LL..LLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLL..LLLLLLLL
LL.L..L.L..L.LL..L.....LLLL..L.L.LL..L...LLLLLL...L.....L.....LL.L...LLLLL..L.LL..L..L.LL..
LLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLL..LLLLLLL.LLLLLLL.LLLLL.L.LLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL
L.LLLLLLLLLL.LLL..LLLLLLLLL.LLLLL.LLLLL..LLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLL..LLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLL..LLLLLLLLLLL.L.LL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLL.L.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLL
.......LL.L....L......L....L...L.LL..L..L...L.LL.L.LL..L..L.L.LLL.....L...LL..L..L..L.LL..L
LLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.L.LL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLL.LLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLL.LL.LLLL.LLL..LLLLLL.LLLLL.LLLLLLLLLL.LLLL.LLLLLLLLL.LLLL.LLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLL.LLLL.LLL
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
.LL..LL.L.L....L...L.........L......LL.L..........L....L...L.L..L.L.L.LLL...LLL.L.....L.L..
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLL.L.LLL.LLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL..LLLLL.LLLLL.LLLLLLLL
LL.LLLLLLL.LL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLL..LLL.LLL.LLLLL.LLLLLLLL..LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LL..LL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLL.L.LLLLLLLL.LL.LLLL.LLLL.LLLLLLLLL.LLL.LLLLL.L.LLLL.LLLLL.LLLLLLLL
LL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LL.LLLLLLL.LLLL..LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLL
......L.L......L.L.LLL.L.......L.LL..L..........LLL.LL..L.L............LL.LL.L.LL.......L..
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.L.LLL.LLLLL.LLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL..LLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.L.LLL.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLL.LLLL.LLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLL..LL..LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LL
LL.LLLLLLLLLLLLLLLL.LLL.LLL.LLLLL.L.LLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.L.LLLLLLLLLLLLLLLLLLL
LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLL..LLLLLLLLL..L.LLLLLLLL
........LL.........L........L.L.LL..L.......L....L.....L.......L...L.L..L..........L......L
LL.LLLLLLL.LLLLLLLL.LLLLLL.LLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLL.LLL.LLLLLL.LLLLL.LLLLLL.L
LLLLLLLLLL.LLLLLLLLLLLLLLLL..LLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLL.LLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLL.LL.L.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLL.L.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
...L....L.LL..L.......L..L...L.LL..LL.L....LL......L...L..L.....LL.LL....L.L..L.....L..L.L.
LLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLLL.LLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLL..LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLL.LLLLLLLLL.LLL.LL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLL.LLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLL.LLLL
L.L..LL..L..LL....L...LL...L.LL...L.L.......LL.......L.LLL....L.....L..L.L..L...L.L.L.L....
LLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LL.LLLLLLLLL.LLLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLL.L.LLLLL.LL
LLLLLLLLLLLLLL.LLLL..LLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLL..LLLLLLLL.LLLLLL.LLL.L.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLL.LL.L.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLL.LLLLL.LL.LLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
...LLL.LL.LL.LLL......L.L.L..L..L....L........L........L...LL....L.LLL....LLL..L.......LLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLLL.LL.LLLLLLLLL.L.LLL.LLL.LLL.LLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLL.LLLL.LLLL.LLL.LLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLLLL.L.LLLLLLLL..LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLL.L
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLL.LLLL.LLLLLLLLL..LLLLLLL..LLLLLL.LLLLL.LLLLLL.L
LLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.
LLLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLL.LLL.LLLLL.LLLLLLLLLLLLLLLLLLLLL
LL.LL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLL.L..LL.LLL.LLLLLLLLLLLLLL
L.L........L...LL.L..L....LLLL...LL.L......L.L....L...L.L..LL......LL......L....L..L.LL..L.
LLLLLLLLLLLLLLLLLLL..L.LLLL.L.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL..LLLLLL.L.L..LLL.LLLLL.LLLLLLLL
LLLLLLLL.L.LLLL.LLL.LLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLL.LL.L.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLL.L
LLLLLLLLL..LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.L.LLLL.LLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLL.LL.LLLLL.LLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLLLLLLLL..L.LLLLLLL.LLLLL.LLL.L.LLLL..LLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
...LL......L..LL.....L.LLLLLL..LL.LLL...L.L...LLL.LL........L......LLL..L.L.L.L..L..LLL.L..
LL.LLL.LLL.LLLLLLLL.L.LLLLL.LLLLL.L.LLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLL.L.LL.LLLL..LLLLL.LL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLL.LLLLL..LLLLLLLL.LLLLL.L.LLLLL.LLLLL.LLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LL.LLLL.
LLLLLLLLLLLLLLLLLLL.LLL.LLL.LLLL.LLLLLL.LL.L.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
.......LLL....LLL.LL.L.L.L....LLL..L..L...L.LL.LLLL...LL............LL..LL....L..L...L.LLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLL.LLL
LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLL.LLLLLL..L.LLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLL.LLLL.LLLLLLLL.LLLLLLL..LLLL.LLLLLLLLLL.LLLL.LLLLLLLLL.LLL.LLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLL..LLLL.LLL
LLLLLLLLLL.LLLLLLL..LLLLLLL.LLLLL..LLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLL..LLLLL.LLLL.LLLL.LLLLLLLLL.L.LLLLLLL.LLLLLLLLLLLL.L.LLLLLL
LLLLLLLLLL.LLLLL.L..LLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
.....L...L..L.L..L.LLLLLLLLL....L.LL...LL...L.LL..L..L.L..L.L.....L...L.LL...L.L.......L..L
LLLLLLLLLLLLLLLLLLL.LLLLL.L.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLL.LL.LLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLL.LLL.LLLLL.LLL.L.LLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL
LLLLLLLL.L.LLLLLLLL.LLLLLLL.L.LLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL
LLLLLLLLLL.LLL.LLLL.LLLLLLL.LL.LL.LLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.
LLLLLLLLLL.LLLLLLL..LLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLL..LLLL.LLLLLLL.L.LLLLLLLLL..LLLLLLLLLLLLLLLLLLL.)";
