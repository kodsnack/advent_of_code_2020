//
//  main.cpp
//  AdventOfCode201224_2
//
//  Created by Kjell-Olov Högdal on 2020-12-24.
//

#include <iostream>
#include <bitset>
#include <vector>
#include <map>
#include <sstream>
#include <utility>
#include <numeric>

extern char const* pThreeEntries;
extern char const* pExample1;
extern char const* pExample2;
extern char const* pData;

enum e_Direction {
    /* east, southeast, southwest, west, northwest, and northeast. These directions are given in your list, respectively, as e, se, sw, w, nw, and ne */
    undefined_direction
    ,east           // e
    ,south_east     // se
    ,south_west     // sw
    ,west           // w
    ,north_west     // nw
    ,north_east     // ne
    ,unknown_direction
};

using TilePath6StepsCode = std::bitset<6>;
using TilePath = std::vector<e_Direction>;
using StepDictionary = std::map<std::string,e_Direction>;
//using PathParsingNibbles = std::map<std::string,TilePath>;
using PathParsingNibbles = std::vector<std::pair<std::string,TilePath>>;
using TilePaths = std::vector<TilePath>;
using Coordinate = int;
using XCoordinate = Coordinate;
using YCoordinate = Coordinate;
using Vector = std::pair<Coordinate,Coordinate>;
using Vectors = std::vector<Vector>; // :-0
using Count = unsigned int;
using Tiling = std::map<YCoordinate,std::map<XCoordinate,Count>>;

/*
 six bit binary number. 64 possible combinations
 Q: Which are valid? All?
 A: Yes :). And also all permutations (But I did not use them)
 
 Q: How is each tile oriented (no north?)?
 A: They have the corners north,north east, south east, south, south west, north west :)
 
 e se sw w nw ne
 000000 ""
 000001 "ne"
 000010 "nw"
 000011 "nenw"
 000100 ...
 000101
 ...
 111101 "eseswwne"
 111110 "eseswwnw"
 111111 "eseswwnwne"

 */

TilePath tile_path(TilePath6StepsCode path_code) {
    TilePath result {};
    for (int i = path_code.size()-1; i >=0; --i) {
        bool is_set = path_code.test(i);
        e_Direction step {};
        if (is_set) {
            switch (i) {
                case 5: step = east; break;
                case 4: step = south_east; break;
                case 3: step = south_west; break;
                case 2: step = west; break;
                case 1: step = north_west; break;
                case 0: step = north_east; break;
            }
            result.push_back(step);
        }
    }
    return result;
}

std::string to_path_string(e_Direction step) {
    std::string result {};
    switch (step) {
        case east: result = "e"; break;           // e
        case south_east: result = "se"; break;     // se
        case south_west: result = "sw"; break;     // sw
        case west: result = "w"; break;           // w
        case north_west: result = "nw"; break;     // nw
        case north_east: result = "ne"; break;     // ne
    }
    return result;
}

std::string to_path_string(TilePath6StepsCode path_code) {
    // std::cout << "\nto_path_string(" << path_code << ")";
    std::string result {};
    for (int i = path_code.size()-1 ; i>=0; --i) {
        bool is_set = path_code.test(i);
        std::string nibble {};
        if (is_set) {
            switch (i) {
                case 5: nibble = "e"; break;
                case 4: nibble = "se"; break;
                case 3: nibble = "sw"; break;
                case 2: nibble = "w"; break;
                case 1: nibble = "nw"; break;
                case 0: nibble = "ne"; break;
            }
            result += nibble;
            // std::cout << "\n\t" << result;
        }
    }
    return result;
}

std::string to_string(Vector const& v) {
    std::string result = std::string{"{x:"} + std::to_string(v.first) + ",y:" + std::to_string(v.second) + "}";
    return result;
}

std::string to_path_string(TilePath const& path) {
    std::string result {};
    for (auto const& step : path) {
        result += to_path_string(step) + " ";
    }
    return result;
}

// bit pattern 1..63 to step path (permutations NOT included)
PathParsingNibbles parse_nibbles_dictionary() {
    // std::cout << "\ndictionary";
    PathParsingNibbles possible_paths {};
    for (unsigned int i = 1; i<64 ; i++) {
        std::bitset<6> path_code {i};
        auto path_string = to_path_string(path_code);
        possible_paths.push_back({path_string,tile_path(path_code)});
    }
    std::sort(possible_paths.begin(), possible_paths.end(), [](auto const& e1,auto const& e2) {
        return (e2.first.size() < e1.first.size());
    });
    return possible_paths;
}

TilePath parse_line(std::string sEntry,PathParsingNibbles& path_nibbles) {
    TilePath tile_path {};
    std::cout << "\nparse : " << sEntry;
    while (sEntry.size() > 0) {
        bool matched {false};
        for (auto const& entry : path_nibbles) {
            if (sEntry.find(entry.first) == 0) {
                tile_path.insert(tile_path.end(), entry.second.begin(),entry.second.end());
                sEntry.erase(0,entry.first.size());
                matched = true;
                break;
            }
        }
        if (matched == false) {
            // Failed!
            std::cout << "{nPARSE FAIL for : " << sEntry;
            break;
        }
    }
    std::cout << "\n\tresult:" << to_path_string(tile_path);
    return tile_path;
}

Vector direction_v(e_Direction dir) {
    Vector result {0,0}; //
    // x = right, y = down
    switch (dir) {
        case east: result = {2,0}; break;           // e
        case south_east: result = {1,1}; break;     // se
        case south_west: result = {-1,1}; break;     // sw
        case west: result = {-2,0}; break;           // w
        case north_west: result = {-1,-1}; break;     // nw
        case north_east: result = {1,-1}; break;     // ne
    }
    return result;
}

Vector walk(TilePath const& path) {
    Vector result {0,0};
    Vector start {0,0};
    std::cout << "\naccumulate : " << to_path_string(path) << " ";
    result = std::accumulate(path.begin(), path.end(),start,[](auto const& acc,auto const& step){
        Vector result;
        auto v = direction_v(step);
        result.first = acc.first + v.first;
        result.second = acc.second + v.second;
        std::cout << " {" << result.first << "," << result.second << "}";
        return result;
    });
    std::cout << " to {x:" << result.first << ",y:" << result.second << "}";
    return result;
}

// Vectors to neghboyrs of v
Vectors search_space(Vector const& v) {
    Vectors result;
    for (e_Direction dir = static_cast<e_Direction>(undefined_direction+1); dir < unknown_direction; dir = static_cast<e_Direction>(dir + 1)) {
        Vector dir_v = direction_v(dir);
        result.push_back({v.first + dir_v.first,v.second + dir_v.second});
    }
    return result;
}

bool is_black_tile(Vector const& v,Tiling& tiling) {
    return (tiling[v.second][v.first] % 2 > 0); // odd number is black
}

Count const black {1};
Count const white {0};

void set_tile(Vector const& v,Count count,Tiling& tiling) {
    tiling[v.second][v.first] = count;
}

Count black_tiles_count(Tiling const& tiling) {
    Count result {0};
    for (auto const& y_entry : tiling) {
        for (auto const& x_entry : y_entry.second) {
            Vector v {x_entry.first,y_entry.first};
            bool is_black {x_entry.second % 2 > 0};
            if (is_black) {
                result += 1;
            }
        }
    }
    return result;
}

// v must be a black tile
void increment_neigbours_count(Vector const& v,Tiling& tiling) {
    auto v_neighbours {search_space(v)};
    for (auto const& neighbour_v : v_neighbours) {
        tiling[neighbour_v.second][neighbour_v.first] += 1;
    }
}

Count solve_part2(Vectors const& tiles,Tiling& tiling) {
    std::cout << "\nsolve_part2 enters with black tiles count " << ": " << black_tiles_count(tiling);
    // In Part 2 we need to "fill in" the white tiles too
    // "The lobby is large enough to fit whatever pattern might need to appear there."
    Count result {};
    
    // Do this:
    // 4. repeat 0..3 the required number of days :)
    Count const DAY_COUNT {100};
    for (int i = 1;i<=DAY_COUNT;++i) {
        // 0. create an intermediate tilings copy with zero counts
        Tiling intermediate_tiling {};
        for (auto const& y_entry : tiling) {
            for (auto const& x_entry : y_entry.second) {
                intermediate_tiling[y_entry.first][x_entry.first] = 0; // Zero out count
            }
        }

        // 1. for each black tile in main tilings - increment the neighbour count for its neigbours in intermediate tilings :)
        for (auto const& y_entry : tiling) {
            for (auto const& x_entry : y_entry.second) {
                Vector v {x_entry.first,y_entry.first};
                if (is_black_tile(v, tiling)) {
                    increment_neigbours_count(v, intermediate_tiling);
                }
            }
        }
        // 2. for every tile now in intermediate tilings - adjust count to indicate while or black tiles
        for (auto& y_entry : intermediate_tiling) {
            for (auto x_iter = y_entry.second.begin();x_iter != y_entry.second.end(); ++x_iter) {
                Vector v {x_iter->first,y_entry.first};
                auto count = x_iter->second;
                // count is now the number of black neighbours
                // OUUPS! We need to update intermediate ALSO for unmodified tiles!!
                if (is_black_tile(v, tiling)) {
                    // it was black :)
                    if ((count == 0) or (count > 2)) {
                        // It shall now become white
                        intermediate_tiling[v.second][v.first] = white;
                    }
                    else {
                        intermediate_tiling[v.second][v.first] = black;
                    }
                }
                else {
                    // It was white
                    if (count == 2) {
                        // it shall now become black
                        intermediate_tiling[v.second][v.first] = black;
                    }
                    else {
                        intermediate_tiling[v.second][v.first] = white;
                    }
                }
            }
        }
        // 3. set main tilings to intermediate tiling
        tiling = intermediate_tiling;
        std::cout << "\n\tDay " << i << ": " << black_tiles_count(tiling);
    }
    
    
    // 5. Count the number of black tiles :)
    result = black_tiles_count(tiling);
    return result;
}

int main(int argc, const char * argv[]) {
    // Q: How figure out what combination of directions each entry defines?
    // Try "brute force" :)
    auto path_nibbles = parse_nibbles_dictionary();
    std::basic_istringstream<char> in {pData};
    std::string sEntry {};
    TilePaths tile_paths {};
    while (std::getline(in, sEntry)) {
        auto path = parse_line(sEntry, path_nibbles);
        tile_paths.push_back(path);
    }
    Vectors tiles_to_flip {};
    for (auto const& path : tile_paths) {
        auto v = walk(path);
        tiles_to_flip.push_back(v);
    }
    std::sort(tiles_to_flip.begin(), tiles_to_flip.end(), [](auto const& v1,auto const& v2) {
        if (v1.first < v2.first) {
            return true;
        }
        else {
            return v1.second < v2.second;
        }
    });
    // Flip the tiles to flip
    Tiling tiling {};
    for (auto const& v : tiles_to_flip) {
        tiling[v.second][v.first] += 1;
    }
    std::cout << "\n\nThe tiling contains a flip count of : " << tiles_to_flip.size();
    // count the black tiles
    unsigned int init {0};
    unsigned int result = std::accumulate(tiles_to_flip.begin(), tiles_to_flip.end(), init,[&tiling](auto const& acc,auto const& v){
        std::cout << "\n\t{x:" << v.first << ",y:" << v.second << "} " << "count : " << tiling[v.second][v.first];
        return acc + (is_black_tile(v, tiling)?1:0);
    });
    std::cout << "\n\nAfter flipping the black tile count is : " << result;
    
    auto result_part2 = solve_part2(tiles_to_flip,tiling);
    std::cout << "\n\nPart 2 result = " << result_part2;
    
    std::cout << "\n\nBye :)";
    return 0;
}

// First
// sesenwnenenewseeswwswswwnenewsewsw
// sesenwnenenewseeswwswswwnenewsewsw
// e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne

// Second
// neeenesenwnwwswnenewnwwsewnenwseswesw
// neeenesenwnwwswnenewnwwsewnenwseswesw
// e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne e se sw w nw ne

// Last
// wseweeenwnesenwwwswnew
// wseweeenwnesenwwwswnew

char const* pThreeEntries = R"(esenee
esenee
sesenwnenenewseeswwswswwnenewsewsw)";
char const* pExample1 = R"(esenee)";
char const* pExample2 = R"(sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew)";
char const* pData = R"(nwnwsenwswseewwseswenwnwneswewnwnew
swneneneneneneneswne
nwseseswsesesewsewseswseswsesenenesee
ewewwwwwswwwwwnwwwwwww
swswsewswnwwswsewsenenwneneseeswnwneswse
wnenwsweswwwnwenwnwsenewnewnewwswse
swnwswwweseneeswenwwwwswesw
nwwswswsewnwwewswnwnwsenewwwsew
nwwnwnwenwnwsenwnwsenwnenwnwwnwnwnwnwnww
nwwnweseswsesesewseseeee
seseseseseseeseseswneseesesee
neenesenewneneeneneneneseneneswnewe
nwnwnwenwnwsenwnenwnwnwwnwsenenenwwnw
eeneswnenesweswnwswsenwseenweseswnesw
seseseeswesesenwseweneseeeseeew
swwswswwswswswnesenwswneswswwswwsww
nenesenenenewnwenewnewnenenese
wnwenwewnwnenwwseswnwnwnwnwnwnwwnwsw
weseeenenweeenesewswnewnwsewnwe
nwenwnenenwsesenwnwwnwnenenwswsew
seneneneneesenwewene
eseeeeeneesweweweeeeeewe
swswswwwswnwswswswseswenwsweswwsw
wnwnweswneswnweeswsewwewnwwwsw
nwnwnwnwwnwnwnwswnwnwwnenwsenwnwnw
nenenenwswweneenwnenenesewenesesenene
nenwwsewneneswneeneneneneneseenenene
nwneneneeswneseenwnewwnwnwnenesenwwnw
nenwwneeseenenesewseswnwnenwneenenene
seesesesesesenwseseeseewseeswesese
seseeswswswnwseswnwswseseswseswsesesese
sewswwwwnwwwnewnwnw
swnenwnenwnwenwwsenenwswswnw
eenenenenweswneswneneneneneeswneswnw
nwnwswwnwsenwnwwnwwenewwnwnwwnwnwnw
wswneswswswseseseseseswswsenenwseseswsewse
eseswsesewseseeweneseeseeeesesene
nwenwswneeswwswwswswswwswseneswsenesww
wwnwswwwwswewwwswwswwwsewnesw
esenenweseswneenwneneeneneneneneneswe
swwseswneseeseseseseswneseseswsesesese
nwnwnwswnenwswswnwnwenwnenwnw
senenenwnwsewwneseenewneneneswnenenenene
nenwswnwswneenwsesewnewesesenwnwnenw
nweeewweneeeenweseseesesee
swswswseseeswseswsewswswseswseneseseswnw
nwesenwswnwswnwnwwnwseswenwnwnwwenwse
nwnenwnenwnwneenwnwnwneneswwnesenene
neswsenweswwnweswsweswswsweswswswwsw
wwwwwwsewnwwseewnwnenwnwwwww
wnenenwnwswnenwnwnweeneneneswneneeswne
neswsenwnenenenenwneenwnenwnenwnenenwne
swwswseswwnewwnewsenwwwwwwwsw
wneesesenesweswneswenwnweewnenee
nweswnwnwnwnwnwnwnenwwnwnwwsenwnwnwnw
eswnwsenwseswswsesenweeseseswseeswwsww
sewseseswseneesenwseseenwswsesesesene
neeeeeeneneneneneenenenenwsw
sesesesesesesesenwnwseswseeseseeesese
weneeneseenenenenenwneneneneesenwnenesw
wseswwswswwwwswwwwnesw
newnwsesesenwnenweseewwswswswwswwsw
swenweeeeeeeeeeneeene
swswneewnwwnenwsewewwsenesenwwww
eeeseewseeeeeeesee
wsenenwseweeeeseseeeenweesenw
nwswseneseesewnweeweweneewenwse
wswwewwswwwwwwnwsww
eseeseeseeeseeseeesenweewsew
swwwswswnwewwwswwswwwsww
wnweeseswswnenenwnenesenenenesewnee
neneseswnwseseeewseseseweseeseeee
swseneesenwsesewsenwnewnewseeswnee
seneneneeneswnenenenweneswneneenenwnenew
senwwwwswnwnwnwewwnenwwnwnw
ewwwnwwnwnwwwwnwwwnwww
wneeseseeewnwseseesee
nenwnenenenwnenwnwsenwnwnenwwnenenesw
neswnwnwnwnwenwnenesenwnwnwsewewnwnw
seseseseseseswsesesewneswseswnesesw
eeneeneneneneswnwesweenenenwneneee
neeeeeeneseseneeweeweneeenee
wswwewswnwnwwnenwwsewwewwww
wwwwwwwwwsewwwwsenewwnee
swnweesewenenee
wwnwnwnenwnwwnwnwnwnwsenwnwwnwnwew
swswswswswseswswsenwswsesenesesenwswswsesw
nwwnwwnwwenwnwnwnwsw
neneneneseeneneneeeweneeneeweswne
seswnwsesesenwnwswsesenwesesweseswswe
wwwnwnweesenwnwewnwwnenww
swnwnwswewseswswewwswweswswnwsww
nwweswseewnwsenenwwewnw
wsesenwnwswwweswnewwwwne
neeeneesweenenwsenwneeenwneesenesw
wwwwwnewswww
eneneneneneneneneneneseneseneweenwne
nwnwnwnesenwnwnwnwnwnwnenw
seeeseseseeseeweseenese
swneeneswswswswnewwnwneseseneswswsene
seewneenenwnweswwseswswsenwseseswnenw
nwswsesenwnwswwswswswwwenwneseswesw
nwseseseseswswswswsw
neeeeeneeeeeneenesw
neneswneneneenenwneneenenenenene
sesweswswswnwnewsewswseseswseneenenw
neeeeenwneseneeweeeesesweene
seswnesenwseswswnwswseswsesewseseeseswse
neeeneneneneeeesweeeene
nwwswnwewwwwnwwwnwwenwsenenwsw
seswnewseseseseseswseswsesewnesesesene
swswseswwsweswwnww
enwswnenenenenwswnwwnenenwnenenwenene
swseseseseeneseseswswwseswswswswsenese
eneswsenwsenesenwwwsenwnwwnwnwsenwne
wnwnwwnwwsewnwsenwwnww
nwwnwnwenwnwnwnwnwnwwnwnwnwnw
wneswwwswsenewswnwwsesenwenwnwswsw
nweneewwwnenwwwseesewswwnwnwww
swwswswswswswseseseseseswswswseswswenwnw
neseswnwseeeeswseee
ewwwwnwnwnwsenwwwwwsewwnwwwnw
wswneseswswswswswswseswswsenenweswsesesww
ewsweswwseesewswenenweneswwnwwnwsw
neswneenenenwnwswnwnwnwnenwenwnenenwnenw
eswseeeeeeneseeeseesese
swswswswswnwnweenewswsweswnwswswswsw
wswnwewwswnwnwnwnesenwnwsesenwnwnew
neeseseeewwneneeseeseeeesweee
swwsewswswneswseswwwwswneswnwsww
wswswswnwswsweswswnwswswwswswswewe
swswsewswswwswwwswswswneseswswnwwswe
nwsenwnwnwswnwnweeswnwnwnwnwnwnenwnw
wswswwswwneswswsenwwswwswswwwesw
eeseseseseneeneeswsw
swnwswseseeseswswwswswsweswswnwneswse
senesesesesesesesenesesewneseseseseseswsesw
wnwseseseswseswnwseneeseseeswneswsesesw
nwnenwswnenenenenwsenenenwnenenenenenese
swswseswseswseweswnwseseseswwswsenesw
nenenwnewneeswnenene
neesenenwnwnwnwnenenwnwswswnwnwnwnwnwnwnw
seswswneswswseswswswswswswswswsw
wwwnwwswewwwwwnenwsewnenwnwnw
senenesesenewsenenenwwnwswnenewseneneene
senwnwsesweseseseesesesese
nenwnwsenwnenwnwnenwnwnwswnwnwnwneswneenw
wwewewwenwwwnwwwwwnwwswwww
neseseseswseseswsesesewsesesenenwseswsesese
wwnwswwswswswswweewwswwnwwswsww
eneenewsweenwneswneeeeswnweese
neswswswsweswswswsesweswwswswswswswswnw
wseswseseseseseswesesesesenewseesew
sweeneneeenewesenwenweeneeee
swwnewnenwnwwnwneenwneesenwnwnesenw
seseesewesesesenweseseseseseeese
nwnenwnwnwnenwnwnwnwseenwnwswwnwnwnwew
wnwnewnwnenwwnwswnwwwnwnwsenwnwsesenw
ewsesenewneenwswwenwwswnee
neneswnwneneenesenenwnenenene
nwnwnwsewsenwsewnwnwnwnwnwwnenenenwe
nwswsenwwseeseseenesenwswseneseswww
weeeneeeenenenenee
swnenwnwneeneseswneesweswnenwneewsw
nwwwswnenesweswwswnweeeswsenwswswe
eeseneneweeew
newswwesweeweneneeenenenenesesee
neeewenwweweseeweswenweesw
swwswnwwwsweneswwseeswswnewwww
nesenwseswnwswseeseeeeesenwswsesese
seseenweeneswseseswswswnwsenwswswsesene
neswnenwneneneneneneneneneneswne
newsesenwnwseenenwseeesweeseseesw
seesenwseseswswsenweeswswswseswswnwsw
weswwsesenwseenwnesweseeseseesesese
swswswswswswsenwneneseseneswnwswswsewswnw
seseseeswswsesesewesewsw
nwsweewnwsenwnwnwnwnwnwenwnwnwnewnwne
nwnwwswnenwnwnenwnwnwswesewnwnwnwenw
wneneneneneneneneseneneneneswnenenenene
enenenwnewneenweseseneneeneneesweew
wswswseweeeweneswenesesenwneswnese
swnwwwnwsweenwnwenenwnwnwnenw
neenweneeneseeenene
neeeeswneneenwnenewsee
eewwwnweneneseseswswnwseneswnenwswe
eseseeenwsesewseseseeseewsesesewne
eeeeeseeeseeenwsenweweeenenee
neseneswsweneswsesesenwseswseswnwsesese
wneneesewnwnenenwewenenwsenww
wswenwsenenenwnwnwswnenenwneeswnenese
swesesewwswneseseseswswswsewseesenwese
seswseseeseseswseseseswnwenwswsesesesw
nwswwweeswnwnwnwnwwwnwnwnwnwwnwwnw
swseseseseseseseseswseneseseseseneswswsene
wsenenenwsenewnee
enweswnweeeeeesweeeeeeswnee
seswswswnwswneseseenwwseswswnesewswsw
nwnwnwnenwwswnwnwnenwnenweneenwneswnwnwnw
seeenwsenwnweswenwsesenwsesweswseswnw
swswwwswswswswswswnwswesewwwwsw
swwswnwwwswswweseswsweswwnwwnesww
seswesewwnenweewsweseseswnwewew
nwwwnewwwwwwswww
nwswnwewewnwseesesewnwsewwnwswnene
seseeweseeeseseseeeseesee
eeesweeeeeneeeeneseeewnwe
sewwwwnwwnwnwwwnwnwnewwwsew
seseneswsewneesenesesewsese
enenenwnwneswnenwenenenenenenenwnwwnw
swswseseseswwnwnwswswseseswseswsesenwnese
swnenewseeswwwwwwwswwwwwwne
neenewwsweneeeeeneseneeenenwne
seeeeseneeeesesweee
eenewweneeeeeeneeeese
neswswseenwnesenweneeeeneeeenenew
nenwneseneneswnenenwnenwswnenenenwseswwse
eeseesesesenenweseeseenweeseewese
nenwneeneneneeeneneseswnenenw
wwwnewwnesewwnwwwwwwwsewwsw
eeeeenesweeeeeseswweneenene
esenwnenenwswnewnwnesenwswnewnwnwewne
wnwsewwweswswswswswnwwesw
wwwwnwwwwsenwsenewwwewwwww
nwwnwwnwnwwnweswwswwwsenwsenenwneew
nwnwewwwnwseswnw
swseseseneswsenwseseesee
eneneeswneesesweeeswweswewsenwne
nwnwsweswnwnwseeeswweneeswnwnewnw
wswwnewswwsewswnwwweswswswswsesww
ewseeesenwenesewseseesewneeeee
swswswneswsesweenwwswswwswswswswswsw
nwswswnenenwwenwswswnenwenewwnwesesw
nwsewseeswswswsweswswswswswswsenwseswnesw
neeneeeneneeswneewnwsewneesewnene
nwnenwsenenwnwnenenwnwnwnenwnwnw
senenwnwnenwnwwnwnwnwnwneneenwneswnwnwnw
wwswwswsenenwwwwneswwswswswswsweese
neeswseneeeeeswnwseewwswnwnesesenw
nesenwwwwsenwneneewsewseeenwnesw
swseeneswsesesesenwswwseswswseswseseswnw
seenenwwswwswwswnwnwewswweew
wnwnwwesweswnenwswnwnwnwwewne
wswswswseswswswswswswsenwseswnwwnwswswsw
neewseseswwnenese
eenesweeeswneeneneeneeswnwenenwe
senwseesesesesenwswsweseneseseswsesee
weesenewneneswneneseswne
nwswseeweseseeeeeseeseeswnenwe
sweswswswswsweswswswswswnwnwswsenw
wseeeneseswesesee
enwsenwneseeeeeeeeewsweeew
eneeeenenewneeneene
nwnesesewnwswnwwswnwnwnwnenwnenwwwnwnwne
eeeeeeswnweeeswseeseene
seeeseseesenwseeeseewnw
eseswswswnwswnwswswswswsweseswswswnesw
nwseseswswnenwseseseseseseseeesesesee
eweneesweneweeenenweswnenewnese
nwnesewseseseneesesenwsesweswseesesesese
wwnewnewwsewwwwnwwwwwsew
nenewnenesweneneeswenw
nwnwnwnwnwwnwnwnwnwsenwnwwnwnwnw
wnwewneeneseswswsweswseswswwseseswsesw
sweswswwswnenwswswwwwswswwswwswsene
eswneneswsenewswswswswnwswswswswewnwsw
neneeeeneeneneneeweenenenwswese
nwnenewswswnenwnenwnesenenwswnwewsenwnw
nwseswenwnwnenwsenwnwnwwwenwnwnenewsenw
nwnwnwwwnwnwsewwnwwewnwwwsenew
nwnwnenwnwnwenenenenwnwnwnewnwnw
eeseswsenweeseeeseese
nwenwnwnenwwnwnwnwwnwnwwenwswnwsenww
seseswsenesesesesesesesesese
eenenwneeneneeneeswneneeswweneneee
weswneseswnwesesesesw
nwenwnwnwnewnwwnweswsewenenwnwnesenw
nwwnwnwnwnwnwwnenwnwnwsenwsesenwnwnenwnw
neswenweneseeweeeeswesewenwse
senenwsesewseneseseswseseseseseseseesesese
eweeeeenewseswneneseneeeneneee
swswswwnwswswsweswswswswsweswswswswnwsw
nwsewnwseswseeesweeseeenwesesee
eswneswswwswsewseseswneswnesenenesesw
wwwwnwnwenwewswenw
neseenenewnenewsenenenewneneseneneenene
sweewseesewesenwnwseeseeewsenee
nwsenenenwewneneneneneneneneneeswnene
esesesenwseeeeweeseseewsesesww
swsewnwswswswneseswneswswswseswwswsesw
sweeeeeweweweeenwswewnee
swnenenenwnenwneseneneneneneneneswnenwnene
weenesenwswneneneneneneneswswnesenee
neneseenenwweswnewneneeneneewswnee
swswnwswneswneseseswnwewswswsesesenwsw
seeneeeseneeswnwseeeseeeeseesw
swwwwnewwwwwnewwwwwwswsew
swsewwwwwwwnewnwwnewnenesewsesw
swnwswwswswsweswswswsesw
nwnwnwswnenenenewswnenenenwnenesenenenw
nwnwnwswnwnwnwnwnenwnwnwnwnwnwne
seneseswseswswnwseswseeneneseswswswseswse
eneeneneneneneneswnweenewswnenenenene
seseseseesesewsesenesesenweseesesenw
wnwswswnwswwwwwseewwwwwsewwne
senwnwnwwweswwwwwnwwwnwnww
wwswnewwswwswseswwswswnweneswesww
eeeeeeseeseeeeeenweeewne
sesenesenwseseeswseswswsesesesesesenwsese
neswnwnwesweneswseswseswneeswnwnwwnesenw
swnwseneseeswewesewseswnwseseneseswnw
nwenwsenwnenwwnwnwnenewnwsesesesenwsw
nweneeeeesweeswenweee
nwwwnewwewnewwsewwwsewwwnww
nenenwswnwnwenenwnwnwnenwnwewwnwnwnwnw
nwswseseesesesesesenwsesese
neeesweeeseeseeswseseesesenwsee
nenenesenesewswnenenenwnenenenwnesewnene
wnenenenwenenenenwnene
nesenenewswnwenenewneneswenwnwnenenene
ewwswnwwnwwnwnwnwnwnwwsewwsenwnwnw
swswnwswswswswsewesese
swswswswwnenwswseswswswsweswswnwsewnew
swswseswswswsweswsenwweneswsenenwnesw
sesesesewnweswsenwenesesesesewswswsee
swswseswesewseswseseswsw
neeenwnwnwnwwnwnwwnwsewswseneswsenwnw
wwwswswwswwswnesw
neneswnenwnwenwwnwnwnwswnenenwnenwnene
newneneneweeeeeneewseweswenee
wnwsewseneseswseswneeswswswseswswnewse
nwwenwnwnesewwsenenwnweswesenwewsww
swneeneswenwswnenwse
nwnewnwnwnwnwneswneseneesenwnwnwnwswe
eeweneenweeseseeseseeeweseee
nenwenwenwneswseesesesenwnwswseswswsw
swwsenweesweneswseswswswwwswswswswse
wwwwswneseswswnewewnwnesw
wwnwswwwwneneswseseseewneneswnew
senenenenenesenenewnwenewnwnenenenwse
eeewnweeeeneeeseeeeseesee
nenewnewnenenenwnenenwseswsenenenesew
swnwsesweeswswnwnenwsewwsenwsw
newseneenwwneneseneeeeeswewnwnee
nwnwwnwsenwnwnwenwweswwsenwnenwnesww
wnweswswswseswswseswseswswseswswnenesw
wsenewswnwseswesenwnesese
nenewswseneewenwewsweeseswnenesw
swsesenwseesesesesenwseseswsesenwswswene
nwsweenwwnwwnwnwwenwwwnwwwwew
swsweswneswswswswswswswswswswswswswswnw
eenenewneneenweseseeee
wswswseswneseneswwswswnweswsewwnwswwsw
nenwnwnwnwnwnenwnwnwswswnwenwnwnwnwnenw
enwewneenwnenenwswnenwenwnwwsewsw
enenwnenenesenenenwnwwnenwwnene
neneswwwswwnewswweseswewwwww
newewswnwnwwnwnwnewnwnwwwnwnwwsw
nweseswsenwneeeeseseswnenenweewsw
wwwewwwwswwwnwwnwwwnwnwwe
swneneswnwseswnwenenwswnenwnweeenwsesee
swneswswseswneswwswswswseswnwswswswswswsw
wewswwseswwswewswwswneswwnenwsw
swswseswswswnwseswswswnwseseseseeeneswswsw
senewswwswswewnwseewwwswsweswnw
seeswswnwseswneneswnwnwwwseenwnewew
nenenenenenewweseeneeeneneenenee
wwsewwewnewwewwwswwwwww
newsesesesesenenwsewseswnwseseseseesesene
wswwweswseswswswswseswnwswswswswswnw
eneneseneneneswnwnenenenwneneneseneenene
enwnesenwswnewnwsewwwseswnwwwwew
wwneswwwswneenesesw
nenwwwwseenwwswwwwwswnwsenewnw
esesesewnwseenwswenwnwwnwswnwwnww
neneneneneneneeeneswswwnenwnenenenenene
swnewsenwnwnwnwnenwnwnwnenwwnenenweene
wwwwwwwwwwwwswwne
wseneneeenweseeswnweeesweswnwne
eseeeeeeseenwseeseeenwee
wnwwnenwesenwnenenenenwnenenwnwnwnwnwnwse
swswswswswsewseswnenwseswswseseswse
eneswsesenwnwnewnee
wwwwwswsenwwseswswwswwswwswnesw
senwnwswnwenenwnwsenwenenenenenewwnwnwne
senesenenwwesewwweeneseseseseeew
esweneswwwswswswswswswwswswswsw
seeswnenwnwswenwnwnwnw
wsenweenenwswnewwnesweseseseewnwe
nwnwnwnwnenenwnwnwnwnwnwnwneswnw
wwswwnwwwnwesewswew
sewseswweseseseneneswsewseseswswsesese
wnesewseweswswnesenenwswnewwwnwswwse
nwnenesenenesewneneswnenenenwneenewnenene
seswwseneswnwnenenenenwneneswswnwsewese
nweswswwnewswwwswwsewwwnwswwsw
nwnwwnwnwnwsenwnewnwnw
swnwwneneneeneseewesenenwnenwswnwnesene
swenwswswswswswswseseswswneswwswswesw
swneseswseseeswnwswseseswseeswwne
seseseswseswneswseseseewsesenwswseneseswse
swwwwwwsesewnwwwenwswwwwww
seeeseenweesenweeeswswnwseeeese
nwnwewwenenwnenwnenwnenenesewnenenenwne
enwneseenesewnwswswseeeseeeeseesew
weseeesweweeneswenwneenwee
swswseeseesesesesenwseneeseenweseese
swswweswswswswnewswwswswswnwswswswsw
neneweewneneneneneeneneneneenesese
neswewswwwswwswwnwwsewwwwenw
wneswnwnwnwnenesenwswswnewseenwnwnwwne
neneweesweseenwswnwwswneneseeeene
enewneswnenesenwnesewnenesenenewnwnene
neseswswswswswswswswswswsw
eeeeeneeseeneneneenenenewnewe
wwwwswswwwseewswswswnwwnwsww
nwsenwnenwnenenwswnenwnwnwnwnwsesenwnwnwnw
senenesesenwseswenwenwswswneswswseee
enenenwswnenenenwwnenwnwnwnw
swsewwnewwswswneswwwswsewswnwww
seseseswneseswwesesewsewneseseeswswse
swswwswswswseneswswwswswswswswsw
wswneswwweswewwswwswnwwswswnwsw
sesesesesewswseswswwswneseswswnesesese
seneeswseseneneneseseseseswswseseewse
eseseseeseseeeneseseseswsewseesene
eeeeswnweeeweeeeswneeewe
swneswseneswwswswnewswswseswswswwswswsw
seswenewnwnesweswswnenwnwswneneswnwwe
neswwswswwwwswsw
wswswwneswswweswwnwwwswswwwww
nwnwwnwsesenwesenenesenwnwswnwsw
neswwsenwwnenewneneewnwnenwseneneene
wneneswseneswseseseneswseseneseswseew
swswnenwswsesesesenwsesenwswneewsesenese
swwwnwwnwneswweswwwsweswwswsew
seswnwnwwseeswnwnenwneneswnweswnenwsee
nesenwnenesenesenwsenwnwnenenenenenwwnenew
nwnwneswnwnwwsesewnwnwnwnwwewnewsw
wsesewwwwneweswneswswwwswswswnwswsw
nwneenweweseeeseeweseeenwese
swneneneseneseeswwnenenenenenewnenenene
nwwnweswnewnwnwwnwwsenewwww
senwnwnwnwnwnwwesenwnwnwnwnwnwnwnwwnwne
nesesesenenwseesewswseswneswse
nwnwwwwwwseesesewnwnwnwnwnwwwnw
eeswswnwswswnwnwwseseneweseseswwnesw
swswswneswswwswswswswsw
wwwnwwwwwwwswswnwweswseneswsesww
eseenwseesenwnesewsweseseseseenwsese
nwwnwnwnwwwnewwwnwwwnwnwsenw
senewenwwswseeswwwnwnewwsweseswnw
swswsenesewseswsenwwswseneseswnenwesewsw
sewswswswswswseseswswswseswseneewnesesw
seswsesenwswseseseeseswseeseseswsewse
eeseseeseeenewsewweseneseesenw
neseneneneneneenwewweneeseneneene
eseweeseseeeeseeenwseeeneswee
sesesesenweeswewsenenwsewneseseew
seswseswswswnesenwswesenwsewwneseswswsesw
newswweswwwsewenesenwnwswwenwnwnw
nwnwnweeswwnwenenwswnwnwweneseswww
eeseeneswseeneeswseeewseseseenwse
swnwsweswseswnweseswswswswnwswwswswswnw
eseswswswseswnenwsewwneseeseswnwseenwse
eswnenenenewnwnesenenewswneneenenenene
seseseseseeesenwsesesewswneseeeesee
seeneneneeenenwneeneneswnewseeneswwnw
wwnwsewwwnwnwewwwenwwneseswww
swseneseswswnwswsenwswenw
neewenesweneeeeeneenwneneneee
nweneneswwnwnwnenwnwsenwwnwseenesene
seseseseeseseseswswseseswsenwsw
nwnenewnenenwnwnwnesenwnenwwneneenwnw
swseneenwewwswswnwneenenenwneseswnewnw
sewseneseesesesewseenweesee
newnenwswnwnwswseenwnwnwnwnwwnwswnwese
nwswnwnwneswnwnesenwneneneenwnwnenenenwne
swnwnewwwwneeswwwwswenwwsenwwese
sesenenweswwneenenweneneeneeenwne)";
