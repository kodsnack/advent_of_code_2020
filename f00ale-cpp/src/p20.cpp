#include "aoc.h"
#include <vector>
#include <algorithm>
#include <iostream>
#include <map>
#include <set>

struct piece {
    int64_t id;
    uint32_t up, le, dn, ri;
    bool xflip = false, yflip = false;
    int rots = 0;
};

auto reverse = [](uint32_t n, const uint32_t N = 10) {
    uint32_t ret = 0;
    for(uint32_t i = 0; i < N; i++) {
        auto b = n & (1u << i);
        ret |= ((b >>i)<<(N-1-i));
    }
    return ret;
};

auto rot90 = [](const piece & p) { return piece{p.id, p.ri, reverse(p.up), p.le, reverse(p.dn), p.xflip, p.yflip, p.rots+1}; };
auto flipY = [](const piece & p) { return piece{p.id, reverse(p.up), p.ri, reverse(p.dn), p.le, !p.xflip, p.yflip, p.rots}; };
auto flipX = [](const piece & p) { return piece{p.id, p.dn, reverse(p.le), p.up, reverse(p.ri), p.xflip, !p.yflip, p.rots}; };


bool solve(const std::vector<piece> & tiles, std::vector<piece> & grid, std::vector<int> & taken, const size_t side, size_t idx = 0)
{
    if((side*side) != tiles.size()) {
        std::cout << "wrong side " << side << " " << tiles.size() << std::endl;
        return false;
    }
    if(idx == tiles.size()) return true;


    size_t row = idx / side;
    size_t col = idx % side;
    for(size_t i = 0; i < tiles.size(); i++) {
        if(taken[i]) continue;

        auto p = tiles[i];
        for(int k = 0; k < 12; k++) {
            p = rot90(p);
            if(k >= 8) {
                p = flipX(p);
            } else if(k >= 4) {
                p = flipY(p);
            }

            bool ok = true;
            if(row > 0) {
                ok = p.up == grid[(row-1)*side + col].dn;
            }
            if(col > 0) {
                ok = p.le == grid[row * side + col - 1].ri;
            }

            if(ok) {
                grid[idx] = p;
                taken[i] = 1;
                auto found = solve(tiles, grid, taken, side, idx+1);
                if(found) return true;
                taken[i] = 0;
            }

            // restore
            if(k >= 8) {
                p = flipX(p);
            } else if(k >= 4) {
                p = flipY(p);
            }
        }
    }

    return false;
}

std::tuple<std::string, std::string> p20(const std::string & input) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    std::map<int, std::vector<std::string>> map;
    {
        int num = 0;
        std::string str;
        bool lastnl = false;
        std::vector<std::string> tile;
        for(const auto c : input) {
            if(c == '\n') {
                if(lastnl) {
                    if(!tile.empty()) map[num] = tile;
                    tile.clear();
                    num = 0;
                } else {
                    if(!str.empty()) tile.push_back(str);
                    str.clear();
                }
                lastnl = true;
            } else {
                lastnl = false;
            }
            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
            } else if(c == '#' || c == '.') {
                str.push_back(c);
            }

        }
    }

    std::vector<piece> tiles;

    auto toint = [](const std::string & s) {
        uint32_t ret = 0;
        for(auto c : s) {
            ret <<= 1u;
            ret |= (c == '#' ? 1u : 0u);
        }
        return ret;
    };

    for(auto & [id,tile] : map) {
        auto up = toint(tile.front());
        auto dn = toint(tile.back());

        std::string l, r;
        for(auto & s : tile) {
            l.push_back(s.front());
            r.push_back(s.back());
        }
        auto le = toint(l);
        auto ri = toint(r);

        tiles.push_back({id, up, le, dn, ri});
    }

    std::vector<int> taken(tiles.size());
    std::vector<piece> grid(tiles.size());

    constexpr size_t SIDE = 12;
    auto b = solve(tiles, grid, taken, SIDE);

    if(b) {
        ans1 = grid[0].id * grid[SIDE-1].id * grid[SIDE*(SIDE-1)].id * grid[SIDE*SIDE - 1].id;
    } else {
        std::cout << ":/" << std::endl;
    }

    auto rotimg = [](const std::vector<std::string> & img) {
        std::vector<std::string> ret(img.size());
        for(auto & row : img) {
            int out = 0;
            for(auto it = row.rbegin(); it != row.rend(); it++) {
                ret[out++].push_back(*it);
            }
        }
        return ret;
    };
    auto flipimgY = [](const std::vector<std::string> & img) {
        return std::vector<std::string>(img.rbegin(), img.rend());
    };
    auto flipimgX = [](const std::vector<std::string> & img) {
        std::vector<std::string> ret;
        for(auto & s : img) {
            ret.emplace_back(s.rbegin(),s.rend());
        }
        return ret;
    };
    auto imgcrop = [](const std::vector<std::string> & img) {
        std::vector<std::string> ret;
        for(size_t i = 1; i < img.size()-1; i++) {
            ret.emplace_back(img[i].substr(1, img[i].size()-2));
        }
        return ret;
    };
    std::vector<std::vector<std::string>> imggrid;

    for(auto & p : grid) {
        auto img = imgcrop(map[p.id]);
        for(int i = 0; i < p.rots; i++) img = rotimg(img);
        if(p.xflip) img = flipimgX(img);
        if(p.yflip) img = flipimgY(img);
        imggrid.push_back(img);

    }
    std::vector<std::string> bigpicture;
    for(size_t y = 0; y < SIDE*SIDE; y += SIDE) {
        for(size_t imgrow = 0; imgrow < imggrid[y].size(); imgrow++) {
            bigpicture.emplace_back();
            for (size_t x = 0; x < SIDE; x++) {
                bigpicture.back() += imggrid[y+x][imgrow];
            }
        }

    }

    //bigpicture = flipimgX(rotimg(bigpicture)); // to get same orientation as in example

    std::vector<std::string> monster {
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   "
    };

    std::vector<std::tuple<int,int>> points;
    for(size_t r = 0; r < monster.size(); r++) {
        for(size_t c = 0; c < monster[r].size(); c++) {
            if(monster[r][c] == '#') points.emplace_back(r,c);
        }
    }

    for(int k = 0; k < 12; k++) {
        bigpicture = rotimg(bigpicture);
        if(k >= 8) bigpicture = flipimgX(bigpicture);
        else if(k >= 4) bigpicture = flipimgY(bigpicture);

        int found = 0;

        for(size_t y = 0; y < bigpicture.size(); y++) {
            for(size_t x = 0; x < bigpicture[y].size(); x++) {
                bool ok = true;
                for(auto [dy,dx] : points) {
                    if(y+dy >= bigpicture.size() || x+dx >= bigpicture[y+dy].size() || bigpicture[y+dy][x+dx] != '#') {
                        ok = false;
                        break;
                    }
                }
                if(ok) {
                    for(auto [dy,dx] : points) {
                        bigpicture[y+dy][x+dx] = 'O';
                    }
                    found++;
                }
            }
        }

        if(found)
        {
            for(auto & s : bigpicture) for(auto c : s) if(c == '#') ans2++;
            break;
        }

        if(k >= 8) bigpicture = flipimgX(bigpicture);
        else if(k >= 4) bigpicture = flipimgY(bigpicture);
    }

    return {std::to_string(ans1), std::to_string(ans2)};
}
