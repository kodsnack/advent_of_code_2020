// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <common/tuple_hash.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <filesystem>
#include <iostream>
#include <set>
#include <stdint.h>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>
#include <optional>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

using Grid = vector<string>;

struct Connection
{
	int targetId;
	int targetSide;
};

bool operator<(const Connection& c1, const Connection& c2)
{
	return tie(c1.targetId, c1.targetSide) < tie(c2.targetId, c2.targetSide);
}

struct Tile
{
	int id;
	Grid grid;

	array<optional<Connection>, 4> connections;
};

using Tiles = unordered_map<int, Tile>;

auto parseLines(const vector<string>& lines)
{
	int i{};

	auto readTile = [&lines, &i]() -> optional<Tile> {
		if(i >= lines.size())
			return {};
		auto id = stoi(lines[i].substr(5));
		++i;
		vector<string> grid;
		for(; i < lines.size(); ++i)
		{
			if(lines[i].empty())
				break;
			grid.push_back(lines[i]);
		}
		return Tile{id, grid};
	};

	Tiles tiles;
	while(i < lines.size())
	{
		if(lines[i].empty())
		{
			i++;
			continue;
		}

		auto tile = readTile();
		if(!tile)
			break;
		tiles[tile->id] = *tile;
	}

	return tiles;
}

char readAt(const Tile& tile, int side, int i)
{
	auto& grid = tile.grid;
	auto w = grid.size();

	if(side == 0)
		return grid[0][i];
	else if(side == 1)
		return grid[i][w - 1];
	else if(side == 2)
		return grid[w - 1][i];
	else if(side == 3)
		return grid[i][0];

	else if(side == 4)
		return grid[0][w - 1 - i];
	else if(side == 5)
		return grid[w - 1 - i][w - 1];
	else if(side == 6)
		return grid[w - 1][w - 1 - i];
	else
		return grid[w - 1 - i][0];
}

bool isConnection(const Tile& tile1, int side1, const Tile& tile2, int side2)
{
	auto w = tile1.grid.size();
	for(int i = 0; i < w; ++i)
	{
		if(readAt(tile1, side1, i) != readAt(tile2, side2, i))
			return false;
	}
	return true;
}

bool isConnectedTo(const Tile& tile1, const Tile& tile2)
{
	for(int i = 0; i < 4; ++i)
	{
		if(tile1.connections[i])
		{
			if(tile1.connections[i]->targetId == tile2.id)
				return true;
		}
	}
	return false;
}

bool tryConnect(Tile& tile1, Tile& tile2)
{
	int w = tile1.grid.size();

	for(int side1 = 0; side1 < 4; ++side1)
	{
		for(int side2 = 0; side2 < 8; ++side2)
		{
			bool b = isConnection(tile1, side1, tile2, side2);
			if(b)
			{
				Connection c1;
				c1.targetId = tile2.id;
				c1.targetSide = side2;
				tile1.connections[side1] = c1;

				if(side2 >= 4)
				{
					side1 += 4;
					side2 -= 4;
				}
				Connection c2;
				c2.targetId = tile1.id;
				c2.targetSide = side1;
				tile2.connections[side2] = c2;

				return true;
			}
		}
	}
	return false;
}

void findConnections(Tile& tile, Tiles& tiles)
{
	for(auto& [id, candTile] : tiles)
	{
		if(candTile.id == tile.id)
			continue;
		//		if(!isConnectedTo(tile,candTile))
		tryConnect(tile, candTile);
	}
}

int numConnections(const Tile& tile)
{
	int n{};
	if(tile.connections[0])
		n++;
	if(tile.connections[1])
		n++;
	if(tile.connections[2])
		n++;
	if(tile.connections[3])
		n++;
	return n;
}

vector<int> findCorners(const Tiles& tiles)
{
	vector<int> ids;
	for(auto& [id, tile] : tiles)
	{
		if(numConnections(tile) == 2)
			ids.push_back(tile.id);
	}
	return ids;
}

auto solve_part1(const path& inputFile)
{
	auto tiles = parseLines(readLines(inputFile));
	for(auto [id, tile] : tiles)
	{
		findConnections(tile, tiles);
	}

	auto corns = findCorners(tiles);
	int64_t r{1};
	for(auto x : corns)
	{
		r *= x;
	}

	return r;
}

// -- part 2 --

char readAt(const Tile& tile, int orientation, int x, int y)
{
	vector<char> cs;
	auto& grid = tile.grid;
	auto w = grid.size();

	if(orientation == 0)
		return grid[y][x];
	else if(orientation == 1)
		return grid[x][y];
	else if(orientation == 2)
		return grid[w - 1 - y][w - 1 - x];
	else if(orientation == 3)
		return grid[w - 1 - x][y];

	else if(orientation == 4)
		return grid[y][w - 1 - x];
	else if(orientation == 5)
		return grid[x][w - 1 - y];
	else if(orientation == 6)
		return grid[w - 1 - y][x];
	else // if(orientation == 7)
		return grid[w - 1 - x][w - 1 - y];
}

using Image = unordered_map<tuple<int, int>, char>;

void writeAt(Image& image, char c, int x, int y)
{
	image[make_tuple(x, y)] = c;
}
void clearAt(Image& image, int x, int y)
{
	image.erase(make_tuple(x, y));
}
optional<char> readAt(const Image& image, int x, int y)
{
	auto p = make_tuple(x, y);
	if(image.contains(p))
	{
		return image.at(p);
	} else
	{
		return {};
	}
}

optional<bool> checkEdge(const Image& image, int n, int x1, int y1, int x2, int y2, int dx, int dy)
{
	int nmatch{};
	int noutside{};
	for(int i = 0; i < n; ++i)
	{
		auto c1 = readAt(image, x1 + dx * i, y1 + dy * i);
		auto c2 = readAt(image, x2 + dx * i, y2 + dy * i);
		if(!c1 || !c2)
			noutside++;
		else if(c1 && c2 && (c1 == c2))
			nmatch++;
	}
	if(noutside > 0)
		return {};
	else
		return nmatch == n;
}

void drawTile(Image& image, const Tile& tile, int x0, int y0, int o)
{
	int w = tile.grid.size();
	// draw tile
	for(int y = 0; y < w; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			auto c = readAt(tile, o, x, y);
			writeAt(image, c, x0 * w + x, y0 * w + y);
		}
	}
}

void clearTile(Image& image, int w, int x0, int y0)
{
	// draw tile
	for(int y = 0; y < w; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			clearAt(image, x0 * w + x, y0 * w + y);
		}
	}
}

bool tryDrawTile(Image& image, const Tile& tile, int x0, int y0)
{
	int w = tile.grid.size();
	for(int o = 0; o < 8; o++)
	{
		drawTile(image, tile, x0, y0, o);

		// check edges
		int x = x0 * w;
		int y = y0 * w;

		// top
		auto ok1 = checkEdge(image, w, x, y, x, y - 1, 1, 0);
		// bottom
		auto ok2 = checkEdge(image, w, x, y + w - 1, x, y + w, 1, 0);
		// left
		auto ok3 = checkEdge(image, w, x, y, x - 1, y, 0, 1);
		// right
		auto ok4 = checkEdge(image, w, x + w - 1, y, x + w, y, 0, 1);
		bool anyEdgeMatch = (ok1 || ok2 || ok3 || ok4);
		bool ok =
		    anyEdgeMatch && ((!ok1 || *ok1) && (!ok2 || *ok2) && (!ok3 || *ok3) && (!ok4 || *ok4));
		if(ok)
			return true;
	}
	clearTile(image, w, x0, y0);
	return false;
}

using Map = unordered_map<tuple<int, int>, int>;

int buildMap(const Tiles& tiles, Image& image, int id)
{
	set<int> tilesToDraw;
	for(auto [id, tile] : tiles)
	{
		tilesToDraw.insert(id);
	}

	auto tryDraw = [&image, &tiles, &tilesToDraw](int x, int y) {
		for(auto id : tilesToDraw)
		{
			if(tryDrawTile(image, tiles.at(id), x, y))
			{
				tilesToDraw.erase(id);
				// cout << "Draw tile " << id << " at (" << x << ", " << y << ")" << endl;
				break;
			}
		}
	};

	drawTile(image, tiles.at(id), 0, 0, 0);
	tilesToDraw.erase(id);
	int i = 1;
	for(; !tilesToDraw.empty(); i++)
	{
		auto n0 = tilesToDraw.size();

		for(int j = -i + 1; j < i; j++)
		{
			tryDraw(j, -i);
			tryDraw(-j, i);

			tryDraw(i, j);
			tryDraw(-i, -j);
		}
		int j = -i;
		tryDraw(j, -i);
		tryDraw(-j, i);
		tryDraw(i, j);
		tryDraw(-i, -j);

		auto n1 = tilesToDraw.size();
		if(n0 == n1)
		{
			cout << ">>>> FAIL <<<" << endl;
			throw "cannot draw all tiles";
		}
	}
	return i;
}

// maps coordinate from map of 8x8 tiles to map of 10x10 tiles.
constexpr int pointWithEdge1(int x)
{
	x = 10 * (x / 8) + 1 + (x % 8);
	return x;
}

struct ImageInfo
{
	int maxX;
	int maxY;
	int ori;
};

tuple<int, int> pointWithEdge(const ImageInfo& imageInfo, tuple<int, int> p /* point in no edge */)
{
	auto [x, y] = p;
	switch(imageInfo.ori)
	{
		case 0:
			return {pointWithEdge1(x), pointWithEdge1(y)};
		case 1:
			return {pointWithEdge1(x), pointWithEdge1(imageInfo.maxY - 0 - y)};
		case 2:
			return {pointWithEdge1(imageInfo.maxX - 0 - x), pointWithEdge1(y)};
		case 3:
			return {pointWithEdge1(imageInfo.maxX - 0 - x), pointWithEdge1(imageInfo.maxY - 0 - y)};
		case 4:
			return {pointWithEdge1(y), pointWithEdge1(x)};
		case 5:
			return {pointWithEdge1(y), pointWithEdge1(imageInfo.maxX - 0 - x)};
		case 6:
			return {pointWithEdge1(imageInfo.maxY - 0 - y), pointWithEdge1(x)};
		case 7:
			return {pointWithEdge1(imageInfo.maxY - 0 - y), pointWithEdge1(imageInfo.maxX - 0 - x)};
	}
	throw "invalid orientation";
}

tuple<Image, int> buildImage(const Tiles& tiles, int id0)
{
	Image image;
	int mapSize = buildMap(tiles, image, id0);

	return {image, mapSize};
}

// clang-format off
const vector<string> monster = {
	"                  # ",
	"#    ##    ##    ###",
	" #  #  #  #  #  #   " };
// clang-format on

bool isSeamonsterAt(Image& image, const ImageInfo& imageInfo, int x0, int y0)
{
	for(int y = 0; y < monster.size(); ++y)
	{
		for(int x = 0; x < monster[y].length(); ++x)
		{
			auto [xx, yy] = pointWithEdge(imageInfo, make_tuple(x0 + x, y0 + y));
			auto c = readAt(image, xx, yy);
			if(!c)
				return false;
			if(monster[y][x] == '#' && *c == '.')
			{
				return false;
			}
		}
	}
	return true;
}

void drawSeamonsterAt(Image& image, const ImageInfo& imageInfo, int x0, int y0)
{
	for(int y = 0; y < monster.size(); ++y)
	{
		for(int x = 0; x < monster[y].length(); ++x)
		{
			if(monster[y][x] == '#')
			{
				auto [xx, yy] = pointWithEdge(imageInfo, make_tuple(x0 + x, y0 + y));
				writeAt(image, 'O', xx, yy);
			}
		}
	}
}

int findAndMarkSeamonsters(Image& image, const ImageInfo& imageInfo, int mapSize)
{
	int c{};
	for(int x = -mapSize * 2; x <= mapSize * 2; ++x)
	{
		for(int y = -mapSize * 2; y <= mapSize * 2; ++y)
		{
			if(isSeamonsterAt(image, imageInfo, x, y))
			{
				// cout << "Monster at (" << x << ", " << y << ")" << endl;
				drawSeamonsterAt(image, imageInfo, x, y);
				c++;
			}
		}
	}
	return c;
}

// print with edges
void printImage(Image& image, const ImageInfo& imageInfo)
{
	for(int y = 0; y <= imageInfo.maxY; ++y)
	{
		for(int x = 0; x <= imageInfo.maxX; ++x)
		{
			auto c = readAt(image, x, y);
			if(!c)
			{
				cout << " ";
			} else
			{
				cout << *c;
			}
		}
		cout << endl;
	}
}

// print without edges
void printImage2(Image& image, const ImageInfo& imageInfo)
{
	for(int y = 0; y <= imageInfo.maxY; ++y)
	{
		for(int x = 0; x <= imageInfo.maxX; ++x)
		{
			auto [xx, yy] = pointWithEdge(imageInfo, make_tuple(x, y));
			auto c = readAt(image, xx, yy);
			if(!c)
			{
				cout << " ";
			} else
			{
				cout << *c;
			}
		}
		cout << endl;
	}
}

tuple<int, int, int, int> extents(const Image& image)
{
	int minX{}, minY{}, maxX{}, maxY{};
	for(auto [p, _] : image)
	{
		auto [x, y] = p;
		minX = std::min(x, minX);
		minY = std::min(y, minY);

		maxX = std::max(x, maxX);
		maxY = std::max(y, maxY);
	}
	return {minX, minY, maxX, maxY};
}

auto solve_part2(const path& inputFile)
{
	auto tiles = parseLines(readLines(inputFile));
	for(auto [id, tile] : tiles)
	{
		findConnections(tile, tiles);
	}

	auto corns = findCorners(tiles);
	assert(corns.size() == 4);

	auto [image0, mapSize] = buildImage(tiles, corns[0]);
	mapSize *= 10;
	// transform map so it has 0,0 in center.
	auto [minX, minY, maxX, maxY] = extents(image0);
	Image image;
	for(auto [p, c] : image0)
	{
		image[make_tuple(get<0>(p) - minX, get<1>(p) - minY)] = c;
	}
	ImageInfo imageInfo;
	imageInfo.maxX = maxX - minX;
	imageInfo.maxY = maxY - minY;

	/*	cout << "Map with edges:" << endl;
	    printImage(image);
	    cout << "Map with removed:" << endl;
	    printImage2(image);*/

	int roughnessFinal{};
	for(int ori = 0; ori < 8; ori++)
	{
		imageInfo.ori = ori;
		int c = findAndMarkSeamonsters(image, imageInfo, mapSize);
		if(c == 0)
			continue;

		// cout << "Map with monsters:" << endl;
		// printImage2(image, imageInfo);

		int roughness{};
		for(int y = 0; y <= imageInfo.maxY; ++y)
		{
			for(int x = 0; x <= imageInfo.maxX; ++x)
			{
				auto [xx, yy] = pointWithEdge(imageInfo, make_tuple(x, y));
				auto c = readAt(image, xx, yy);
				if(c && *c == '#')
					roughness++;
			}
		}
		// cout << "roughness: " << roughness << endl;
		roughnessFinal = roughness;
	}
	return roughnessFinal;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 20899048083289LL);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 273);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
