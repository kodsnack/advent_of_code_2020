// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <filesystem>
#include <functional>
#include <iostream>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

// Part 1 ------------------------------------------------------------------------------------------

using Space = tuple<int, int, int, int, int, int, set<tuple<int, int, int>>>;

Space next(const Space s0)
{
	auto& [minx0, maxx0, miny0, maxy0, minz0, maxz0, d0] = s0;
	Space s;
	int minx = 0, maxx{}, miny{}, maxy{}, minz{}, maxz{};

	for(int x = minx0 - 1; x <= maxx0 + 1; ++x)
	{
		for(int y = miny0 - 1; y <= maxy0 + 1; ++y)
		{
			for(int z = minz0 - 1; z <= maxz0 + 1; ++z)
			{
				bool active = d0.contains(make_tuple(x, y, z));
				int n{};
				for(int dz = -1; dz <= 1; ++dz)
					for(int dy = -1; dy <= 1; ++dy)
						for(int dx = -1; dx <= 1; ++dx)
						{
							if(dx == 0 && dy == 0 && dz == 0)
								continue;
							if(d0.contains(make_tuple(x + dx, y + dy, z + dz)))
							{
								n++;
							}
						}
				bool ss{};
				if(active && (n == 2 || n == 3))
				{
					get<6>(s).insert(make_tuple(x, y, z));
					ss = true;
				} else if(!active && (n == 3))
				{
					get<6>(s).insert(make_tuple(x, y, z));
					ss = true;
				}
				if(ss)
				{
					minx = min(minx, x);
					miny = min(miny, y);
					minz = min(minz, z);
					maxx = max(maxx, x);
					maxy = max(maxy, y);
					maxz = max(maxz, z);
				}
			}
		}
	}
	get<0>(s) = minx;
	get<1>(s) = maxx;
	get<2>(s) = miny;
	get<3>(s) = maxy;
	get<4>(s) = minz;
	get<5>(s) = maxz;
	return s;
}

auto solve_part1(const path& inputFile)
{
	Space space;

	auto slice = readLines(inputFile);
	int x{}, y{}, z{};
	int minx = 0, maxx{}, miny{}, maxy{}, minz{}, maxz{};
	for(auto& row : slice)
	{
		x = 0;
		for(auto c : row)
		{
			if(c == '#')
			{
				get<6>(space).insert(make_tuple(x, y, z));
				minx = min(minx, x);
				miny = min(miny, y);
				minz = min(minz, z);
				maxx = max(maxx, x);
				maxy = max(maxy, y);
				maxz = max(maxz, z);
			}
			++x;
		}
		++y;
	}
	get<0>(space) = minx;
	get<1>(space) = maxx;
	get<2>(space) = miny;
	get<3>(space) = maxy;
	get<4>(space) = minz;
	get<5>(space) = maxz;

	for(int cycle = 0; cycle < 6; ++cycle)
	{
		space = next(space);
	}

	return get<6>(space).size();
}

// Part 2 - copy paste of part 1 more or less-------------------------------------------------------

using Space4 = tuple<int, int, int, int, int, int, int, int, set<tuple<int, int, int, int>>>;

Space4 next(const Space4 s0)
{
	auto& [minx0, maxx0, miny0, maxy0, minz0, maxz0, minw0, maxw0, d0] = s0;
	Space4 s;
	int minx = 0, maxx{}, miny{}, maxy{}, minz{}, maxz{}, minw{}, maxw{};

	for(int x = minx0 - 1; x <= maxx0 + 1; ++x)
	{
		for(int y = miny0 - 1; y <= maxy0 + 1; ++y)
		{
			for(int z = minz0 - 1; z <= maxz0 + 1; ++z)
			{
				for(int w = minw0 - 1; w <= maxw0 + 1; ++w)
				{
					bool active = d0.contains(make_tuple(x, y, z, w));
					int n{};
					for(int dw = -1; dw <= 1; ++dw)
						for(int dz = -1; dz <= 1; ++dz)
							for(int dy = -1; dy <= 1; ++dy)
								for(int dx = -1; dx <= 1; ++dx)
								{
									if(dx == 0 && dy == 0 && dz == 0 && dw == 0)
										continue;
									if(d0.contains(make_tuple(x + dx, y + dy, z + dz, w + dw)))
									{
										n++;
									}
								}
					bool ss{};
					if(active && (n == 2 || n == 3))
					{
						get<8>(s).insert(make_tuple(x, y, z, w));
						ss = true;
					} else if(!active && (n == 3))
					{
						get<8>(s).insert(make_tuple(x, y, z, w));
						ss = true;
					}
					if(ss)
					{
						minx = min(minx, x);
						miny = min(miny, y);
						minz = min(minz, z);
						minw = min(minw, w);
						maxx = max(maxx, x);
						maxy = max(maxy, y);
						maxz = max(maxz, z);
						maxw = max(maxz, w);
					}
				}
			}
		}
	}
	get<0>(s) = minx;
	get<1>(s) = maxx;
	get<2>(s) = miny;
	get<3>(s) = maxy;
	get<4>(s) = minz;
	get<5>(s) = maxz;
	get<6>(s) = minw;
	get<7>(s) = maxw;
	return s;
}

auto solve_part2(const path& inputFile)
{
	Space4 space;

	auto slice = readLines(inputFile);
	int x{}, y{}, z{}, w{};
	int minx = 0, maxx{}, miny{}, maxy{}, minz{}, maxz{}, minw{}, maxw{};
	for(auto& row : slice)
	{
		x = 0;
		for(auto c : row)
		{
			if(c == '#')
			{
				get<8>(space).insert(make_tuple(x, y, z, w));
				minx = min(minx, x);
				miny = min(miny, y);
				minz = min(minz, z);
				minw = min(minw, w);
				maxx = max(maxx, x);
				maxy = max(maxy, y);
				maxz = max(maxz, z);
				maxw = max(maxw, z);
			}
			++x;
		}
		++y;
	}
	get<0>(space) = minx;
	get<1>(space) = maxx;
	get<2>(space) = miny;
	get<3>(space) = maxy;
	get<4>(space) = minz;
	get<5>(space) = maxz;
	get<6>(space) = minw;
	get<7>(space) = maxw;

	for(int cycle = 0; cycle < 6; ++cycle)
	{
		space = next(space);
	}

	return get<8>(space).size();
}

// tests an main -----------------------------------------------------------------------------------

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 112);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 848);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
