// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <regex>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

struct Machine
{
	uint64_t maskValue{};
	uint64_t maskMask{};
	unordered_map<uint64_t, uint64_t> mem;
};

void evalLine(Machine& state, const string& line, int version)
{
	regex re("(mask = ([X10]+))|(mem\\[(\\d+)\\] = (\\d+))");
	smatch m;
	regex_match(line, m, re);
	if(m[1].matched)
	{
		auto mask = m[2].str();
		state.maskValue = 0;
		state.maskMask = 0;
		for(auto c : mask)
		{
			state.maskMask <<= 1;
			state.maskValue <<= 1;
			if(version == 1)
			{
				if(c == 'X')
				{
				} else if(c == '0')
				{
					state.maskMask |= 1;
				} else if(c == '1')
				{
					state.maskMask |= 1;
					state.maskValue |= 1;
				}
			} else // version 2
			{
				if(c == 'X')
				{
					state.maskMask |= 1;
				} else if(c == '0')
				{
				} else if(c == '1')
				{
					state.maskValue |= 1;
				}
			}
		}
	} else if(m[3].matched)
	{
		uint64_t addr = stoll(m[4].str());
		uint64_t val = stoll(m[5].str());
		if(version == 1)
		{
			uint64_t v1 = val & (-1LL ^ state.maskMask);
			state.mem[addr] = v1 | state.maskValue;
		} else // version 2
		{
			addr = addr | state.maskValue;
			auto x = state.maskMask;
			if(x != 0)
			{
				vector<uint64_t> vals{addr};
				for(uint64_t bitv = 1; x != 0; bitv <<= 1, x >>= 1)
				{
					if(x & 1)
					{
						auto vals2 = vals;
						for(auto& val2 : vals2)
						{
							val2 |= bitv;
						}
						for(auto& val : vals)
						{
							val = (val & (-1LL ^ bitv));
						}
						copy(begin(vals2), end(vals2), back_inserter(vals));
					}
				}
				for(auto v : vals)
				{
					state.mem[v] = val;
				}
			}
		}
	}
}

void evalLines(Machine& s, const vector<string>& lines, int v)
{
	for(auto& line : lines)
	{
		evalLine(s, line, v);
	}
}

auto runFile(const path& inputFile, int version)
{
	Machine s;
	evalLines(s, readLines(inputFile), version);

	uint64_t sum{};
	for(auto [_, v] : s.mem)
	{
		sum += v;
	}
	return sum;
}

auto solve_part1(const path& inputFile)
{
	return runFile(inputFile, 1);
}

auto solve_part2(const path& inputFile)
{
	return runFile(inputFile, 2);
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 165);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample2.txt")) == 208);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
