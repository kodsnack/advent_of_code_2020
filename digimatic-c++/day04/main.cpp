// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <iostream>
#include <regex>
#include <string>
#include <unordered_map>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

void parseLine(unordered_map<string, string>& ps, const string& line)
{
	unordered_map<string, int> p;
	auto vs = split(regex(" "), line);
	regex re("(\\w\\w\\w)\\:(.*)");
	for(auto v : vs)
	{
		smatch m;
		if(regex_match(v, m, re))
			ps[m[1].str()] = m[2].str();
	}
}

auto parseLines(const vector<string>& lines)
{
	vector<unordered_map<string, string>> ps;
	unordered_map<string, string> p;
	for(auto& line : lines)
	{
		if(line.length() == 0)
		{
			ps.push_back(p);
			p.clear();
		}

		parseLine(p, line);
	}
	if(!p.empty())
		ps.push_back(p);
	return ps;
}

auto solve_part1(const path& inputFile)
{
	auto ps = parseLines(readLines(string(inputFile)));
	int n{};
	for(auto p : ps)
	{
		if(p.contains("byr") && p.contains("iyr") && p.contains("eyr") && p.contains("hgt") &&
		   p.contains("hcl") && p.contains("ecl") && p.contains("pid"))
			++n;
	}
	return n;
}

bool validate(const string& a, const string& b)
{
	if(a == "byr")
	{
		smatch m;
		regex re("(\\d+)");
		if(regex_match(b, m, re))
		{
			auto x = stoi(m[1].str());
			if(x >= 1920 && x <= 2002)
				return true;
		}
		return false;
	} else if(a == "iyr")
	{
		smatch m;
		regex re("(\\d+)");
		if(regex_match(b, m, re))
		{
			auto x = stoi(m[1].str());
			if(x >= 2010 && x <= 2020)
				return true;
		}
		return false;
	} else if(a == "eyr")
	{
		smatch m;
		regex re("(\\d+)");
		if(regex_match(b, m, re))
		{
			auto x = stoi(m[1].str());
			if(x >= 2020 && x <= 2030)
				return true;
		}
		return false;
	} else if(a == "hgt")
	{
		smatch m;
		regex re("(\\d+)((cm)|(in))");
		if(regex_match(b, m, re))
		{
			auto x = stoi(m[1].str());
			if((m[3].matched && x >= 150 && x <= 193) || (m[4].matched && x >= 59 && x <= 76))
				return true;
		}
		return false;
	} else if(a == "hcl")
	{
		smatch m;
		regex re("\\#([0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f])");
		if(regex_match(b, m, re))
		{
			return true;
		}
		return false;
	} else if(a == "ecl")
	{
		smatch m;
		regex re("(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)");
		if(regex_match(b, m, re))
		{
			return true;
		}
		return false;
	} else if(a == "pid")
	{
		smatch m;
		regex re("\\d\\d\\d\\d\\d\\d\\d\\d\\d");
		if(regex_match(b, m, re))
		{
			return true;
		}
		return false;
	}
	return true;
}

auto solve_part2(const path& inputFile)
{
	auto ps = parseLines(readLines(string(inputFile)));
	int n{};
	for(auto p : ps)
	{
		if(p.contains("byr") && p.contains("iyr") && p.contains("eyr") && p.contains("hgt") &&
		   p.contains("hcl") && p.contains("ecl") && p.contains("pid"))
		{
			bool hasInvalid{};
			for(auto [a, b] : p)
			{
				if(!validate(a, b))
				{
					hasInvalid = true;
					break;
				}
			}
			if(!hasInvalid)
				++n;
		}
	}

	return n;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 2);
}

TEST_CASE("validate", "[validate]")
{
	REQUIRE(validate("byr", "2002"));
	REQUIRE(!validate("byr", "2003"));

	REQUIRE(validate("hgt", "60in"));
	REQUIRE(validate("hgt", "190cm"));
	REQUIRE(!validate("hgt", "190in"));
	REQUIRE(!validate("hgt", "190"));

	REQUIRE(validate("hcl", "#123abc"));
	REQUIRE(!validate("hcl", "#123abz"));
	REQUIRE(!validate("hcl", "123abc"));

	REQUIRE(validate("ecl", "brn"));
	REQUIRE(!validate("ecl", "wat"));

	REQUIRE(validate("pid", "000000001"));
	REQUIRE(!validate("pid", "0123456789"));
}

TEST_CASE("invalid-samples", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputInvalidExamples.txt")) == 0);
}

TEST_CASE("valid-samples", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputValidExamples.txt")) == 4);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return 0;
}
