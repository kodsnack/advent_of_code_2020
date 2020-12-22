// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <array>
#include <deque>
#include <filesystem>
#include <iostream>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

bool verbose = false;

using Decks = array<deque<int>, 2>;

Decks parseLines(const vector<string>& lines)
{
	Decks r;
	int p{-1};
	for(auto& line : lines)
	{
		if(line.empty())
			continue;
		if(line[0] == 'P')
		{
			p++;
			continue;
		}
		r[p].push_back(stoi(line));
	}
	return r;
}

optional<int> playRound(Decks& decks)
{
	auto p1 = decks[0].front();
	auto p2 = decks[1].front();
	decks[0].pop_front();
	decks[1].pop_front();
	if(p1 > p2)
	{
		decks[0].push_back(p1);
		decks[0].push_back(p2);
	} else
	{
		decks[1].push_back(p2);
		decks[1].push_back(p1);
	}
	if(decks[0].empty())
		return 1;
	if(decks[1].empty())
		return 0;
	return {};
}

auto solve_part1(const path& inputFile)
{
	auto decks = parseLines(readLines(inputFile));

	int winner{};
	while(true)
	{
		auto r = playRound(decks);
		if(r)
		{
			winner = *r;
			break;
		}
	}

	int i = 0;
	int score{};
	for(int i = 0; i < decks[winner].size(); ++i)
	{
		int mult = decks[winner].size() - i;
		score += decks[winner][i] * mult;
	}

	return score;
}

void printDeck(int p, Decks& d)
{
	cout << "Player " << (p + 1) << "'s deck: ";
	for(auto c : d[p])
	{
		cout << c << ", ";
	}
	cout << endl;
}

pair<int, Decks> playRoundV2(Decks decks, int gameId, int& gameCounter)
{
	if(verbose)
	{
		cout << "=== Game " << gameId << " ===" << endl;
	}

	set<Decks> seen;
	int round{0};
	while(true)
	{
		round++;
		if(verbose)
		{
			cout << "-- Round " << round << " (Game " << gameId << ") --" << endl;
			printDeck(0, decks);
			printDeck(1, decks);
		}

		if(seen.contains(decks))
		{
			if(verbose)
			{
				cout << "Player 1 winns directly!!!" << endl << endl;
			}
			return {0, decks};
		}

		seen.insert(decks);

		auto p1 = decks[0].front();
		auto p2 = decks[1].front();
		decks[0].pop_front();
		decks[1].pop_front();
		if(verbose)
		{
			cout << "Player 1 plays: " << p1 << endl;
			cout << "Player 2 plays: " << p2 << endl;
		}

		int winner;
		if(decks[0].size() >= p1 && decks[1].size() >= p2)
		{
			Decks decks2;
			for(int i = 0; i < p1; i++)
				decks2[0].push_back(decks[0][i]);
			for(int i = 0; i < p2; i++)
				decks2[1].push_back(decks[1][i]);

			winner = playRoundV2(decks2, ++gameCounter, gameCounter).first;
		} else
		{
			winner = (p1 > p2) ? 0 : 1;
		}

		decks[winner].push_back(winner == 0 ? p1 : p2);
		decks[winner].push_back(winner == 0 ? p2 : p1);

		if(verbose)
		{
			cout << "Player " << (winner + 1) << " wins round " << round << " of game " << gameId
			     << "!" << endl;
		}

		if(decks[0].size() == 0)
		{
			if(verbose)
			{
				cout << "The winner of game " << gameId << " is player 2!" << endl << endl;
			}
			return {1, decks};
		}
		if(decks[1].size() == 0)
		{
			if(verbose)
			{
				cout << "The winner of game " << gameId << " is player 1!" << endl << endl;
			}
			return {0, decks};
		}
	}
}

auto solve_part2(const path& inputFile)
{
	auto decks0 = parseLines(readLines(inputFile));
	int gameCounter = 0;
	auto [winner, decks] = playRoundV2(decks0, ++gameCounter, gameCounter);

	int i = 0;
	int score{};
	for(int i = 0; i < decks[winner].size(); ++i)
	{
		int mult = decks[winner].size() - i;
		score += decks[winner][i] * mult;
	}

	if(verbose)
	{
		cout << "Final winner is player " << (winner + 1) << " with score " << score << "." << endl;
	}
	return score;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 306);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 291);
}

int main(int argc, char* argv[])
{
	if(argc > 1)
		verbose = true;

	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
