//
//  main.cpp
//  AdventOdCode201222_1
//
//  Created by Kjell-Olov Högdal on 2020-12-22.
//

#include <iostream>
#include <sstream>
#include <utility>
#include <deque>

using Dummy = int;
using Card = unsigned int;
using Deck = std::deque<Card>;
using Decks = std::pair<Deck, Deck>;
using Result = std::uint64_t;

std::ostream& operator<<(std::ostream& out,Deck const& deck) {
    for (auto const& card : deck) {
        out << " " << card;
    }
    return out;
}

struct CombatGame {
    Decks decks;
    unsigned int round {0};
    std::string sLogEntry {};
    bool operator++() {
        bool result {false};
        ++round;
        std::ostringstream log {};
        log << "\n-- round " << round << " --";
        log << "\n\tPlayer 1's deck was:" << decks.first;
        log << "\n\tPlayer 2's deck was:" << decks.second;
        Card card_1 = decks.first.front(); decks.first.pop_front();
        Card card_2 = decks.second.front(); decks.second.pop_front();
        log << "\n\tPlayer 1 played: " << card_1;
        log << "\n\tPlayer 2 played: " << card_2;

        if (card_1 > card_2) {
            // Player 1 wins
            decks.first.push_back(card_1);
            decks.first.push_back(card_2);
            log << "\n\tPlayer 1 won!";
            result =  (decks.first.size() > 0 and decks.second.size() > 0);
        }
        else if (card_2 > card_1) {
            // Player 2 wins
            decks.second.push_back(card_2);
            decks.second.push_back(card_1);
            log << "\n\tPlayer 2 won!";
            result =  (decks.first.size() > 0 and decks.second.size() > 0);
        }
        else {
            // Same card!
            log << "\n\tSame Cards!"; // should never happen?
            result = false; // break
        }
        sLogEntry = log.str();
        return result;
    }
    
    unsigned int winner_is() const {
        if (decks.first.size() == 0) return 2;
        if (decks.second.size() == 0) return 1;
        return 0;
    }
    
    Result deck_score(Deck const& deck) {
        Result result {0};
        Result card_weight {1};
        for (auto it = deck.rbegin();it != deck.rend();++it) {
            result += (*it) * card_weight++;
        }
        return result;
    }
    
    Result winners_score() {
        Result result {0};
        switch (winner_is()) {
            case 0: break;
            case 1: {result = deck_score(decks.first);} break;
            case 2: {result = deck_score(decks.second);} break;
        }
        return result;
    }
    
    friend std::ostream& operator<<(std::ostream& out,CombatGame const& game);
};

std::ostream& operator<<(std::ostream& out,CombatGame const& game) {
    if (game.winner_is() > 0) {
        out << "\n\n== post-game results ==";
        out << "\nPlayer 1's deck: " << game.decks.first;
        out << "\nPlayer 2's deck: " << game.decks.second;
    }
    else {
        out << game.sLogEntry;
    }
    return out;
}

extern char const* pExample;
extern char const* pData;

Decks decks_from_stream(std::istream& in) {
    Decks result;
    std::string sEntry {};
    bool second_deck {false};
    while (std::getline(in, sEntry)) {
        if (sEntry.find("Player") != std::string::npos) {
            // skip
        }
        else {
            // Split on empty line
            if (sEntry.size() == 0) {
                second_deck = true;
            }
            else {
                if (second_deck == false) {
                    result.first.push_back(std::stoi(sEntry));
                    std::cout << "\nFirst Deck Card " << sEntry;
                }
                else {
                    result.second.push_back(std::stoi(sEntry));
                    std::cout << "\nSecond Deck Card " << sEntry;
                }
            }
        }
        
    }
    return result;
}

int main(int argc, const char * argv[]) {
    //std::basic_istringstream<char> in {pExample};
    std::basic_istringstream<char> in {pData};
    auto decks = decks_from_stream(in);
    CombatGame combat_game {decks};
    while (++combat_game) {
        std::cout << combat_game;
    }
    std::cout << combat_game;
    std::cout << "\n\nWinner is " << combat_game.winner_is();
    
    auto result = combat_game.winners_score();
    std::cout << "\nWinners Score is: " << result;
    std::cout << "\n\nBye :)";
    return 0;
}

char const* pExample = R"(Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10)";

char const* pData = R"(Player 1:
14
23
6
16
46
24
13
25
17
4
31
7
1
47
15
9
50
3
30
37
43
10
28
33
32

Player 2:
29
49
11
42
35
18
39
40
36
19
48
22
2
20
26
8
12
44
45
21
38
41
34
5
27)";
