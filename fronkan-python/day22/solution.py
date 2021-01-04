from typing import Any, Iterable, List, Set, Tuple
from pathlib import Path
from collections import namedtuple
from aoc_lib.input_readers import read_chunk


def read_starting_hands(input_file: Path) -> List[List[int]]:
    return [
        list(int(num.strip()) for num in chunk[1:]) for chunk in read_chunk(input_file)
    ]


def score(cards) -> int:
    return sum(i * v for i, v in enumerate(reversed(cards), start=1))


def play_combat_round(player_hands: List[List[int]]) -> None:
    cards = [hand.pop(0) for hand in player_hands]
    winner_idx = cards.index(max(cards))
    player_hands[winner_idx].extend(sorted(cards, reverse=True))


def puzzle1(input_file: Path):
    player_hands = read_starting_hands(input_file)
    while len(player_hands) > 1:
        play_combat_round(player_hands)
        player_hands = [hand for hand in player_hands if len(hand) != 0]
    return score(player_hands[0])


class HandPlayed(RecursionError):
    pass


def play_recursive_combat_game(player_hands: List[Tuple[int]]):
    earlier_hands = set()
    player_hands = player_hands.copy()
    while len(player_hands) > 1:
        try:
            winner_idx = play_recursive_combat_round(player_hands, earlier_hands)
        except HandPlayed:
            return 0, score(player_hands[0])
        assert winner_idx in {0, 1}
        cards = [hand[0] for hand in player_hands]
        cards = cards if winner_idx == 0 else list(reversed(cards))
        player_hands[winner_idx] = (*player_hands[winner_idx], *cards)

        winning_player = None
        for player_idx, hand in enumerate(player_hands):
            new_hand = hand[1:]
            if not new_hand:
                winning_player = 1 if player_idx == 0 else 0
        if winning_player is not None:
            return winning_player, score(player_hands[winning_player][1:])
        player_hands = [hand[1:] for hand in player_hands if (new_hand := hand[1:])]


def play_recursive_combat_round(
    player_hands: List[Tuple[int]], earlier_hands: Set[Any]
) -> int:
    cards = []
    cannot_recurse = False
    for p_idx, hand in enumerate(player_hands):
        p_hash = (p_idx, hand)
        if p_hash in earlier_hands:
            raise HandPlayed()
        earlier_hands.add(p_hash)
        if hand[0] > len(hand[1:]):
            cannot_recurse = True
        cards.append(hand[0])

    if cannot_recurse:
        winner_idx = cards.index(max(cards))  # same as index in player_hands array
        return winner_idx
    new_hand = [hand[1 : hand[0] + 1] for hand in player_hands]
    return play_recursive_combat_game(new_hand)[0]


def puzzle2(input_file: Path):
    player_hands = [tuple(hand) for hand in read_starting_hands(input_file)]
    winning_score = play_recursive_combat_game(player_hands)[1]
    return winning_score


if __name__ == "__main__":
    print("Day 22")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
