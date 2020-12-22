using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var isLogEnabled = false;
var text = File.ReadAllText("input.txt");
var tokens = text.Split("\n\n");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var player1 = new Deck(tokens.First());
  var player2 = new Deck(tokens.Last());
  int round = 1;
  while (player1.Any() && player2.Any())
  {
    Log($"-- Round {round} --");
    Log($"Player 1's deck: {player1.ToString()}");
    Log($"Player 2's deck: {player2.ToString()}");

    var card1 = player1.Pop();
    var card2 = player2.Pop();
    Log($"Player 1 plays: {card1}");
    Log($"Player 2 plays: {card2}");

    if (card1 > card2)
    {
      Log($"Player 1 wins the round!");
      player1.Add(card1, card2);
    }
    else
    {
      Log($"Player 2 wins the round!");
      player2.Add(card2, card1);
    }
    Log();
    round++;
  }

  Log();
  Log("== Post-game results ==");
  Log($"Player 1's deck: {player1.ToString()}");
  Log($"Player 2's deck: {player2.ToString()}");

  var winner = player1.Any() ? player1 : player2;
  return winner.GetScore();
}

int PartTwo()
{
  var player1 = new Deck(tokens.First());
  var player2 = new Deck(tokens.Last());
  var winner = Play(1, player1, player2);

  Log();
  Log();
  Log("== Post-game results ==");
  Log($"Player 1's deck: {player1.ToString()}");
  Log($"Player 2's deck: {player2.ToString()}");

  return winner == 1 ? player1.GetScore() : player2.GetScore();

  int Play(int game, Deck player1, Deck player2)
  {
    var subGame = 0;
    var round = 1;
    Log($"=== Game {game} ===");

    while (player1.Any() && player2.Any())
    {
      Log();
      Log($"-- Round {round} (Game {game}) --");
      Log($"Player 1's deck: {player1.ToString()}");
      Log($"Player 2's deck: {player2.ToString()}");

      // Before either player deals a card, if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks, the game instantly ends in a win for player 1. Previous rounds from other games are not considered. (This prevents infinite games of Recursive Combat, which everyone agrees is a bad idea.)
      if (player1.HasInvalidHistory() || player2.HasInvalidHistory())
      {
        Log($"The winner of game {game} is player 1!");
        return 1;
      }

      var card1 = player1.Pop();
      var card2 = player2.Pop();
      Log($"Player 1 plays: {card1}");
      Log($"Player 2 plays: {card2}");

      // If both players have at least as many cards remaining in their deck as the value of the card they just drew, the winner of the round is determined by playing a new game of Recursive Combat (see below).
      if (player1.Cards.Count >= card1 && player2.Cards.Count >= card2)
      {
        subGame++;
        Log("Playing a sub-game to determine the winner...");
        Log();
        var subWinner = Play(game + subGame, new Deck(player1.Cards.Take(card1)), new Deck(player2.Cards.Take(card2)));
        Log();
        Log($"...anyway, back to game {game}.");
        if (subWinner == 1)
        {
          Log($"Player 1 wins round {round} of game {game}!");
          player1.Add(card1, card2);
        }
        else
        {
          Log($"Player 2 wins round {round} of game {game}!");
          player2.Add(card2, card1);
        }
      }
      else
      {
        if (card1 > card2)
        {
          Log($"Player 1 wins round {round} of game {game}!");
          player1.Add(card1, card2);
        }
        else
        {
          Log($"Player 2 wins round {round} of game {game}!");
          player2.Add(card2, card1);
        }
      }
      round++;
    }

    var winner = player1.Any() ? 1 : 2;
    Log($"The winner of game {game} is player {winner}!");
    return winner;
  }
}

void Log(string message = "")
{
  if (isLogEnabled) Console.WriteLine(message);
}

class Deck
{
  public List<int> Cards { get; } = new List<int>();

  HashSet<string> History { get; } = new HashSet<string>();

  public Deck(string token)
  {
    var values = token.Split("\n", StringSplitOptions.RemoveEmptyEntries).Skip(1).Select(int.Parse);
    foreach (var value in values)
      Cards.Add(value);
  }

  public Deck(IEnumerable<int> cards)
  {
    Cards.AddRange(cards);
  }

  public bool Any() => Cards.Any();

  public int Pop()
  {
    var first = Cards.First();
    Cards.RemoveAt(0);
    return first;
  }

  public void Add(int winner, int other)
  {
    Cards.Add(winner);
    Cards.Add(other);
  }

  public bool HasInvalidHistory()
  {
    if (History.Contains(ToString())) return true;
    History.Add(ToString());
    return false;
  }

  public int GetScore() => Cards.ToArray().Reverse().Select((value, index) => value * (index + 1)).Sum();

  public override string ToString()
  {
    return string.Join(", ", Cards);
  }
}
