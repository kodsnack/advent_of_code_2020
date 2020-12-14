using System;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  return GetOccupiedCount(GetAdjacentSeats, 4);

  (int y, int x)[] GetAdjacentSeats(char[,] board, int y, int x)
  {
    return new (int, int)[]
    {
      new (y-1, x-1), new (y-1, x), new (y-1, x+1),
      new (y,   x-1),               new (y,   x+1),
      new (y+1, x-1), new (y+1, x), new (y+1, x+1),
    };
  }
}

int PartTwo()
{
  return GetOccupiedCount(GetVisibleSeats, 5);

  (int y, int x)[] GetVisibleSeats(char[,] board, int y, int x)
  {
    return new (int, int)[]
    {
      Get(y, x, -1, -1), Get(y, x, -1, 0), Get(y, x, -1, 1),
      Get(y, x,  0, -1),                   Get(y, x,  0, 1),
      Get(y, x,  1, -1), Get(y, x,  1, 0), Get(y, x,  1, 1)
    };

    (int y, int x) Get(int y, int x, int deltaY, int deltaX)
    {
      y = y + deltaY;
      x = x + deltaX;
      if (y < 0 || x < 0) return new(y, x);
      if (y >= board.GetLength(0) || x >= board.GetLength(1)) return new(y, x);
      if (board[y, x] != '.') return new(y, x);

      return Get(y, x, deltaY, deltaX);
    }
  }
}

int GetOccupiedCount(Func<char[,], int, int, (int y, int x)[]> otherSeats, int occupiedLevel)
{
  var board = new char[lines.Length, lines[0].Length];
  for (int y = 0; y < lines.Length; y++)
    for (int x = 0; x < lines[y].Length; x++)
      board[y, x] = lines[y][x];

  while (true)
  {
    var next = Run(board);
    if (AreEqual(board, next)) break;
    Copy(next, board);
  }

  var result = 0;
  for (int y = 0; y < board.GetLength(0); y++)
    for (int x = 0; x < board.GetLength(1); x++)
      if (board[y, x] == '#') result++;

  return result;

  bool AreEqual(char[,] a, char[,] b)
  {
    for (int y = 0; y < a.GetLength(0); y++)
      for (int x = 0; x < a.GetLength(1); x++)
        if (a[y, x] != b[y, x]) return false;

    return true;
  }

  void Copy(char[,] from, char[,] to)
  {
    for (int y = 0; y < from.GetLength(0); y++)
      for (int x = 0; x < from.GetLength(1); x++)
        to[y, x] = from[y, x];
  }

  char[,] Run(char[,] board)
  {
    var next = new char[board.GetLength(0), board.GetLength(1)];
    Copy(board, next);

    for (int y = 0; y < board.GetLength(0); y++)
      for (int x = 0; x < board.GetLength(1); x++)
        Flip(y, x);

    return next;

    void Flip(int y, int x)
    {
      if (board[y, x] == '.') return;

      var others = otherSeats(board, y, x);

      if (IsEmpty(y, x))
      {
        if (others.All(o => !IsOccupied(o.y, o.x)))
        {
          next[y, x] = '#';
        }
      }
      else
      {
        if (others.Count(o => IsOccupied(o.y, o.x)) >= occupiedLevel)
        {
          next[y, x] = 'L';
        }
      }
    }

    bool IsEmpty(int y, int x) => board[y, x] == 'L';

    bool IsOccupied(int y, int x)
    {
      if (y < 0 || x < 0) return false;
      if (y >= board.GetLength(0) || x >= board.GetLength(1)) return false;
      return board[y, x] == '#';
    }
  }
}
