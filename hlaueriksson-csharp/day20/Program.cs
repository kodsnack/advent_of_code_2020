using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var text = File.ReadAllText("input.txt");
var tiles = GetTiles().ToList();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne()
{
  var edgesInCommon = new Dictionary<string, int>();
  foreach (var tile in tiles)
  {
    foreach (var edge in tile.Edges)
    {
      if (edgesInCommon.ContainsKey(edge)) edgesInCommon[edge]++;
      else edgesInCommon[edge] = 1;
    }
  }

  var corners = new List<Tile>();
  foreach (var tile in tiles)
  {
    var outerEdges = 0;
    foreach (var edge in tile.Edges)
      if (edgesInCommon[edge] == 1)
        outerEdges++;
    if (outerEdges == 4) // 2 + 2 flipped
      corners.Add(tile);
  }
  return corners.Select(x => (long)x.Id).Aggregate((result, value) => result * value);
}

long PartTwo()
{
  var edgesInCommon = new Dictionary<string, int>();
  foreach (var tile in tiles)
  {
    foreach (var edge in tile.Edges)
    {
      if (edgesInCommon.ContainsKey(edge)) edgesInCommon[edge]++;
      else edgesInCommon[edge] = 1;
    }
  }

  var corners = new List<Tile>();
  foreach (var tile in tiles)
  {
    var outerEdges = 0;
    foreach (var edge in tile.Edges)
      if (edgesInCommon[edge] == 1)
        outerEdges++;
    if (outerEdges == 4) // 2 + 2 flipped
      corners.Add(tile);
  }

  var image = new Image(tiles.Count);
  image.AddStartTile(corners.First(), edgesInCommon);

  // left edges
  for (var y = 1; y < image.Tiles.GetLength(0); y++)
  {
    var previousTile = image.Tiles[y - 1, 0];
    var edge = previousTile.BottomEdge;
    var tile = FindTile(previousTile.Id, edge);
    image.AddTile(tile, y, 0);
  }

  // the rest
  for (var y = 0; y < image.Tiles.GetLength(0); y++)
  {
    for (var x = 1; x < image.Tiles.GetLength(1); x++)
    {
      var previousTile = image.Tiles[y, x - 1];
      var edge = previousTile.RightEdge;
      var tile = FindTile(previousTile.Id, edge);
      image.AddTile(tile, y, x);
    }
  }

  Image monsterImage = null;
  foreach (var i in image.ImageVariants())
  {
    if (i.HasMonsters())
    {
      monsterImage = i;
      break;
    }
  }
  monsterImage.MarkMonsters();
  return monsterImage.CountNonMonsterPixels();

  Tile FindTile(int id, string edge)
  {
    foreach (var tile in tiles)
      if (tile.Id != id && tile.Edges.Contains(edge))
        return tile;
    throw new Exception("💥");
  }
}

IEnumerable<Tile> GetTiles()
{
  var tokens = text.Split("\n\n", StringSplitOptions.RemoveEmptyEntries);
  foreach (var token in tokens)
    yield return new Tile(token);
}

class Tile
{
  public int Id { get; }
  public char[,] Matrix { get; }

  public string[] Edges
  {
    get
    {
      return new[]
      {
        GetEdge(0),
        GetEdge(1),
        GetEdge(2),
        GetEdge(3),
        Flip(GetEdge(0)),
        Flip(GetEdge(1)),
        Flip(GetEdge(2)),
        Flip(GetEdge(3))
      };

      string Flip(string edge) => string.Concat(edge.Reverse());
    }
  }

  public string TopEdge => GetEdge(0);
  public string RightEdge => GetEdge(1);
  public string BottomEdge => GetEdge(2);
  public string LeftEdge => GetEdge(3);

  public Tile(string token)
  {
    var lines = token.Split("\n", StringSplitOptions.RemoveEmptyEntries);

    Id = Convert.ToInt32(lines.First().Replace("Tile ", string.Empty).Replace(":", string.Empty));
    Matrix = new char[lines.Length - 1, lines[1].Length];

    for (int y = 1; y < lines.Length; y++)
      for (int x = 0; x < lines[y].Length; x++)
        Matrix[y - 1, x] = lines[y][x];
  }

  internal Tile(int id, char[,] matrix)
  {
    Id = id;
    Matrix = matrix;
  }

  string GetEdge(int edge)
  {
    return string.Concat(GetEdgeArray(edge));

    IEnumerable<char> GetEdgeArray(int edge)
    {
      int x = 0, y = 0, dx = 0, dy = 0;
      if (edge == 0)
      {
        dx = 1;
      }
      else if (edge == 1)
      {
        x = Matrix.GetLength(0) - 1;
        dy = 1;
      }
      else if (edge == 2)
      {
        y = Matrix.GetLength(0) - 1;
        dx = 1;
      }
      else if (edge == 3)
      {
        dy = 1;
      }

      for (int i = 0; i < Matrix.GetLength(0); i++)
        yield return Matrix[y + dy * i, x + dx * i];
    }
  }
}

class Image
{
  public Tile[,] Tiles { get; }

  public char[,] Matrix { get; }

  public Image(int tileCount)
  {
    var size = (int)Math.Sqrt(tileCount);
    Tiles = new Tile[size, size];
    Matrix = new char[size * 8, size * 8];
  }

  Image(char[,] matrix)
  {
    Matrix = matrix;
  }

  public void AddStartTile(Tile tile, Dictionary<string, int> edgesInCommon)
  {
    foreach (var t in TileVariants(tile))
      if (edgesInCommon[t.TopEdge] == 1 && edgesInCommon[t.LeftEdge] == 1) // happens twice
        Add(t, 0, 0);
  }

  public void AddTile(Tile tile, int y, int x)
  {
    if (x == 0)
    {
      var previousTile = Tiles[y - 1, x];
      var bottom = previousTile.BottomEdge;
      foreach (var t in TileVariants(tile))
      {
        if (t.TopEdge == bottom)
        {
          Add(t, y, x);
          return;
        }
      }
    }
    else
    {
      var previousTile = Tiles[y, x - 1];
      var right = previousTile.RightEdge;
      foreach (var t in TileVariants(tile))
      {
        if (t.LeftEdge == right)
        {
          Add(t, y, x);
          return;
        }
      }
    }
    throw new Exception("💥");
  }

  public IEnumerable<Image> ImageVariants()
  {
    foreach (var matrix in Variants(Matrix))
      yield return new Image(matrix);
  }

  public bool HasMonsters()
  {
    for (var y = 0; y < Matrix.GetLength(0); y++)
      for (var x = 0; x < Matrix.GetLength(1); x++)
        if (IsMonster(y, x))
          return true;
    return false;
  }

  public void MarkMonsters()
  {
    for (var y = 0; y < Matrix.GetLength(0); y++)
      for (var x = 0; x < Matrix.GetLength(1); x++)
        if (IsMonster(y, x))
          MarkMonster(y, x);
  }

  public int CountNonMonsterPixels()
  {
    var result = 0;
    for (var y = 0; y < Matrix.GetLength(0); y++)
      for (var x = 0; x < Matrix.GetLength(1); x++)
        if (Matrix[y, x] == '#') result++;
    return result;
  }

  void Add(Tile tile, int y, int x)
  {
    Tiles[y, x] = tile;
    for (var dy = 0; dy < 10; dy++)
      for (var dx = 0; dx < 10; dx++)
        if (dy > 0 && dy < 9 && dx > 0 && dx < 9)
          Matrix[y * 8 + dy - 1, x * 8 + dx - 1] = tile.Matrix[dy, dx];
  }

  IEnumerable<Tile> TileVariants(Tile tile)
  {
    foreach (var matrix in Variants(tile.Matrix))
      yield return new Tile(tile.Id, matrix);
  }

  IEnumerable<char[,]> Variants(char[,] matrix)
  {
    var r90 = Rotate90(matrix);
    var r180 = Rotate90(r90);
    var r270 = Rotate90(r180);
    return new[]
    {
      matrix,
      r90,
      r180,
      r270,
      FlipVertical(matrix),
      FlipVertical(r90),
      FlipVertical(r180),
      FlipVertical(r270),
    };
  }

  char[,] Rotate90(char[,] matrix)
  {
    int rows = matrix.GetLength(1);
    int columns = matrix.GetLength(0);
    var result = new char[rows, columns];
    for (var i = 0; i < rows; i++)
      for (var j = 0; j < columns; j++)
        result[i, j] = matrix[j, rows - i - 1];
    return result;
  }

  char[,] FlipVertical(char[,] matrix)
  {
    int rows = matrix.GetLength(0);
    int columns = matrix.GetLength(1);
    var result = new char[rows, columns];
    for (var i = 0; i < rows; i++)
      for (var j = 0; j < columns; j++)
        result[i, j] = matrix[rows - 1 - i, j];
    return result;
  }

  readonly string[] monster = new string[]
  {
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  };

  bool IsMonster(int y, int x)
  {
    if (y + monster.Length > Matrix.GetLength(0) || x + monster[0].Length > Matrix.GetLength(1)) return false;
    for (var i = 0; i < monster.Length; i++)
      for (var j = 0; j < monster[i].Length; j++)
        if (monster[i][j] == '#' && Matrix[y + i, x + j] != '#') return false;
    return true;
  }

  void MarkMonster(int y, int x)
  {
    for (var i = 0; i < monster.Length; i++)
      for (var j = 0; j < monster[i].Length; j++)
        if (monster[i][j] == '#')
          Matrix[y + i, x + j] = 'O';
  }
}
