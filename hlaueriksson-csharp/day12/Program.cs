using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  int east = 0, north = 0;
  var direction = 'E';

  foreach (var line in lines)
  {
    var action = line.First();
    var value = Convert.ToInt32(line.Substring(1));

    switch (action)
    {
      case 'N':
      case 'S':
      case 'E':
      case 'W':
        Move(action, value);
        break;
      case 'L':
      case 'R':
        Turn(action, value);
        break;
      case 'F':
        Move(direction, value);
        break;
    }
  }

  return Math.Abs(east) + Math.Abs(north);

  void Move(char direction, int value)
  {
    switch (direction)
    {
      case 'N':
        north += value;
        break;
      case 'S':
        north -= value;
        break;
      case 'E':
        east += value;
        break;
      case 'W':
        east -= value;
        break;
    }
  }

  void Turn(char way, int value)
  {
    var directions = new List<char>() { 'N', 'E', 'S', 'W' };
    var index = directions.FindIndex(x => x == direction);
    var steps = value / 90;

    switch (way)
    {
      case 'L':
        direction = directions[Mod(index - steps, directions.Count)];
        break;
      case 'R':
        direction = directions[Mod(index + steps, directions.Count)];
        break;
    }

    int Mod(int value, int mod)
    {
      return (value % mod + mod) % mod;
    }
  }
}

int PartTwo()
{
  (int east, int north) waypoint = new(10, 1);
  (int east, int north) position = new(0, 0);

  foreach (var line in lines)
  {
    var action = line.First();
    var value = Convert.ToInt32(line.Substring(1));

    switch (action)
    {
      case 'N':
      case 'S':
      case 'E':
      case 'W':
        MoveWaypoint(action, value);
        break;
      case 'L':
      case 'R':
        TurnWaypoint(action, value);
        break;
      case 'F':
        MoveShip(value);
        break;
    }
  }

  return Math.Abs(position.east) + Math.Abs(position.north);

  void MoveWaypoint(char direction, int value)
  {
    switch (direction)
    {
      case 'N':
        waypoint.north += value;
        break;
      case 'S':
        waypoint.north -= value;
        break;
      case 'E':
        waypoint.east += value;
        break;
      case 'W':
        waypoint.east -= value;
        break;
    }
  }

  void TurnWaypoint(char way, int value)
  {
    var steps = value / 90;

    for (int i = 0; i < steps; i++)
    {
      int east = waypoint.east, north = waypoint.north;

      switch (way)
      {
        case 'L':
          waypoint.east = -north;
          waypoint.north = east;
          break;
        case 'R':
          waypoint.east = north;
          waypoint.north = -east;
          break;
      }
    }
  }

  void MoveShip(int value)
  {
    position.north = position.north + waypoint.north * value;
    position.east = position.east + waypoint.east * value;
  }
}
