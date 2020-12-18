using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var instructions = GetInstructions();
  (int accumulator, _) = TryRun(instructions);

  return accumulator;
}

int PartTwo()
{
  var instructions = GetInstructions();

  for (int i = 0; i < instructions.Count; i++)
  {
    instructions[i].Flip();

    (int accumulator, bool success) = TryRun(instructions);

    if (success) return accumulator;

    instructions[i].Flip();
  }

  return default(int);
}

List<Instruction> GetInstructions()
{
  var instructions = new List<Instruction>();

  foreach (var line in lines)
  {
    var tokens = line.Split(" ");
    instructions.Add(new Instruction(tokens.First(), Convert.ToInt32(tokens.Last())));
  }

  return instructions;
}

(int accumulator, bool success) TryRun(List<Instruction> instructions)
{
  var accumulator = 0;
  var visits = new bool[instructions.Count];
  var index = 0;

  while (true)
  {
    if (visits[index]) break;

    visits[index] = true;
    var instruction = instructions[index];

    switch (instruction.Operation)
    {
      case "nop":
        index++;
        break;
      case "acc":
        accumulator += instruction.Argument;
        index++;
        break;
      case "jmp":
        index += instruction.Argument;
        break;
    }

    if (index == instructions.Count) break;
  }

  return (accumulator, index == instructions.Count);
}

class Instruction
{
  public string Operation { get; private set; }
  public int Argument { get; }

  public Instruction(string operation, int argument)
  {
    Operation = operation;
    Argument = argument;
  }

  public void Flip()
  {
    if (Operation == "nop")
      Operation = "jmp";
    else if (Operation == "jmp")
      Operation = "nop";
  }
}
