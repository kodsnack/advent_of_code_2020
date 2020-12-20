using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne()
{
  var expressions = lines.Select(x => Parse(x).ToList()).ToList();

  return expressions.Select(x => Evaluate(x, Calculate)).Sum();
}

long PartTwo()
{
  var expressions = lines.Select(x => Parse(x).ToList()).ToList();

  return expressions.Select(x => Evaluate(x, CalculateAdvanced)).Sum();
}

IEnumerable<object> Parse(string line)
{
  foreach (char token in line.Replace(" ", string.Empty))
  {
    yield return Get(token);
  }

  object Get(char token)
  {
    switch (token)
    {
      case '(':
      case ')': return new Parenthesis(token);
      case '+':
      case '*': return new Operator(token);
    }

    return new Number(token - '0');
  }
}

long Evaluate(List<object> expression, Func<List<object>, long> calculate)
{
  while (expression.Any(x => x is Parenthesis))
  {
    int lastStartParenthesisIndex = -1, firstEndParenthesisIndex = -1, index = 0;

    foreach (var item in expression)
    {
      if (item is Parenthesis start && start.IsStart)
      {
        lastStartParenthesisIndex = index;
      }
      else if (item is Parenthesis end && end.IsEnd)
      {
        firstEndParenthesisIndex = index;
        break;
      }

      index++;
    }

    var temp = expression.Skip(lastStartParenthesisIndex + 1).Take(firstEndParenthesisIndex - lastStartParenthesisIndex - 1).ToList();
    var result = calculate(temp);
    expression.RemoveRange(lastStartParenthesisIndex, firstEndParenthesisIndex - lastStartParenthesisIndex + 1);
    expression.Insert(lastStartParenthesisIndex, new Number(result));
  }

  return calculate(expression);
}

long Calculate(List<object> expression)
{
  long result = 0;
  Operator op = null;
  Number number = null;

  foreach (var item in expression)
  {
    if (item is Operator o)
    {
      op = o;
    }
    else if (item is Number n)
    {
      if (result == 0) result = n.Value;
      else number = n;
    }

    if (op != null && number != null)
    {
      switch (op.Value)
      {
        case '+':
          result += number.Value;
          break;
        case '*':
          result *= number.Value;
          break;
      }

      op = null;
      number = null;
    }
  }

  return result;
}

long CalculateAdvanced(List<object> expression)
{
  while (expression.Any(x => x is Operator o && o.IsAddition))
  {
    var additionIndex = expression.FindIndex(x => x is Operator o && o.IsAddition);
    var temp = expression.Skip(additionIndex - 1).Take(3).ToList();
    var result = Calculate(temp);
    expression.RemoveRange(additionIndex - 1, 3);
    expression.Insert(additionIndex - 1, new Number(result));
  }

  return Calculate(expression);
}

record Number(long Value)
{
  public override string ToString() => Value.ToString();
}
record Operator(char Value)
{
  public bool IsAddition => Value == '+';
  public override string ToString() => Value.ToString();
}
record Parenthesis(char Value)
{
  public bool IsStart => Value == '(';
  public bool IsEnd => Value == ')';
  public override string ToString() => Value.ToString();
}
