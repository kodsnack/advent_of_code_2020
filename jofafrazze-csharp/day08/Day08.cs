using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;

namespace day08
{
    public class Day08
    {
        readonly static string nsname = typeof(Day08).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        public class Computer
        {
            public struct OpCode
            {
                public string name;
                public Action<int> action;
                public OpCode(string n, Action<int> a) { name = n; action = a; }
            };
            public struct Instruction
            {
                public OpCode opCode;
                public int argument;
                public Instruction(OpCode c, int a) { opCode = c; argument = a; }
                public void Execute() { opCode.action(argument); }
            };
            public int acc = 0;
            public int pc = 0;
            public List<string> source = new List<string>();
            public List<Instruction> program = new List<Instruction>();
            public Dictionary<string, OpCode> instructionSet;
            public HashSet<int> visited = new HashSet<int>();
            public List<int> orderVisited = new List<int>();
            public Computer(Computer c) : this(c.source)
            {
                acc = c.acc;
                pc = c.pc;
            }
            public Computer(List<string> s) : this()
            {
                source = new List<string>(s);
                BuildProgram();
            }
            public Computer() { Init(); }
            private void Init()
            {
                var opCodes = new List<OpCode>()
                {
                    new OpCode("nop", delegate(int a) { pc++;  }),
                    new OpCode("acc", delegate(int a) { acc += a; pc++; }),
                    new OpCode("jmp", delegate(int a) { pc += a; }),
                };
                instructionSet = opCodes.ToDictionary(a => a.name, a => a);
            }
            private void BuildProgram()
            {
                program = new List<Instruction>();
                foreach (string line in source)
                {
                    string[] s = line.Split(' ').ToArray();
                    program.Add(new Instruction(instructionSet[s[0]], int.Parse(s[1])));
                }
            }
            public bool Run()
            {
                while ((pc >= 0) && (pc < program.Count))
                {
                    if (!visited.Add(pc))
                        return false;
                    orderVisited.Add(pc);
                    program[pc].Execute();
                }
                return true;
            }
        }

        static Object PartA()
        {
            var input = ReadInputs.ReadStrings(inputPath);
            var c = new Computer(input);
            c.Run();
            var ans = c.acc;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadInputs.ReadStrings(inputPath);
            var computer = new Computer(input);
            computer.Run();
            int ans = 0;
            foreach (int offs in computer.orderVisited)
            {
                var c = new Computer(computer.source);
                var i = c.program[offs];
                var n = i.opCode.name;
                if (n != "acc")
                {
                    i.opCode = c.instructionSet[n == "jmp" ? "nop" : "jmp"];
                    c.program[offs] = i;
                    if (c.Run())
                    {
                        ans = c.acc;
                        break;
                    }
                }
            }
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 1709;
            int b = 1976;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
