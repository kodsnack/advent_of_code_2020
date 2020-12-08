using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day08
{
    public class Day08
    {
        readonly static string nsname = typeof(Day08).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        struct OpCode
        {
            public string name;
            public Action<int> action;
            public OpCode(string n, Action<int> a) { name = n; action = a; }
        };

        static int register = 0;
        static int pc = 0;

        static readonly List<OpCode> opCodes = new List<OpCode>()
        {
            new OpCode("nop", delegate(int a) {}),
            new OpCode("acc", delegate(int a) { register += a; }),
            new OpCode("jmp", delegate(int a) { pc += a; }),
        };
        static readonly Dictionary<string, int> instructionSet = opCodes.Select((x, i) => new { x, i }).ToDictionary(a => a.x.name, a => a.i);

        struct Instruction
        {
            public OpCode opCode;
            public int argument;
            public void Execute()
            {
                opCode.action(argument);
            }
        };

        static List<Instruction> ReadInput(string path)
        {
            StreamReader reader = File.OpenText(path);
            List<Instruction> list = new List<Instruction>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Instruction i = new Instruction();
                string[] s = line.Split(' ').ToArray();
                i.opCode = opCodes[instructionSet[s[0]]];
                i.argument = int.Parse(s[1]);
                list.Add(i);
            }
            return list;
        }

        static HashSet<int> visited = new HashSet<int>();
        static List<int> order = new List<int>();

        static bool RunProgramA(List<Instruction> program, bool registerOrder)
        {
            pc = 0;
            register = 0;
            while ((pc >= 0) && (pc < program.Count))
            {
                if (visited.Contains(pc))
                {
                    return false;
                }
                else {
                    visited.Add(pc);
                    if (registerOrder)
                        order.Add(pc);
                    bool inc = program[pc].opCode.name != "jmp";
                    program[pc].Execute();
                    if (inc)
                        pc++;
                }
            }
            return true;
        }

        static void RunProgramB(List<Instruction> program)
        {
            foreach (int offs in order)
            {
                visited = new HashSet<int>();
                List<Instruction> p = new List<Instruction>(program);
                string s = p[offs].opCode.name;
                if (s != "acc")
                {
                    Instruction i = p[offs];
                    if (s == "jmp")
                        i.opCode = opCodes[instructionSet["nop"]];
                    else
                        i.opCode = opCodes[instructionSet["jmp"]];
                    p[offs] = i;
                    if (RunProgramA(p, false))
                        return;
                }
            }
        }

        //static void PartAB()
        //{
        //    List<Instruction> program = ReadInput();
        //    RunProgram(program, 0);
        //    Console.WriteLine("Part A: Result is {0}.", registers[1]);
        //    RunProgram(program, 1);
        //    Console.WriteLine("Part B: Result is {0}.", registers[1]);
        //}

        static Object PartA()
        {
            List<Instruction> program = ReadInput(inputPath);
            RunProgramA(program, true);
            int ans = register;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<Instruction> program = ReadInput(inputPath);
            RunProgramB(program);
            int ans = register;
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
            int a = 42;
            int b = 4711;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
