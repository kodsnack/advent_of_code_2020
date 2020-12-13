using System;
using System.Collections.Generic;
using System.IO;

namespace Day06
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput6.txt");

            List<string> groups = new List<string>();
            List<string> lines = new List<string>();
            
            int groupMembers = 0;
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                string line = puzzleInput[i];
                
                if (line == "")
                {
                    AddToGroups(groups, lines, groupMembers);
                    groupMembers = 0;
                }
                else
                {
                    groupMembers++;
                    lines.Add(line);
                }
            } AddToGroups(groups, lines, groupMembers); //Last group

            ////
            //// PART 1
            ////

            int sum = 0;
            for (int i = 0; i < groups.Count; i++)
            {
                if (groups[i].Contains('a')) { sum++; }
                if (groups[i].Contains('b')) { sum++; }
                if (groups[i].Contains('c')) { sum++; }
                if (groups[i].Contains('d')) { sum++; }
                if (groups[i].Contains('e')) { sum++; }
                if (groups[i].Contains('f')) { sum++; }
                if (groups[i].Contains('g')) { sum++; }
                if (groups[i].Contains('h')) { sum++; }
                if (groups[i].Contains('i')) { sum++; }
                if (groups[i].Contains('j')) { sum++; }
                if (groups[i].Contains('k')) { sum++; }
                if (groups[i].Contains('l')) { sum++; }
                if (groups[i].Contains('m')) { sum++; }
                if (groups[i].Contains('n')) { sum++; }
                if (groups[i].Contains('o')) { sum++; }
                if (groups[i].Contains('p')) { sum++; }
                if (groups[i].Contains('q')) { sum++; }
                if (groups[i].Contains('r')) { sum++; }
                if (groups[i].Contains('s')) { sum++; }
                if (groups[i].Contains('t')) { sum++; }
                if (groups[i].Contains('u')) { sum++; }
                if (groups[i].Contains('v')) { sum++; }
                if (groups[i].Contains('w')) { sum++; }
                if (groups[i].Contains('x')) { sum++; }
                if (groups[i].Contains('y')) { sum++; }
                if (groups[i].Contains('z')) { sum++; }
            }

            Console.WriteLine($"Part 1: {sum}");

            ////
            //// PART 2
            //// 

            sum = 0;
            for (int i = 0; i < groups.Count; i++)
            {
                int[] sums = new int[26];
                for (int j = 0; j < sums.Length; j++)
                {        
                    sums[j] = 0;
                }
                foreach (char chr in groups[i])
                {
                    if (chr == 'a') { sums[0]++; }
                    if (chr == 'b') { sums[1]++; }
                    if (chr == 'c') { sums[2]++; }
                    if (chr == 'd') { sums[3]++; }
                    if (chr == 'e') { sums[4]++; }
                    if (chr == 'f') { sums[5]++; }
                    if (chr == 'g') { sums[6]++; }
                    if (chr == 'h') { sums[7]++; }
                    if (chr == 'i') { sums[8]++; }
                    if (chr == 'j') { sums[9]++; }
                    if (chr == 'k') { sums[10]++; }
                    if (chr == 'l') { sums[11]++; }
                    if (chr == 'm') { sums[12]++; }
                    if (chr == 'n') { sums[13]++; }
                    if (chr == 'o') { sums[14]++; }
                    if (chr == 'p') { sums[15]++; }
                    if (chr == 'q') { sums[16]++; }
                    if (chr == 'r') { sums[17]++; }
                    if (chr == 's') { sums[18]++; }
                    if (chr == 't') { sums[19]++; }
                    if (chr == 'u') { sums[20]++; }
                    if (chr == 'v') { sums[21]++; }
                    if (chr == 'w') { sums[22]++; }
                    if (chr == 'x') { sums[23]++; }
                    if (chr == 'y') { sums[24]++; }
                    if (chr == 'z') { sums[25]++; }
                }
                foreach (int result in sums)
                {
                    int num = (int)char.GetNumericValue(groups[i][0]);
                    if (result == num)
                    {
                        sum++;
                    }
                }
            }
            Console.WriteLine($"Part 2: {sum}");

        }

        static void AddToGroups(List<string> groups, List<string> lines, int members)
        {
            groups.Add($"{members}");
            for (int j = 0; j < lines.Count; j++)
            {
                groups[groups.Count - 1] += lines[j];
            }
            lines.Clear();
        }
    }
}
