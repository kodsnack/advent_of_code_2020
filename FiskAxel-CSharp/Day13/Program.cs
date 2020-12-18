using System;
using System.Collections.Generic;
using System.IO;

namespace Day13
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput13.txt");

            int eariliestTimestamp = int.Parse(puzzleInput[0]);
            int timestamp = eariliestTimestamp;

            string[] busses = puzzleInput[1].Split(',');
            List<int> bussNumbers = new List<int>();
            int[] bussNumbers2 = new int[busses.Length];
            int numOfBusses = 0;
            for (int i = 0; i < busses.Length; i++)
            {
                if (int.TryParse(busses[i], out int a))
                {
                    bussNumbers.Add(int.Parse(busses[i]));
                    bussNumbers2[i] = int.Parse(busses[i]);
                    numOfBusses++;
                }
                else
                {
                    bussNumbers2[i] = 0;
                }
            }

            ////
            //// PART 1
            ////

            bool found = false;
            int earliestBuss = 0;
            while (!found)
            {
                foreach (int bn in bussNumbers)
                {
                    if (timestamp  % bn  == 0)
                    {
                        Console.WriteLine($"Buss#:{bn}");
                        Console.WriteLine($"{timestamp}");
                        earliestBuss = bn;
                        found = true;
                    }  
                }
                if (!found)
                {
                    timestamp++;
                }               
            }

            int answere1 = (timestamp - eariliestTimestamp) * earliestBuss;
            Console.WriteLine($"Part 1: {answere1}");

            ////
            //// PART 2
            ////

            var bussPair = new List<KeyValuePair<int, int>>();
            for (int i = 0; i < bussNumbers2.Length; i++)
            {
                if (bussNumbers2[i] == 0)
                { }
                else
                {      
                    bussPair.Add(new KeyValuePair<int, int>(bussNumbers2[i], Array.IndexOf(bussNumbers2, bussNumbers2[i])));
                }
            }

            long plus = 1;
            long t = 0;
            foreach (KeyValuePair<int, int> set in bussPair)
            {                     
                if (plus == 0)
                {
                    plus = set.Key;
                }
                
                while ((t + set.Value) % set.Key != 0)
                {                 
                    t += plus;
                }
                plus *= set.Key;
            }
            Console.WriteLine($"Part 2: {t}");
        }
    }
}
