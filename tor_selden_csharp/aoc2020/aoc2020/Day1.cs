using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc2020
{
    class Day1
    {
        static IEnumerable<string> data = Utils.ReadInputLines("day1.txt");

        public static void A()
        {
            var entries = data.Select(int.Parse).ToArray();

            for (int i = 0; i < entries.Length; i++)
            {
                var val = entries[i];
                var valPair = 2020 - val;
                if (Array.IndexOf(entries, valPair) > -1)
                {
                    Console.WriteLine(val * valPair);
                    return;
                }
            }
        }

        public static void B()
        {
            var entries = data.Select(int.Parse).ToArray();

            for (int i = 0; i < entries.Length; i++)
            {
                var val = entries[i];
                for (int j = i + 1; j < entries.Length; j++)
                {
                    var val2 = entries[j];
                    var val3 = 2020 - val - val2;
                    if (Array.IndexOf(entries, val3) > -1)
                    {
                        Console.WriteLine(val * val2 * val3);
                        return;
                    }
                }
            }
        }
    }
}
