using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace aoc2020
{
    class Utils
    {
        public static IEnumerable<string> ReadInputLines(string input) 
        {
            return File.ReadAllLines(Path.Combine(Program.basePath, input));            
        }
    }
}
