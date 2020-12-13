using System;
using System.IO;

namespace Day02
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput2.txt");

            int length = puzzleInput.Length;
            int[] minNum = new int[length];
            int[] maxNum = new int[length];
            char[] character = new char[length];
            string[] password = new string[length];

            for (int i = 0; i < length; i++)
            {
                minNum[i] = int.Parse(puzzleInput[i].Split('-')[0]);

                int from = puzzleInput[i].IndexOf('-') + 1;
                int to = puzzleInput[i].IndexOf(' ') - from;
                maxNum[i] = int.Parse(puzzleInput[i].Substring(from, to));

                int charIndex = puzzleInput[i].IndexOf(' ') + 1;
                character[i] = puzzleInput[i][charIndex];

                password[i] = puzzleInput[i].Split(' ')[2];
            }

            ////
            //// PART 1
            ////

            int validPasswords = 0;
            for (int i = 0; i < length; i++)
            {
                int num = NumberOfCharInPassword(character[i], password[i]);
                if (num >= minNum[i] && num <= maxNum[i])
                {
                    validPasswords++;
                }
            }
            Console.WriteLine(validPasswords);


            ////
            //// PART 2
            ////

            int validPasswords2 = 0;
            for (int i = 0; i < length; i++)
            {
                if (password[i][minNum[i]-1] == character[i] && 
                    password[i][maxNum[i]-1] != character[i])
                {
                    validPasswords2++;
                }
                else if (password[i][minNum[i]-1] != character[i] &&
                         password[i][maxNum[i]-1] == character[i])
                {
                    validPasswords2++;
                }
            }
            Console.WriteLine(validPasswords2);
        }
        static int NumberOfCharInPassword(char c, string pw)
        {
            string[] splitPw = pw.Split(c);
            return splitPw.Length - 1; 
        }
    }
}
