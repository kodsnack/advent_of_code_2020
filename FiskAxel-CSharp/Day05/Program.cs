using System;
using System.IO;

namespace Day05
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput5.txt");

            int[] rows = new int[128];
            for (int i = 0; i < 128; i++)
            {
                rows[i] = i;
            }
            int[] seats = new int[8];
            for (int i = 0; i < 8; i++)
            {
                seats[i] = i;
            }

            int[] seatIDs = new int[puzzleInput.Length];
            //// 
            //// PART 1
            ////

            int highest = 0;
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                int row = GetRowNum(puzzleInput[i], rows, 0);
                int collumn = GetSeatNum(puzzleInput[i], seats, 7);
                int seatID = row * 8 + collumn;
                if(seatID > highest)
                {
                    highest = seatID;
                }
                seatIDs[i] = seatID;
            }
            Console.WriteLine($"The highest seat ID is: {highest}");

            ////
            //// PART 2
            ////

            Array.Sort(seatIDs);
            for (int i = 0; i + 1 < seatIDs.Length; i++)
            {
                if( seatIDs[i + 1] - seatIDs[i] == 2)
                {
                    Console.WriteLine($"Your seat ID: {seatIDs[i] + 1}");
                    break;
                }
            }
        }

        static int GetRowNum(string input, int[] rows, int index)
        {
            rows = SplitArray(rows, input[index]);
            if (index == 6)
            {
                return rows[0];
            }
            return GetRowNum(input, rows, index + 1);
        }
        static int GetSeatNum(string input, int[] seats, int index) 
        {
            seats = SplitArray(seats, input[index]);
            if (index == 9)
            {
                return seats[0];
            }
            return GetSeatNum(input, seats, index + 1);
        }
        static public int[] SplitArray(int[] arr, char BFLR)
        {
            int length = arr.Length/2;
            int[] half = new int[length];
            if (BFLR == 'F' || BFLR == 'L')
            {             
                Array.Copy(arr, 0, half, 0, length);

            }
            if (BFLR == 'B' || BFLR == 'R')
            {
                Array.Copy(arr, length, half, 0, length);
            }
            return half;
        }
    }
}