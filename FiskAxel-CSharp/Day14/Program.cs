using System;
using System.Collections.Generic;
using System.IO;

namespace Day14
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] input = File.ReadAllLines("../../../puzzleInput14.txt");

            int largestMemoryAddress = 0;
            foreach (string line in input)
            {
                if (line.Contains("mem["))
                {
                    int end = line.IndexOf("]") - 4;
                    int memoryAddress = int.Parse(line.Substring(4, end));
                    if (memoryAddress > largestMemoryAddress)
                    {
                        largestMemoryAddress = memoryAddress;
                    }
                }
            }           

            ////
            //// PART 1
            ////

            double[] memory = new double[largestMemoryAddress + 1];
            for (int i = 0; i < input.Length;)
            {
                string mask = "";
                if (input[i].Contains("mask = "))
                {
                    mask = input[i].Substring(7, 36);
                    i++;
                }
                while ( i < input.Length && !input[i].Contains("mask = "))
                {
                    int start = input[i].IndexOf(" = ") + 3;
                    double number = double.Parse(input[i].Substring(start));
                    string biNum = ToBinary(number);
                    
                    int length = input[i].IndexOf("]") - 4;
                    int address = int.Parse(input[i].Substring(4, length));
                    
                    memory[address] = ToDecimal(Mask(biNum, mask));; 
                    i++;
                }
            }

            double result = 0;
            foreach (double value in memory)
            {
                result += value;
            }
            Console.WriteLine($"Part 1 sum: {result}");


            ////
            //// PART 2  TAKES A MINUTE TO RUN...
            ////

            List<KeyValuePair<double, double>> memory2 = new List<KeyValuePair<double, double>>();
            for (int i = 0; i < input.Length;)
            {
                string mask = "";
                if (input[i].Contains("mask = "))
                {
                    mask = input[i].Substring(7, 36);
                    i++;
                }
                while (i < input.Length && !input[i].Contains("mask = "))
                {
                    int start = input[i].IndexOf(" = ") + 3;
                    double number = double.Parse(input[i].Substring(start));

                    int length = input[i].IndexOf("]") - 4;
                    int rawAddress = int.Parse(input[i].Substring(4, length));
                    double[] addresses = MaskAddresses(rawAddress, mask);

                    foreach (double address in addresses)
                    {
                        bool remove = false;
                        double value = 0;
                        for (int j = 0; j < memory2.Count; j++)
                        {
                            if (memory2[j].Key == address)
                            {
                                value = memory2[j].Value;
                                remove = true;
                            }
                        }
                        if (remove)
                        {
                            memory2.Remove(new KeyValuePair<double, double>(address, value));
                        }
                        memory2.Add(new KeyValuePair<double, double>(address, number));
                        
                    }
                    
                    i++;
                }
            }

            result = 0;
            foreach (KeyValuePair<double, double> item in memory2)
            {
                result += item.Value;
            }
            Console.WriteLine($"Part 2 sum: {result}");

        }
        static string Mask(string num, string mask)
        {
            string maskedNum = "";
            for (int i = 0; i < 36; i++)
            {
                if (mask[i] != 'X')
                {
                    maskedNum += mask[i];
                }
                else
                {
                    maskedNum += num[i];
                }
            }
            return maskedNum;
        }
        static double[] MaskAddresses(int raw, string mask)
        {
            string address = ToBinary(raw);
            string rawMasked = "";
            for (int i = 0; i < 36; i++)
            {
                if (mask[i] == '0')
                {
                    rawMasked += address[i];
                }
                else
                {
                    rawMasked += mask[i];
                }
            }

            int XsInMask = 0;
            foreach (char c in mask)
            {
                if (c == 'X')
                {
                    XsInMask++;
                }
            }

            int length = (int)Math.Pow(2, XsInMask);
            
            string[] X = new string[length];
            for (int i = 0; i < length; i++)
            {
                X[i] = ToBinary(i);
            }

            string[] noX = new string[length];      
            for (int i = 0; i < length; i++)
            {
                int index = 35;
                foreach (char c in rawMasked)
                {
                    if (c == 'X')
                    {
                        noX[i] += X[i][index];
                        index--;
                    }
                    else
                    {
                        noX[i] += c;
                    }
                }
            }
            

            double[] maskedAddresses = new double[length];
            for (int i = 0; i < length; i++)
            {
                maskedAddresses[i] = ToDecimal(noX[i]);
            }
            
            return maskedAddresses;
        }

        static double ToDecimal(string binaryNum)
        {
            double decimalNum = 0;
            for (int i = 35; i >= 0; i--)
            {
                string parse = "";
                parse += binaryNum[i];
                double power = 35 - i;
                decimalNum += int.Parse(parse) * Math.Pow(2, power);
            }        
            return decimalNum;
        }

        static string ToBinary(double decimalNum)
        {
            string[] binaryNum = new string[36];
            for (int i = 35; i >= 0; i--)
            {
                double power = 36 - i;
                if (decimalNum % Math.Pow(2, power) != 0)
                {
                    binaryNum[i] = "1";
                    decimalNum -= Math.Pow(2, power - 1);
                }
                else
                {
                    binaryNum[i] = "0";
                }
            }

            string binaryNumber = "";
            foreach (string num in binaryNum)
            {
                binaryNumber += num;
            }
            return binaryNumber;
        }
    }
}
