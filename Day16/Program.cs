using System;
using System.Collections.Generic;
using System.IO;

namespace Day16
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] ticketRules = File.ReadAllLines("../../../ticketRules.txt");
            string[] yourTicket = File.ReadAllLines("../../../yourTicket.txt");
            string[] nearbyTickets = File.ReadAllLines("../../../nearbyTickets.txt");

            string[] parsedRules = new string[ticketRules.Length];
            List<int> validNumbers = new List<int>();
            List<int> invalidNumbers = new List<int>();
            for (int i = 0; i < ticketRules.Length; i++)
            {
                string[] parse1 = ticketRules[i].Split(": ");
                string[] parse2 = parse1[1].Split(" or ");
                parsedRules[i] += parse2[0];
                parsedRules[i] += ',';
                parsedRules[i] += parse2[1];
                foreach (string hiLo in parse2)
                {
                    string[] parse3 = (hiLo.Split("-"));
                    
                    int lo = int.Parse(parse3[0]);
                    int hi = int.Parse(parse3[1]);                    
                    for (int j = lo; j <= hi ; j++)
                    {
                        if (!validNumbers.Contains(j))
                        {
                            validNumbers.Add(j);
                        }
                        
                    }
                }
            }

            ////
            //// PART 1
            //// 

            foreach (string ticket in nearbyTickets)
            {
                string[] nums = ticket.Split(',');
                for (int i = 0; i < nums.Length; i++)
                {
                    int number = int.Parse(nums[i]);
                    if (!validNumbers.Contains(number))
                    {
                        invalidNumbers.Add(number);
                    }
                }
            }

            double result = 0;
            foreach (int num in invalidNumbers)
            {
                result += num;
            }

            Console.Write("Part 1: ");
            Console.WriteLine(result);


            //// 
            //// PART 2
            ////

            List<string> validTickets = new List<string>();
            foreach (string ticket in nearbyTickets)
            {
                bool validTicket = true;
                string[] nums = ticket.Split(',');
                for (int i = 0; i < nums.Length; i++)
                {
                    int number = int.Parse(nums[i]);
                    if (invalidNumbers.Contains(number))
                    {
                        validTicket = false;
                    }
                }
                if (validTicket)
                {
                    validTickets.Add(ticket);
                }
            }

            string[] fields = new string[ticketRules.Length];
            for (int i = 0; i < fields.Length; i++)
            {
                for (int j = 0; j < validTickets.Count; j++)
                {
                    string[] nums = validTickets[j].Split(',');
                    fields[i] += nums[i];
                    fields[i] += ',';
                }
            }

            string[] positions = new string[parsedRules.Length];
            for (int i = 0; i < fields.Length; i++)
            {
                string[] thisField = fields[i].Split(',');
                for (int j = 0; j < parsedRules.Length; j++)
                {
                    string[] hiLo = (parsedRules[j].Split(','));
                    string[] hiLo1 = hiLo[0].Split('-');
                    string[] hiLo2 = hiLo[1].Split('-');
                    int lo1 = int.Parse(hiLo1[0]);
                    int hi1 = int.Parse(hiLo1[1]);
                    int lo2 = int.Parse(hiLo2[0]);
                    int hi2 = int.Parse(hiLo2[1]);

                    bool match = true;
                    for (int k = 0; k < thisField.Length - 1; k++)
                    {
                        int num = int.Parse(thisField[k]);
                        if (!(lo1 <= num && num <= hi1 || lo2 <= num && num <= hi2))
                        {
                            match = false;
                        }
                    }

                    if (match)
                    {
                        if (j < 10)
                        {
                            positions[i] += 0; 
                        }
                        positions[i] += j;
                        positions[i] += ',';
                    }
                }
            }
            
            string[] finalPositions = new string[positions.Length];
            bool completed = false;
            while(!completed)
            {
                for (int i = 0; i < positions.Length; i++)
                {
                    for (int j = 0; j < finalPositions.Length; j++)
                    {
                        if (finalPositions[j] != null && positions[i].Contains(finalPositions[j]))
                        {
                            int index = positions[i].IndexOf(finalPositions[j]);
                            positions[i] = positions[i].Remove(index, 3);
                        }
                    }
                }

                for (int i = 0; i < positions.Length; i++)
                {
                    if (positions[i].Length == 3 && positions[i].Length > 0)
                    {
                        finalPositions[i] = positions[i];
                    }                   
                }

                completed = true;
                for (int i = 0; i < finalPositions.Length; i++)
                {
                    if (finalPositions[i] == null)
                    {
                        completed = false;
                        break;
                    }
                }
            }

            result = 1;
            for (int i = 0; i < 6; i++)
            {
                string num = '0' + i.ToString() + ',';
                int index = Array.IndexOf(finalPositions, num);
                string[] youTicket = yourTicket[0].Split(',');
                int number = int.Parse(youTicket[index]);
                result *= number;
            }

            Console.Write("Part 2: ");
            Console.WriteLine(result);
        }
    }
}
