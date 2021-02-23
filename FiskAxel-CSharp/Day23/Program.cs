using System;
using System.Collections.Generic;

namespace Day23
{
    public class Cup
    {
        public int Data { get; set; }
        public Cup Next { get; set; }

        public Cup(int data)
        {
            this.Data = data;
            this.Next = null;
        }
    }

    public class CircleOfCups
    {
        private Cup last;
        public Dictionary<int, Cup> map = new Dictionary<int, Cup>();
        public CircleOfCups()
        {

        }

        public void Move(int loops, int highest)
        {
            for (int i = 0; i < loops; i++)
            {
                Cup current = last.Next;
                Cup first = current.Next;
                Cup second = first.Next;
                Cup third = second.Next;
                int destiNum = (current.Data - 1);
                while (destiNum == first.Data || destiNum == second.Data || 
                    destiNum == third.Data || destiNum == 0)
                {
                    if (destiNum == 0)
                    {
                        destiNum = highest;
                        continue;
                    }
                    destiNum--;
                }
                Cup destination = Find(destiNum);

                last = last.Next;
                current.Next = third.Next;
                third.Next = destination.Next;
                destination.Next = first;
            }
        }

        public void AddLast(int data)
        {
            Cup newNode = new Cup(data);
            map.Add(data, newNode);
            if (last == null)
            {
                last = newNode;
                last.Next = newNode;
                return;
            }

            Cup oldLast = last;
            newNode.Next = last.Next;
            oldLast.Next = newNode;
            last = newNode;           
        }

        public Cup Find(int value)
        {
            return map[value];
        }

        public void Print()
        {
            Cup current = Find(1).Next;
            do
            {
                Console.Write(current.Data);
                current = current.Next;
            } while (current.Data != 1);
            Console.WriteLine();
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            //string puzzleInput = "389125467"; // TEST
            string puzzleInput = "562893147";
            int[] input = new int[9];
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                input[i] = int.Parse(puzzleInput.Substring(i, 1));
            }


            CircleOfCups cups = new CircleOfCups();
            for (int i = 0; i < input.Length; i++)
            {
                cups.AddLast(input[i]);
            }

            cups.Move(100, 9);
            Console.WriteLine("Part 1: ");
            cups.Print();



            CircleOfCups cups2 = new CircleOfCups();
            for (int i = 0; i < input.Length; i++)
            {
                cups2.AddLast(input[i]);
            }
            for (int i = 10; i <= 1000000; i++)
            {
                cups2.AddLast(i);
            }
            cups2.Move(10000000, 1000000);


            Cup one = cups2.Find(1);
            long num1 = one.Next.Data;
            long num2 = one.Next.Next.Data;
            long result = num1 * num2;
            Console.WriteLine("Part 2: ");
            Console.WriteLine(result);
        }
    }
}