using System;

namespace Day25
{
    class Program
    {
        static void Main(string[] args)
        {
            //int cardPK = 5764801;  //TEST
            //int doorPK = 17807724; //TEST
            int cardPK = 3469259;
            int doorPK = 13170438;
            int subjectNum = 7;
            
            long value = 1;
            int cardLoopSize = 0;
            while(value != cardPK)
            {
                value *= subjectNum;
                value %= 20201227;
                cardLoopSize++;
            }

            value = 1;
            int doorLoopSize = 0;
            while (value != doorPK)
            {
                value *= subjectNum;
                value %= 20201227;
                doorLoopSize++;
            }

            Console.WriteLine("Encryption Key: ");
            value = 1;
            for (int i = 0; i < cardLoopSize; i++)
            {
                value *= doorPK;
                value %= 20201227;
            }
            Console.WriteLine(value);
            value = 1;
            for (int i = 0; i < doorLoopSize; i++)
            {
                value *= cardPK;
                value %= 20201227;
            }
            Console.WriteLine(value);

        }
    }
}
