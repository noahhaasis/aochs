using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace day1
{
    class Program
    {
        static void Main(string[] args)
        {
          var ints = File.ReadAllLines("input.txt")
            .Select(line => Int32.Parse(line)).ToList();
          Console.WriteLine(Solve(ints, 2, 2020));
          Console.WriteLine(Solve(ints, 3, 2020));
        }

        // Return -1 if no number is found
        static int Solve(List<int> ints, int numberOfCombinations, int searchSum) {
          if (ints.Count == 0) return -1;

          if (numberOfCombinations == 1) { // Linear search
            foreach (int i in ints) {
              if (i == searchSum) return i;
            }
            return -1;
          }

          // First try finding a solution containing the first element.
          // If that fails, try finding one without it.
          var head = ints.First();
          var tail = ints.Skip(1).ToList();
          var mRes = Solve(tail, numberOfCombinations - 1, searchSum - head);

          if (mRes != -1) { // Found a solution containing `head`
            return mRes * head;
          } else { // Try finding a solution which doesn't contain `head`
            return Solve(tail, numberOfCombinations, searchSum);
          }

          return -1;
        }
    }
}
