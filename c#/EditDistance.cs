using System;
using System.Diagnostics;


public class EditDistanceTest
{
  private static int Min(int x, int y, int z)
  {
    var min = (x < y) ? x : y;
    return (min < z) ? min : z;
  }

  // https://www.wikiwand.com/en/Wagner%E2%80%93Fischer_algorithm
  private static int EditDistance(string s, string t)
  {
    var m = s.Length;
    var n = t.Length;

    var d = new int[m + 1, n + 1];

    for (var i = 0; i <= m; i++)
      d[i, 0] = i;
    for (var j = 0; j <= n; j++)
      d[0, j] = j;
    
    for (var j = 1; j <= n; j++)
      for (var i = 1; i <= m; i++)
        if (s[i - 1] == t[j - 1])
          d[i, j] = d[i - 1, j - 1];
        else
          d[i, j] = 1 + Min(d[i - 1, j],  d[i, j - 1], d[i - 1, j - 1]);
  
    return d[m, n];
  }

  public static void Main(string[] args)
  {
    var iterationCount = (args.Length > 0) ? Int32.Parse(args[0]) : 10000000;
    
    var watch = new Stopwatch();
    watch.Start();

    for (var i = 0; i < iterationCount; i++) {
      EditDistance("kitten", "sitting");
    }

    watch.Stop();

    Console.WriteLine("{0} iterations: {1} elapsed", iterationCount, watch.Elapsed);
  }
}
