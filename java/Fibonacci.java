public class Fibonacci
{
    private static int fib(int n)
    {
        if (n < 2)
        {
            return 1;
        }

        return fib(n - 2) + fib(n - 1);
    }

    public static void main(String[] args)
    {
        long start = System.nanoTime();
        int answer = fib(42);
        long elapsed = (long) ((System.nanoTime() - start) / 1e6);

        System.out.format("%1$d, elapsed: %2$d milliseconds\n", answer, elapsed);
    }
}
