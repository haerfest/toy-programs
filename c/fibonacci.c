#include <stdio.h>
#include <sys/time.h>

static int fib(int n)
{
    if (n < 2)
        return 1;

    return fib(n - 2) + fib(n - 1);
}

int main(int argc, char *argv[])
{
    struct timeval start, end;
    double elapsed;
    int answer;

    (void) gettimeofday(&start, NULL);
    answer = fib(42);
    (void) gettimeofday(&end, NULL);

    elapsed = (end.tv_sec   * 1000 + end.tv_usec   / 1000.0) -
              (start.tv_sec * 1000 + start.tv_usec / 1000.0);

    printf("%d, elapsed: %.3f msec\n", answer, elapsed);
    return 0;
}
