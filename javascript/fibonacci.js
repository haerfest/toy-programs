function fib(n) {
    if (n < 2) {
        return 1;
    }
    return fib(n - 2) + fib(n - 1);
}

console.time("fibonacci");
fib(40);
console.timeEnd("fibonacci");
