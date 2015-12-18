import time

# >>> timer(lambda: Fibonacci().recursive(40))
# time elapsed: 69.4402530193 seconds
# 165580141
# >>> timer(lambda: Fibonacci().memoized(40))
# time elapsed: 0.0000679493 seconds
# 165580141

class Fibonacci(object):
    def __init__(self):
        self.memory = {0: 1, 1: 1}

    def recursive(self, n):
        """Trivial recursive fibonacci function"""
        if n == 0 or n == 1:
            return 1
        else:
            return self.recursive(n - 1) + self.recursive(n - 2)

    def memoized(self, n):
        """Memoized fibonacci function"""
        if n in self.memory:
            return self.memory[n]
        else:
            m = self.memoized(n - 1) + self.memoized(n - 2)
            self.memory[n] = m
            return m

class Timer(object):
    @staticmethod
    def time(f):
        t0 = time.time()
        result = f()
        print("time elapsed: %.10f seconds" % (time.time() - t0))
        return result

if __name__ == "__main__":
    print(Timer.time(lambda: Fibonacci().recursive(40)))
