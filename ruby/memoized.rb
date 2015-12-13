# > time { Fibonacci.new.recursive(40) }
# time elapsed: 21.0984350000 seconds
# => 165580141
# > time { Fibonacci.new.memoized(40) }
# time elapsed: 0.0000380000 seconds
# => 165580141

class Fibonacci
    def initialize
        @memory = {0 => 1, 1 => 1}
    end

    def recursive(n)
        if n == 0 or n == 1
            1
        else
            recursive(n - 1) + recursive(n - 2)
        end
    end

    def memoized(n)
        if @memory.has_key?(n)
            @memory[n]
        else
            @memory[n] = memoized(n - 1) + memoized(n - 2)
        end
    end
end

def time(&block)
    t0 = Time.now
    result = yield block
    t1 = Time.now
    puts "time elapsed: %.10f seconds" % (t1 - t0)
    return result
end
