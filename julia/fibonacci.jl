"""
Recursive implementation of the Fibonacci sequence.
"""
function fib(n :: Int)
    if n == 0 || n == 1
        1
    else
        fib(n - 2) + fib(n - 1)
    end
end

"""
Memoized implementation of the Fibonacci sequence.
"""
function memoizedfib(n :: Int)
    memory = Dict{Int,Int}()

    memory[0] = 1
    memory[1] = 1

    function internal(n)
        if !haskey(memory, n)
            memory[n] = internal(n - 2) + internal(n - 1)
        else
            memory[n]
        end
    end

    internal(n)
end

"""
Demonstrates the speed difference between the recursive and memoized Fibonacci
sequence implementations.
"""
function demo()
    print("recursive ... ")
    @time println(fib(40))

    print("memoized .... ")
    @time println(memoizedfib(40))
end

# julia> demo()
# recursive ... 165580141
#   1.154707 seconds (15 allocations: 496 bytes)
# memoized .... 165580141
#   0.000079 seconds (93 allocations: 3.938 KB)
