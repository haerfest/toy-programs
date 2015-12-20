function fib(n)
    if n == 0 || n == 1
        1
    else
        fib(n - 2) + fib(n - 1)
    end
end

function memoizedfib(n)
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

# julia> include("fibonacci.jl")
# recursive ... 165580141
#   0.986287 seconds (1.17 k allocations: 58.486 KB)
# memoized .... 165580141
#   0.009025 seconds (2.80 k allocations: 142.588 KB)

print("recursive ... ")
@time println(fib(40))

print("memoized .... ")
@time println(memoizedfib(40))
