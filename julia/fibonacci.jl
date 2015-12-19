function fib(n)
    if n == 0 || n == 1
        return 1
    else
        return fib(n - 2) + fib(n - 1)
    end
end

function memoizedfib(n)
    memory = Dict{Int,Int}()

    function internal(n)
        if !haskey(memory, n)
            memory[n] = fib(n - 2) + fib(n - 1)
        end
        return memory[n]
    end

    return internal(n)
end

# julia> include("fibonacci.jl")
#   0.988681 seconds (1.15 k allocations: 58.018 KB)
# 165580141
#   0.990496 seconds (2.48 k allocations: 126.052 KB)
# 165580141

@time n = fib(40)
println(n)

@time n = memoizedfib(40)
println(n)
