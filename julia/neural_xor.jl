# While awaiting a pull request of the official package BackpropNeuralNet:
#
# julia> Pkg.clone("https://github.com/TotalVerb/BackpropNeuralNet.jl.git")
# julia> Pkg.checkout("BackpropNeuralNet", "patch-1")
import BackpropNeuralNet

# Train a neural network for #epochs on training samples.
function train(net, epochs, samples)
    @printf "training for %u epochs...\n" epochs
    @time for epoch in 1:epochs
        for (input, desired) in shuffle(trainingsamples)
            BackpropNeuralNet.train(net, input, desired)
        end
    end
end

# Calculate the mean squared error of a neural network on test samples.
function mse(net, samples)
    function error(acc, sample)
        (input, desired) = sample
        output = BackpropNeuralNet.net_eval(net, input)
        error = desired - output
        return acc + sum(error) ^ 2
    end
    return 0.5 * reduce(error, 0.0, samples)
end

# Prints how the neural network performs on test samples.
function show(net, samples)
    @printf "mean squared error: %.5f\n" mse(net, samples)
    for (input, desired) in samples
        output = BackpropNeuralNet.net_eval(net, input)
        @printf "input: %s  expected:%s  output: %s\n" input desired output
    end
end

# Training set for learning the XOR function ($ in Julia).
trainingset = [
    ([0.0, 0.0], [0.0]),  # 0 $ 0 = 0
    ([0.0, 1.0], [1.0]),  # 0 $ 1 = 1
    ([1.0, 0.0], [1.0]),  # 1 $ 0 = 1
    ([1.0, 1.0], [0.0])   # 1 $ 1 = 0
]

# Test set.
testset = trainingset

# Train for this many epochs.
epochs = 10_000

# Build a neural network with two input neurons, one hidden layer with also two
# neurons, and an output layer consisting of one neuron.
net = BackpropNeuralNet.init_network([2, 2, 1])

# Train the network.
train(net, epochs, trainingsamples)

# Show how the net performs on the testset.
show(net, testset)

# Example runs:
#
# training for 100 epochs...
#   0.002291 seconds (42.10 k allocations: 1.010 MB)
# mean squared error: 0.50039
# input: [0.0,0.0]  expected:[0.0]  output: [0.496763805773074]
# input: [0.0,1.0]  expected:[1.0]  output: [0.49636216254953347]
# input: [1.0,0.0]  expected:[1.0]  output: [0.5154420613227905]
# input: [1.0,1.0]  expected:[0.0]  output: [0.5153317963633542]
#
# training for 1000 epochs...
#   0.043723 seconds (421.00 k allocations: 10.101 MB, 19.64% gc time)
# mean squared error: 0.28638
# input: [0.0,0.0]  expected:[0.0]  output: [0.3864448795724854]
# input: [0.0,1.0]  expected:[1.0]  output: [0.5385946568998659]
# input: [1.0,0.0]  expected:[1.0]  output: [0.6878421616932553]
# input: [1.0,1.0]  expected:[0.0]  output: [0.3362890996690191]
#
# training for 10000 epochs...
#   0.244605 seconds (4.21 M allocations: 101.013 MB, 4.84% gc time)
# mean squared error: 0.00094
# input: [0.0,0.0]  expected:[0.0]  output: [0.01965052373124125]
# input: [0.0,1.0]  expected:[1.0]  output: [0.9773738046631867]
# input: [1.0,0.0]  expected:[1.0]  output: [0.9773747516783217]
# input: [1.0,1.0]  expected:[0.0]  output: [0.021649070817093864]
