# While awaiting a pull request of the official package BackpropNeuralNet:
#
# julia> Pkg.clone("https://github.com/TotalVerb/BackpropNeuralNet.jl.git")
# julia> Pkg.checkout("BackpropNeuralNet", "patch-1")
import BackpropNeuralNet

# Train a neural network for #epochs on training samples.
function train(net, epochs, samples)
    @printf "training for %u epochs...\n" epochs
    @time for epoch in 1:epochs
        for (input, desired) in shuffle(samples)
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
    mse = 0.0
    for (input, desired) in samples
        output = BackpropNeuralNet.net_eval(net, input)
        mse += sum(desired - output) ^ 2
        @printf "input: %s  expected: %s  output: %s\n" input desired output
    end
    mse *= 0.5
    @printf "mean squared error: %.5f\n" mse
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
train(net, epochs, trainingset)

# Show how the net performs on the testset.
show(net, testset)

# Example runs:
#
# training for 10000 epochs...
#   0.312623 seconds (4.10 M allocations: 99.007 MB, 2.85% gc time)
# input: [0.0,0.0]  expected: [0.0]  output: [0.5013614681298871]
# input: [0.0,1.0]  expected: [1.0]  output: [0.497504113990461]
# input: [1.0,0.0]  expected: [1.0]  output: [0.9725400358585657]
# input: [1.0,1.0]  expected: [0.0]  output: [0.03535333579622352]
# mean squared error: 0.25293
#
# training for 10000 epochs...
#   0.281678 seconds (4.10 M allocations: 99.007 MB, 3.16% gc time)
# input: [0.0,0.0]  expected: [0.0]  output: [0.02148932340391705]
# input: [0.0,1.0]  expected: [1.0]  output: [0.9800237406902774]
# input: [1.0,0.0]  expected: [1.0]  output: [0.9754353635330041]
# input: [1.0,1.0]  expected: [0.0]  output: [0.019038310651601782]
# mean squared error: 0.00091
