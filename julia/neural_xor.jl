# While awaiting a pull request of the official package BackpropNeuralNet:
#
# julia> Pkg.clone("https://github.com/TotalVerb/BackpropNeuralNet.jl.git")
# julia> Pkg.checkout("BackpropNeuralNet", "patch-1")
using BackpropNeuralNet

# Represents a single sample to feed to the network.
type Sample
    input :: Array{Float64,1}     # input vector
    expected :: Array{Float64,1}  # expected output vector
end

"""
Trains a neural network for #epochs using the samples.
"""
function train(net :: NeuralNetwork, epochs :: Int, samples :: Array{Sample,1})
    @printf "training for %u epochs...\n" epochs
    @time for epoch in 1:epochs
        for sample in shuffle(samples)
            BackpropNeuralNet.train(net, sample.input, sample.expected)
        end
    end
end

"""
Outputs how the neural network performs on test samples. Returns the mean
squared error.
"""
function show(net :: NeuralNetwork, samples :: Array{Sample,1})
    mse = 0.0
    for sample in samples
        output = net_eval(net, sample.input)
        error = sample.expected - output
        mse += sum(error) ^ 2
        @printf "input: %s  expected: %s  output: %s\n" sample.input sample.expected output
    end
    mse *= 0.5
    @printf "mean squared error: %.5f\n" mse
    mse
end

"""
Demonstrates training and using a neural network to implement the XOR function.
"""
function demo()
    # Train for this many epochs by default.
    demo(10_000)
end

function demo(epochs :: Int)
    # Training set for learning the XOR function.
    trainingset = [
        Sample([0.0, 0.0], [0.0]),  # 0 xor 0 = 0
        Sample([0.0, 1.0], [1.0]),  # 0 xor 1 = 1
        Sample([1.0, 0.0], [1.0]),  # 1 xor 0 = 1
        Sample([1.0, 1.0], [0.0])   # 1 xor 1 = 0
    ]

    # For this simple function the testset equals the trainingset.
    testset = trainingset

    # Build a neural network with two input neurons, one hidden layer with also two
    # neurons, and an output layer consisting of one neuron.
    net = init_network([2, 2, 1])

    # Train the network.
    train(net, epochs, trainingset)

    # Show how the net performs on the testset.
    show(net, testset)
end

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
