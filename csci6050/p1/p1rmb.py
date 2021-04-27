#!/usr/bin/env python3.7

# takes about 100 secs ? on 128 x 1000000

import sys, math, random, numpy as np

infilename = sys.argv[1]

infile = open(infilename, "rb")

x = np.fromfile(infile)
# print(x.dtype)  # float64
print("PAST INPUT")

numInputs  = int( x[0] )
numNeurons = int( x[1] )

inputs = x[2:numInputs+2]
w = x[numInputs+2:]
weights = []
for i in range(0,numNeurons):
    s = i * numInputs
    weights.append(w[s:s+numInputs])
print("PAST CREATE ARRAYS")

# print(inputs[0])
# print(weights[0])

outputs = []
for nidx in range(numNeurons):
    output = 0.0
    for iidx in range(numInputs):
        output += inputs[iidx] * weights[nidx][iidx]
    output = 1.0 / (1.0 + math.exp(-output))  # sigmoid
    outputs.append(output)

if "-t" not in sys.argv:
    for (idx,output) in enumerate(outputs):
        print(idx, round(output,6) )
print( "total", round(sum(outputs),6) )
