#!/usr/bin/env python3.7

import sys, random, numpy as np

numInputs  = int(sys.argv[1])
numNeurons = int(sys.argv[2])
if len(sys.argv) > 3:
    random.seed(int(sys.argv[3]))

outfile = open("TEMPBIN", "wb")

print(numInputs,numNeurons)
x = [ float(numInputs), float(numNeurons) ]

inputs = [ round(random.random()/numInputs,4)  for i in range(numInputs) ]
print(inputs)
x.extend( inputs )

for rowidx in range(numNeurons):
    wtsRow = [ random.randrange(1,10)/10  for i in range(numInputs) ]
    print(wtsRow)
    x.extend(wtsRow)

x = np.array(x, dtype="float64")   # dtype='double')
x.tofile(outfile)
