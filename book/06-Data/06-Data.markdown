
# Classification 

This chapter covers the classification and regression of data, which are the most prominent applications of neural networks.

## Support to easily train network

In the previous chapter, we have seen that we can obtain a trained neural network to express the XOR logical gate with:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 1.

10000 timesRepeat: [ 
	n train: { 0 . 0 } desiredOutputs: { 0 }.	
	n train: { 0 . 1 } desiredOutputs: { 1 }.
	n train: { 1 . 0 } desiredOutputs: { 1 }.
	n train: { 1 . 1 } desiredOutputs: { 0 }.
].
```

After evaluating this script, the expression `n feed: {1 . 0}` evaluates to `#(0.9735546630024936)`, an array having an expected float value close to 1.

The example is actually very verbose. 

We define the following method:

```Smalltalk
NNetwork>>train: train nbEpoch: nbEpoch
	"Train the network using the train data set."
	| sumError outputs expectedOutput t |
	1 to: nbEpoch do: [ :epoch |
		sumError := 0.
		train do: [ :row |
			outputs := self feed: row allButLast.
			expectedOutput := (1 to: self numberOfOutputs) collect: [ :notUsed | 0 ].
			expectedOutput at: (row last) + 1 put: 1.
			 
			t := (1 to: expectedOutput size) collect: [ :i | ((expectedOutput at: i) - (outputs at: i)) raisedTo: 2 ].
			sumError := sumError + t sum.
			self backwardPropagateError: expectedOutput.
			self updateWeight: row allButLast.
		].
		errors add: sumError
	] 
```

```Smalltalk
NNetwork>>predict: inputs
	"Make a prediction. This method assume that the number of outputs is the same than the number of different values the network can output"
	"The index of a collection begins at 1 in Pharo"
	| outputs |
	outputs := self feed: inputs.
	^ (outputs indexOf: (outputs max)) - 1
```

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := { 
	{ 0 . 0 . 0 } .
	{ 0 . 1 . 1 } .
	{ 1 . 0 . 1 } .
	{ 1 . 1 . 0 }
}.

n train: data nbEpoch: 10000.
```

## Classification 

Classification can be defined as grouping elements based on their features. Elements shared the similar features are grouped together. 

## One hot encoding

One hot encoding is a simple mechanism that convert categorical variables into a numerical form, eligible to be fed into a neural network. 

## Neural network as a Hashmap



## Normalization

Before 

## Iris Dataset

## Classifying


