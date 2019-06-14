
# Matrix-based Neural Network

This chapter revises the implementation of our neural network. However, it uses matrices to compute the forward and backward propagation algorithm. Overall, our matrix-based implementation is composed of two classes, `NMLayer` and `NMNetwork`.

## Layer

A layer may be described with the class `NMLayer`:

```Smalltalk
Object subclass: #NMLayer
	instanceVariableNames: 'w b delta output previous next lr numberOfExamples'
	classVariableNames: ''
	package: 'NeuralNetwork-Matrix'
```

The class `NMLayer` does not contains neurons, as we have seen in our first implementation. Instead, a matrix describing weights is used, kept in the `w` variable, and another matrix to keep the bias vector, kept in the `b` variable. 


The initialization of a layer set the value of the learning rate:
```Smalltalk
NMLayer>>initialize
	super initialize.
	lr := 0.1
```

Some 
```Smalltalk
NMLayer>>b: biasVector
	b := biasVector
```

```Smalltalk
NMLayer>>b
	^ b
```

```Smalltalk
NMLayer>>delta: deltaMatrix
	delta := deltaMatrix
```

```Smalltalk
NMLayer>>delta
	^ delta
```

```Smalltalk
NMLayer>>lr: aLearningRate
	lr := aLearningRate
```

```Smalltalk
NMLayer>>lr
	^ lr
```

```Smalltalk
NMLayer>>next: aLayer
	next := aLayer
```

```Smalltalk
NMLayer>>next
	^ next
```

```Smalltalk
NMLayer>>nbInputs: nbOfNeurons nbOutputs: nbOfInputs random: random
   w := MMatrix newRows: nbOfNeurons columns: nbOfInputs.
	w random: random.
	b := MMatrix newRows: nbOfNeurons columns: 1.
	b random: random.
	
```

```Smalltalk
NMLayer>>numberOfExamples
	^ numberOfExamples
```

```Smalltalk
NMLayer>>numberOfExamples: anObject
	numberOfExamples := anObject
```

```Smalltalk
NMLayer>>output
	^ output
```

```Smalltalk
NMLayer>>previous
	^ previous
```

```Smalltalk
NMLayer>>previous: aLayer
	previous := aLayer
```

```Smalltalk
NMLayer>>feed: inputMatrix
	"Feed the layer with the input matrix"
	output := (w +* inputMatrix + b) collect: [ :v | 1 / (1 + v negated exp) ].
	^ output
```

```Smalltalk
NMLayer>>
```

```Smalltalk
NMLayer>>
```

```Smalltalk
NMLayer>>
```

```Smalltalk
NMLayer>>update
	w := w - ((delta +* previous output transposed) * lr / numberOfExamples ).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next ifNotNil: [ next update ]
```

```Smalltalk
NMLayer>>update: input
	w := w - ((delta +* input transposed) * lr / numberOfExamples).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next update

```

```Smalltalk
NMLayer>>w: matrixForWeights
	w := matrixForWeights
```

```Smalltalk
NMLayer>>w
	^ w
```

## Neural network

```Smalltalk
Object subclass: #NMNetwork
	instanceVariableNames: 'random errors layers precisions'
	classVariableNames: ''
	package: 'NeuralNetwork-Matrix'
```

```Smalltalk
NMNetwork>>initialize
	super initialize.
	layers := OrderedCollection new.
	random := Random seed: 42.
```

```Smalltalk
NMNetwork>>addLayer: aLayer
	layers ifNotEmpty: [
		layers last next: aLayer. 
		aLayer previous: layers last ].
	layers add: aLayer
```



```Smalltalk
NMNetwork>>backwardX: x y: y
	| lastLayer dz currentLayer |
	lastLayer := layers last.
	dz := lastLayer output - y.	
	lastLayer delta: dz.
	currentLayer := lastLayer previous. 
	[ currentLayer notNil ] whileTrue: [ 
		dz := (currentLayer next w transposed +* dz) 
					multiplyPerElement: (currentLayer output collect: [ :v | v * (1 - v) ]).
		currentLayer delta: dz.
		currentLayer := currentLayer previous.
	].

```

```Smalltalk
NMNetwork>>computeCost: mat and: y
	^ ((mat - y) collect: [ :v | v * v ]) sum
```



```Smalltalk
NMNetwork>>configure: nbOfInputs hidden: nbOfNeurons nbOfOutputs: nbOfOutputs
    "Configure the network with the given parameters
    The network has only one hidden layer"
	self addLayer: (NMLayer new nbInputs: nbOfNeurons nbOutputs: nbOfInputs random: random).
	self addLayer: (NMLayer new nbInputs: nbOfOutputs nbOutputs: nbOfNeurons random: random).
```



```Smalltalk
NMNetwork>>configure: nbOfInputs hidden: nbOfNeurons1 hidden: nbOfNeurons2 nbOfOutputs: nbOfOutputs
    "Configure the network with the given parameters. The network has two hidden layers"
	self addLayer: (NMLayer new nbInputs: nbOfNeurons1 nbOutputs: nbOfInputs random: random).
	self addLayer: (NMLayer new nbInputs: nbOfNeurons2 nbOutputs: nbOfNeurons1 random: random).
	self addLayer: (NMLayer new nbInputs: nbOfOutputs nbOutputs: nbOfNeurons2 random: random).
```

```Smalltalk
NMNetwork>>configure: nbOfInputs hidden: nbOfNeurons nbOfOutputs: nbOfOutputs
    "Configure the network with the given parameters. The network has only one hidden layer"
	self addLayer: (NMLayer new nbInputs: nbOfNeurons nbOutputs: nbOfInputs random: random).
	self addLayer: (NMLayer new nbInputs: nbOfOutputs nbOutputs: nbOfNeurons random: random).
	
```



```Smalltalk
NMNetwork>>feed: inputs
	"Feed the network with the provided inputs vector"
	| mat |
	mat := inputs.
	layers do: [ :l | mat := l feed: mat ].
	^ mat
```



```Smalltalk
NMNetwork>>lr: aLearningRateAsFloat
	"Globally set the learning rate"
	layers do: [ :l | l lr: aLearningRateAsFloat ]
```



```Smalltalk
NMNetwork>>modelX: x y: y nbOfEpochs: nbEpochs
	| cost output |
	"We need to tell to each layer the number of examples they have"
	layers do: [ :l | l numberOfExamples: y nbColumns ].
	
	errors := OrderedCollection new.
	precisions := OrderedCollection new.
	nbEpochs timesRepeat: [ 
		output := self feed: x.
		cost := self computeCost: output and: y.
		self backwardX: x  y: y.
		self update: x.
		errors add: cost.
	].
	^ cost
```


We can simply copy the predict method from our original implementation:

```Smalltalk
NMNetwork>>predict: inputs
	"Make a prediction. This method assume that the number of outputs is the same than the number of different values the network can output"
	"The index of a collection begins at 1 in Pharo"
	| outputs |
	outputs := self feed: inputs.
	^ (outputs asArray indexOf: (outputs max)) - 1
```




```Smalltalk
NMNetwork>>train: data nbEpochs: nbEpochs
	"Data is provided as a collection of arrays."
	| x y labels numberOfOutputs |
	x := (MMatrix newFromArrays: (data collect: #allButLast)) transposed.
	layers do: [ :l | l numberOfExamples: data size ].
	labels := data collect: #last.
	numberOfOutputs := labels asSet size.
	labels := labels collect: [ :row |
		| expectedOutput |
		expectedOutput := Array new: numberOfOutputs withAll: 0.
   		expectedOutput at: row + 1 put: 1.
		expectedOutput
	].
	y := (MMatrix newFromArrays: labels) transposed.
	^ self modelX: x y: y nbOfEpochs: nbEpochs

```



```Smalltalk
NMNetwork>>update: input
	"Update the weights and bias using the provided input vector"
	layers first update: input
```


## Visualization of the results

```Smalltalk
NMNetwork>>viewLearningCurve
	| b ds |
	errors
		ifEmpty: [ ^ RTView new
				add: (RTLabel elementOn: 'Should first run the network');
				yourself ].
	b := RTGrapher new.

	"We define the size of the charting area"
	b extent: 500 @ 300.
	ds := RTData new.
	ds samplingIfMoreThan: 2000.
	ds noDot.
	ds connectColor: Color blue.
	ds points: (errors collectWithIndex: [ :y :i | i -> y ]).
	ds x: #key.
	ds y: #value.
	ds dotShape rectangle color: Color blue.
	b add: ds.

	b axisX
		noDecimal;
		title: 'Epoch'.
	b axisY title: 'Error'.
	^ b
```


```Smalltalk
NMNetwork>>viewLearningCurveIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: 'Cost';
		initializeView: [
			self viewLearningCurve ]
```

## Iris Dataset

We can now adapt the script to train a neural network on the Iris dataset. Consider the script:

```Smalltalk
"The execution of this script initializes the variable irisData.
     This variable is used in the subsequent scripts of this chapter"
irisCSV := (ZnEasy get: 'https://agileartificialintelligence.github.io/Datasets/iris.csv') contents.
lines := irisCSV lines.
lines := lines allButFirst.
tLines := lines collect: [ :l |
             | ss |
             ss := l substrings: ','.
             (ss allButLast collect: [ :w | w asNumber ]), (Array with: ss
last) ].
irisData := tLines collect: [ :row | |l|
             row last = 'setosa' ifTrue: [ l := #( 0 ) ].
             row last = 'versicolor' ifTrue: [ l := #( 1 ) ].
             row last = 'virginica' ifTrue: [ l := #( 2 ) ].
             row allButLast, l ].
irisData.

n := NMNetwork new.
n configure: 4 hidden: 56 nbOfOutputs: 3. 
n train: irisData nbEpochs: 1000. 
n
```

The result is the same than we we previously seen