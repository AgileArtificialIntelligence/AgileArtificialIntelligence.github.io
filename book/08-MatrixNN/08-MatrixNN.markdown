
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


The initialization of a layer simply set the value of the learning rate:

```Smalltalk
NMLayer>>initialize
	super initialize.
	lr := 0.1
```

The class `NMLayer` contains many accessors and mutator methods. First, a layer contains a matrix for the weight. It is set using `w:`:


```Smalltalk
NMLayer>>w: matrixForWeights
	"Take a MMatrix as argument"
	w := matrixForWeights
```

The weight matrix is accessible using `w`:

```Smalltalk
NMLayer>>w
	"Return a MMatrix"
	^ w
```

Similarly, the bias vector is set using `b:`:

```Smalltalk
NMLayer>>b: biasVector
	"Set a vector, instance of MMatrix, as the bias vector"
	b := biasVector
```

The bias vector is accessible using:

```Smalltalk
NMLayer>>b
	"Return the bias vector"
	^ b
```

The delta matrix is stored in the variable `delta`:

```Smalltalk
NMLayer>>delta: deltaMatrix
	delta := deltaMatrix
```

It is read using an accessor:

```Smalltalk
NMLayer>>delta
	^ delta
```

The learning rate, a very small positive number, is globally set of a layer:

```Smalltalk
NMLayer>>lr: aLearningRate
	lr := aLearningRate
```

Layers are chained each other. We use the classical representation of layers: the network is fed from the left-most layer, the input layer. Output is produced from the right-most layer, the output layer. For a given layer `l`, the next layer of `l` is the layer on the right of `l`, and the previous is the layer on the left of `l`. The next layer is set using:

```Smalltalk
NMLayer>>next: aLayer
	"Set the next layer"
	next := aLayer
```

The next layer is retrieved using:

```Smalltalk
NMLayer>>next
	"Return the next layer"
	^ next
```

Similarly, the previous layer is set using:

```Smalltalk
NMLayer>>previous: aLayer
	"Set the previous layer"
	previous := aLayer
```

The previous layer is obtained using:

```Smalltalk
NMLayer>>previous
	"Return the previous layer"
	^ previous
```

The output of the layer is obtained using its accessor:

```Smalltalk
NMLayer>>output
	"Return the output matrix, computed during the feed forward phase"
	^ output
```

The number of examples needs to be accessible to compute the cost derivative. It is set using the method `numberOfExamples:`:

```Smalltalk
NMLayer>>numberOfExamples: aNumber
	numberOfExamples := aNumber
```

The number of example is read using its corresponding accessor:

```Smalltalk
NMLayer>>numberOfExamples
	^ numberOfExamples
```

The layer is initialized by providing the number of neurons it should contains and the number of outputs. The random number generator is also provided to initialize the weight and bias matrices. We defining the method:

```Smalltalk
NMLayer>>nbInputs: nbOfNeurons nbOutputs: nbOfInputs random: random
	"Initialize the layer"
	w := MMatrix newRows: nbOfNeurons columns: nbOfInputs.
	w random: random.
	b := MMatrix newRows: nbOfNeurons columns: 1.
	b random: random.
```

Feed forwarding a layer is carried out using the `feed:` method:

```Smalltalk
NMLayer>>feed: inputMatrix
	"Feed the layer with the input matrix"
	output := (w +* inputMatrix + b) collect: [ :v | 1 / (1 + v negated exp) ].
	^ output
```

Once the error are backpropated, weights and biases can be updated using:

```Smalltalk
NMLayer>>update
	w := w - ((delta +* previous output transposed) * lr / numberOfExamples ).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next ifNotNil: [ next update ]
```

The very first layer uses the input vector to actually update its parameters:

```Smalltalk
NMLayer>>update: input
	w := w - ((delta +* input transposed) * lr / numberOfExamples).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next update

```

Our definition of layer is now complete. We can lay out the necessary to hook layers together using the class `NMNetwork`.

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