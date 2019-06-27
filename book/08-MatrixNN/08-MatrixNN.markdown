
# Matrix-based Neural Network

This chapter revises the implementation of our neural network. In this revision, our network will use matrices to compute the forward and backward propagation algorithm. Overall, our matrix-based implementation is composed of two classes, `NMLayer` and `NMNetwork`. Since our most of the computation is delegated to the matrix library we defined in the previous chapter, our new version of the neural network is rather light in terms of amount of code.

## Defining a matrix-based layer
A neural network is composed of layers. We describe a layer as an instance of the class `NMLayer`, defined as follows:

```Smalltalk
Object subclass: #NMLayer
	instanceVariableNames: 'w b delta output previous next lr numberOfExamples'
	classVariableNames: ''
	package: 'NeuralNetwork-Matrix'
```

The class `NMLayer` does not contains neurons, as we have seen in our first implementation. Instead, a matrix describing weights is used, kept in the `w` variable, and another matrix to keep the bias vector, kept in the `b` variable. 


The initialization of a layer simply consists in setting a default learning rate:

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
	"Return the MMatrix representing the weights"
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

The layer is initialized by providing the number of neurons it should contains and the number of outputs. The random number generator is also provided to initialize the weight and bias matrices. We define the initialization method:

```Smalltalk
NMLayer>>nbInputs: nbOfInputs nbOutputs: nbOfOutputs random: random
	"Initialize the layer"
	w := MMatrix newRows: nbOfOutputs columns: nbOfInputs.
	w random: random.
	b := MMatrix newRows: nbOfOutputs columns: 1.
	b random: random.
```

Feed forwarding a layer is carried out using the `feed:` method:

```Smalltalk
NMLayer>>feed: inputMatrix
	"Feed the layer with the input matrix"
	output := (w +* inputMatrix + b) collect: [ :v | 1 / (1 + v negated exp) ].
	^ output
```

Once the error is backpropagated, weights and biases can be updated using:

```Smalltalk
NMLayer>>update
	"Update the weights and biases using the delta value"
	w := w - ((delta +* previous output transposed) * lr / numberOfExamples).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next ifNotNil: [ next update ]
```

The very first layer uses the input vector to actually update its parameters:

```Smalltalk
NMLayer>>update: input
	"Update the weights and biases using the input value"
	w := w - ((delta +* input transposed) * lr / numberOfExamples).
	b := b - (delta sumHorizontal * lr / numberOfExamples).
	next update

```

Our definition of layer is now complete. We can now propose a definition of the class `NMNetwork`.

## Defining a matrix-based neural network

We will call `NMNetwork` the class describing a matrix-based neural network. Here is its definition:

```Smalltalk
Object subclass: #NMNetwork
	instanceVariableNames: 'random errors layers'
	classVariableNames: ''
	package: 'NeuralNetwork-Matrix'
```

The variables are similar than in our first version of the neural network. The variable `random` contains a random number generator, which is useful to initialize the layers. The `errors` variable contains the errors values during the training. The `layers` variable contains instances of `NMLayer`. 

The network is initialized with no layer and a random number generator:

```Smalltalk
NMNetwork>>initialize
	"Initialize the network with no layer and a proper random generator"
	super initialize.
	layers := OrderedCollection new.
	random := Random seed: 42.
```

When a layer is added to the network, a chain of layers has to be maintained:

```Smalltalk
NMNetwork>>addLayer: aLayer
	"Add a layer to the network. Note that layers form a bidirectional chain."
	layers ifNotEmpty: [
		layers last next: aLayer. 
		aLayer previous: layers last ].
	layers add: aLayer
```

A central method to the learning is `backwardX:y:`, which computes the error and backpropagates it along the layers:

```Smalltalk
NMNetwork>>backwardX: x y: y
	"Compute and backpropagate the error"
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

The cost function is computed for two given vectors:

```Smalltalk
NMNetwork>>computeCost: v1 and: v2
	"Compute the cost function for two provided vectors"
	^ ((v1 - v2) collect: [ :v | v * v ]) sum
```

The configuration of the network is performed through a number of utility methods. The following method configures a network with one hidden layer:

```Smalltalk
NMNetwork>>configure: nbOfInputs hidden: nbOfNeurons nbOfOutputs: nbOfOutputs
    "Configure the network with the given parameters
    The network has only one hidden layer"
	self addLayer: (NMLayer new nbInputs: nbOfInputs nbOutputs: nbOfNeurons random: random).
	self addLayer: (NMLayer new nbInputs: nbOfNeurons nbOutputs: nbOfOutputs random: random).
```

Similarly, two hidden layers may be configured using the following method:

```Smalltalk
NMNetwork>>configure: nbOfInputs hidden: nbOfNeurons1 hidden: nbOfNeurons2 nbOfOutputs: nbOfOutputs
    "Configure the network with the given parameters. The network has two hidden layers"
	self addLayer: (NMLayer new nbInputs: nbOfInputs nbOutputs: nbOfNeurons1 random: random).
	self addLayer: (NMLayer new nbInputs: nbOfNeurons1 nbOutputs: nbOfNeurons2 random: random).
	self addLayer: (NMLayer new nbInputs: nbOfNeurons2 nbOutputs: nbOfOutputs random: random).
```

The forward feeding is simply done using the method `feed:`:

```Smalltalk
NMNetwork>>feed: inputs
	"Feed the network with the provided inputs vector
	Return the output value as a matrix"
	| mat |
	mat := inputs.
	layers do: [ :l | mat := l feed: mat ].
	^ mat
```

The learning rate of the network is defined using a dedicated method:

```Smalltalk
NMNetwork>>lr: aLearningRateAsFloat
	"Globally set the learning rate"
	layers do: [ :l | l lr: aLearningRateAsFloat ]
```

The training is performed using the following method:

```Smalltalk
NMNetwork>>trainX: x y: y nbOfEpochs: nbEpochs
	"Train the network with a set of inputs against the expected values"
	| cost output |
	"We need to tell to each layer the number of examples they have"
	layers do: [ :l | l numberOfExamples: y nbColumns ].
	errors := OrderedCollection new.
	nbEpochs timesRepeat: [ 
		output := self feed: x.
		cost := self computeCost: output and: y.
		self backwardX: x  y: y.
		self update: x.
		errors add: cost.
	].
	^ cost
```

The update of the weights and biases is done using the method:

```Smalltalk
NMNetwork>>update: input
	"Update the weights and bias using the provided input vector"
	layers first update: input
```

Note that the layer performs the job of updating its parameters. Prediction can be achieved by simply copying the `predict:` method from our original implementation:

```Smalltalk
NMNetwork>>predict: inputs
	"Make a prediction. This method assume that the number of outputs is the same than the number of different values the network can output"
	"The index of a collection begins at 1 in Pharo,
	which is why we need to substrate 1"
	| outputs |
	outputs := self feed: inputs.
	^ (outputs asArray indexOf: (outputs max)) - 1
```

We define the `train:nbEpochs:` method, useful to train a model using a labeled dataset:


```Smalltalk
NMNetwork>>train: data nbEpochs: nbEpochs
	"Data is provided as a collection of arrays.
	The example data need to be labeled using a numerical value"
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
	^ self trainX: x y: y nbOfEpochs: nbEpochs

```

At that stage, we have a matrix-based network which is able to learn from a labeled dataset. Consider the following example:

```Smalltalk
xor := #(#(0 0 0)
			#(0 1 1)
			#(1 0 1)
			#(1 1 0)).
n := NMNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2. 
n train: xor nbEpochs: 5000. 
n predict: (MMatrix newFromVector: #(1 0)).
"=> 1"

n predict: (MMatrix newFromVector: #(1 1)).
"=> 0"
```

The following section presents a simple way to draw the error function.

## Visualization of the results

We will extend the class `NMNetwork` to visualize the evolution of the error along the epochs. Simply the define the method:

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
	b axisX noDecimal; title: 'Epoch'.
	b axisY title: 'Error'.
	^ b
```

The hook into the GTInspector framework is simply done using the following method:

```Smalltalk
NMNetwork>>viewLearningCurveIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: 'Cost';
		initializeView: [ self viewLearningCurve ]
```

![Visualizing the learning.](08-MatrixNN/figures/learningMatrixBased.png){#fig:learningMatrixBased}

Evaluating the training instruction in the previous section should output the error curve showing in Figure @fig:learningMatrixBased.

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
n configure: 4 hidden: 6 nbOfOutputs: 3. 
n train: irisData nbEpochs: 3000. 
n
```

The result is the same than we we previously seen.

## What we have seen

This chapter revises our previous implementation of neural network. Our revised implementation employs matrices to model the state of the network, which greatly simplify the implementation of it. However, it raises the level of abstractness since matrices are not at the core. The chapters explores:

- The use of matrices to implement forward and backward propagation. It uses the matrix library presented in the previous chapter
- It revisits the Iris classification example to illustrate the new neural network classes.
