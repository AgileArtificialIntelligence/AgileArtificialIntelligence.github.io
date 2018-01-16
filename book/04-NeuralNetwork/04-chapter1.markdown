
# Neural Networks

In the previous chapter we have seen how to define single artificial neuron. This chapter belongs to the core of this book as it presents how to connect and give a meaning to a group of artificial neurons.

## General architecture

An artificial neural network is a computing system inspired by biological neural networks which define animal brains. 
An artificial neural network is a collection of connected artificial neurons. Each connection between artificial neurons can transmit a signal from one to another. The artificial neuron that receives the signal can process it, and then signal neurons connected to it. 

Artificial neural networks are commonly employed to perform particular tasks, including clustering, classification, prediction, and pattern recognition. 

A neural network acquires knowledge through learning.


[generalStructure]: 04-NeuralNetwork/figures/generalStructure.png "Image Title" {width=400}
![generalStructure][generalStructure] 

The figure above shows a network made of five neurons, three inputs, and two outputs. The left-most column is called the inputs. The networks contains three inputs, `x1`, `x2`, and `x3`. the middle of the network contains the hidden layers. The network above contains only one hidden layer, made of three neurons. The right-most part of the network is called output layer, and is made of two neurons. 

We therefore needs three different components to model networks:
- _Hidden layer_ representing layers within 
- _Output layer_ representing the right-most column. This is that column that spits out computed values
- _Neural Network_ made of several hidden layers and one output layer.

## Neural layer

We define a layer as a set of neurons. Each layer knows about the preceding layer using the variable `previousLayer` and the following layer using `nextLayer`. The `learningRate` variable refers to the learning rate of the layer. We define the class `NeuronLayer` as follows:

```Smalltalk
Object subclass: #NeuronLayer
	instanceVariableNames: 'previousLayer nextLayer neurons learningRate'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We can set the learning rate of a layer as 0.5 per default. A neuron layer may be initialized using the following method:

```Smalltalk
NeuronLayer>>initializeNbOfNeurons: nbOfNeurons nbOfWeights: nbOfWeights using: random
	"Main method to initialize a neuron layer
	nbOfNeurons : number of neurons the layer should be made of
	nbOfWeights : number of weights each neuron should have
	random : a random number generator
	"
	| weights |
	learningRate := 0.5.
	neurons := (1 to: nbOfNeurons) collect: [ :i |
		weights := (1 to: nbOfWeights) collect: [ :ii | random next * 2 - 1 ].
		Neuron new sigmoid; weights: weights; bias: (random next * 2 - 1) ]  

```

The method `initializeNbOfNeurons:nbOfWeights:using:` accepts three arguments. The first one, `nbOfNeurons` is an integer number representing the number of neurons the layer should contains. The second argument, `nbOfWeights`, is an integer that indicates the number of weights each neuron should have. This number of weights reflects the number of input values the layer is accepting. The last argument, `random`, is a random number generator. 

The method set the learning rate to `0.5`, and create `nbOfNeurons` neurons, each having `nbOfWeights` weight value. Each weight is a random number between -1 and +1. Each neuron has a sigmoid activation function.

Forward feeding the layer is an essential operation. It consists in feeding each neurons and forwarding the values to the next value. We define the method `feed:` as:

```Smalltalk
NeuronLayer>>feed: someInputValues
	"Feed the neuron layer with some inputs"

	| someOutputs |
	someOutputs := neurons collect: [ :n | n feed: someInputValues ] as: Array.
	^ self isOutputLayer
		ifTrue: [ someOutputs ]
		ifFalse: [ nextLayer feed: someOutputs ]
```

The method invokes `feed:` on each of its neurons. The results is then kept as an Array. The method then check if the layer is an output layer. If this is the case, the result of the method is simply the results of each neurons. If the layer is not an output (_i.e.,_ it is  an hidden layer), we forward feed the next layer.


We now need the following utility method:
```Smalltalk
NeuronLayer>>isOutputLayer
	"Return true of the layer is the output layer (i.e., the last layer in the network)"
	^ self nextLayer isNil
```

We can now prepare a testbed for our new neuron layer class:
```Smalltalk
TestCase subclass: #NeuronLayerTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

A simple test may be:

```Smalltalk
NeuronLayerTest>>testBasic
	| nl result |
	nl := NeuronLayer new.
	nl initializeNbOfNeurons: 3 nbOfWeights: 4 using: (Random seed: 42).

	self assert: nl isOutputLayer.

	result := nl feed: { 1 . 2 . 3 . 4 }.
	self assert: result size equals: 3.
 	self assert: result closeTo: #(0 1.1277714465408457 1.9863987178478386).
```

## Neural network

We will represent a neural network as an instance of the class `NNetwork`:

```Smalltalk
Object subclass: #NNetwork
	instanceVariableNames: 'layers errors'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We define a neural network simply as a container of layers. We also add an `errors` instance variable that will be useful to trace the error during a learning phase.

The initialization of a network is done through the method `initialize`:
```Smalltalk
NNetwork>>initialize
	super initialize.
	layers := OrderedCollection new.
	errors := OrderedCollection new
```

Both the `layers` and `errors` instance variables are initialized with an empty collection. The variable `layers` will contains instance of the class `NeuronLayer` and `errors` will contains numerical values, representing the errors during the training process. 

Adding a layer is simply done through the method `addLayer:`, which takes a layer as argument:
```Smalltalk
NNetwork>>addLayer: aNeuronLayer
	"Add a neural layer. The added layer is linked to the already added layers."
	layers ifNotEmpty: [ 
		aNeuronLayer previousLayer: layers last.
		layers last nextLayer: aNeuronLayer ].
	layers add: aNeuronLayer.
```

Layers are linked to each other. When a layer is added, it is linked to the previous layer and that layer is linked to the added layer.


```Smalltalk
NNetwork>>feed: someInputValues
	"Feed the first layer with the provided inputs"
	^ self firstLayer feed: someInputValues
```

We add the necessary to easily create a neural network. In case we wish to build a network with one hidden layer and one output layer:

```Smalltalk
NNetwork>>configure: nbOfInputs hidden: nbOfNeurons nbOfOutputs: nbOfOutput
	"Configure the network with the given parameters
	The network has only one hidden layer"
	| random |
	random := Random seed: 1.
	self addLayer: (NeuronLayer new initializeNbOfNeurons: nbOfNeurons nbOfWeights: nbOfInputs using: random).
	self addLayer: (NeuronLayer new initializeNbOfNeurons: nbOfOutput nbOfWeights: nbOfNeurons using: random).
```

In case we wish to have two hidden layers and one output layer:
```Smalltalk
NNetwork>>configure: nbOfInputs hidden: nbOfNeurons1 hidden: nbOfNeurons2 nbOfOutputs: nbOfOutput
	"Configure the network with the given parameters
	The network has only one hidden layer"
	| random |
	random := Random seed: 42.
	self addLayer: (NeuronLayer new initializeNbOfNeurons: nbOfNeurons1 nbOfWeights: nbOfInputs using: random).
	self addLayer: (NeuronLayer new initializeNbOfNeurons: nbOfNeurons2 nbOfWeights: nbOfNeurons1 using: random).
	self addLayer: (NeuronLayer new initializeNbOfNeurons: nbOfOutput nbOfWeights: nbOfNeurons2 using: random).
```


We can now tests our network implementation:

```Smalltalk
TestCase subclass: #NNetworkTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```


```Smalltalk
NNetworkTest>>testBasic
	| n |
	n := NNetwork new.
	n configure: 2 hidden: 2 nbOfOutputs: 1.
	self assert: (n feed: { 1 . 3 }) closeTo: { 0.35859281167322443 } 
```

As you can see, `testBasic` is rather simplistic. It builds a simple network with two inputs, one hidden layer made of 2 neurons, and an output layer with only one neuron, and run the forward feeding. 

## Backpropagation

Backpropagation is an algorithm commonly employed to train neural network. In this section we will focus on implementing the algorithm. The training process is composed of three steps:
1.Forward feeding the inputs
1.Backward propagating the errors through the network
1.Updating the neurons weights and biases

The first phase is mostly implemented by the method `NNetwork>>feed:`, however, we need to improve our neuron to keep the output. During the forward feeding (_i.e.,_ when the method `feed:` is called), an output is produced by each neuron. This output has to be compared with an expected output. Making the network learn is based on the difference between the actual output of a neuron and the expected output. Each neuron has therefore to keep a reference to the output. 

We add two variables, `delta` and `output`, in the class `Neuron`, 

```Smalltalk
Object subclass: #Neuron
	instanceVariableNames: 'weights bias delta output activationFunction'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We now rewrite the method `feed:` on the class `Neuron`:
```Smalltalk
Neuron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
	output := activationFunction eval: z.
	^ output    
```

At that stage, it is important to run the unit tests we have run. We are now done with the first phase of the backpropagation. 

The second phase consists in propagating the errors computed at the output layer back in the network. We define the following method:

```Smalltalk
NNetwork>>backwardPropagateError: expected
	self outputLayer backwardPropagateError: expected
```

And the following helper method:
```Smalltalk
NNetwork>>outputLayer
	"Return the output layer, which is also the last layer"
	^ layers last
```


```Smalltalk
NeuronLayer>>backwardPropagateError: expected
	"This is a recursive method. The back propagation begins with the output layer (i.e., the last later)"
	"We are in the output layer"
	neurons with: expected do: [ :neuron :exp | 
		| theError |
		theError := exp - neuron output.
		neuron adjustDeltaWith: theError ].

	"We iterate"
	self previousLayer notNil
		ifTrue: [
			 self previousLayer backwardPropagateError ].
```

```Smalltalk
NeuronLayer>>backwardPropagateError
	"This is a recursive method. The back propagation begins with the output layer (i.e., the last later)"

	"We are in an hidden layer"
	neurons doWithIndex: [ :neuron :j |
		| theError |
		theError := 0.0.
		self nextLayer neurons do: [ :nextNeuron |
			theError := theError + ((nextNeuron weights at: j) * nextNeuron delta)
		].
		neuron adjustDeltaWith: theError
	].

	self previousLayer notNil
		ifTrue: [
			 self previousLayer backwardPropagateError ].
```

We also need the following helper method:
```Smalltalk
Neuron>>adjustDeltaWith: anError
	delta := anError * (activationFunction derivative: output)
```

The recursion happens in this method:

```Smalltalk
NeuronLayer>>backwardPropagateError
	"This is a recursive method. The back propagation begins with the output layer"

	"We are in an hidden layer"
	neurons doWithIndex: [ :neuron :j |
		| theError |
		theError := 0.0.
		self nextLayer neurons do: [ :nextNeuron |
			theError := theError + ((nextNeuron weights at: j) * nextNeuron delta)
		].
		neuron adjustDeltaWith: theError
	].

	previousLayer notNil
		ifTrue: [
			 previousLayer backwardPropagateError ].
```

The third phase is rather simple:

```Smalltalk
NNetwork>>updateWeight: initialInputs
	"Update the weights of the neurons using the initial inputs"
	layers first updateWeight: initialInputs
```

```Smalltalk
NeuronLayer>>updateWeight: initialInputs
	"Update the weights of the neuron based on the set of initial input. This method assumes that the receiver of the message invoking that method is the first hidden layer."
	| inputs |
	inputs := initialInputs.
		
	neurons do: [ :n |
		n adjustWeightWithInput: inputs learningRate: learningRate.
		n adjustBiasUsingLearningRate: learningRate ].
	
	self nextLayer ifNotNil: [ 
		self nextLayer updateWeight ]
```

And we need the following methods to update a neuron state:
```Smalltalk
Neuron>>adjustWeightWithInput: inputs learningRate: learningRate
	inputs withIndexDo: [ :anInput :index | 
		weights at: index put: ((weights at: index) + (learningRate * delta * anInput)) ]
```

```Smalltalk
Neuron>>adjustBiasUsingLearningRate: learningRate
	bias := bias + (learningRate * delta)
```

We are now ready to hook the backpropagation phases all together:

```Smalltalk
NNetwork>>train: someInputs desiredOutputs: desiredOutputs
	"Train the neural network with a set of inputs and some expected output"
	| realOutputs t |
	realOutputs := self feed: someInputs.
	t := (1 to: desiredOutputs size) collect: 
			[ :i | ((desiredOutputs at: i) - (realOutputs at: i)) raisedTo: 2 ].
	self backwardPropagateError: desiredOutputs.
	self updateWeight: someInputs.
```

We can now test our network with the XOR example:
```Smalltalk
NNetworkTest>>testXOR
	| n |
	n := NNetwork new.
	n configure: 2 hidden: 3 nbOfOutputs: 1.

	10000 timesRepeat: [ 
		n train: { 0 . 0 } desiredOutputs: { 0 }.	
		n train: { 0 . 1 } desiredOutputs: { 1 }.
		n train: { 1 . 0 } desiredOutputs: { 1 }.
		n train: { 1 . 1 } desiredOutputs: { 0 }.
	].

	self assert: ((n feed: { 1 . 1 }) first - 0.025) < 0.01.
	self assert: ((n feed: { 0 . 1 }) first - 1) < 0.01.
	self assert: ((n feed: { 1 . 0 }) first - 1) < 0.01.
	self assert: ((n feed: { 0 . 0 }) first - 0.025) < 0.01.
```

If you try to decrease the `10000` to a low value, `10` for example, the network does not receive enough training and the test ultimately fails.



## Cost function

A cost function is a measure of how well a neural networks leans. A cost function is a single value since it rates how good the neural network did as a whole.