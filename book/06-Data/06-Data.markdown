
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

After evaluating this script, the expression `n feed: {1 . 0}` evaluates to `#(0.9735546630024936)`, an array having an expected float value close to 1. The example is actually very verbose. 

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
			 
			t := (1 to: expectedOutput size) 
					collect: [ :i | ((expectedOutput at: i) - (outputs at: i)) raisedTo: 2 ].
			sumError := sumError + t sum.
			self backwardPropagateError: expectedOutput.
			self updateWeight: row allButLast.
		].
		errors add: sumError
	] 
```

The method makes the network training significantly less verbose.

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {
	#(0 0 0) .
	#(0 1 1) .
	#(1 0 1) .
	#(1 1 0) }.
n train: data nbEpoch: 10000
```

The `data` variable is an array of array of numbers. Each row represents an example and it contains the input values and the output value. For example, the row `#(0 1 1)` represents the line `n train: { 0 . 1 } desiredOutputs: { 1 }` given above. Note that we have two outputs, and not one. We use a one-hot encoding for the output, as explained later on in this chapter.

Predicting the output for a given set of input values could be defined using:

```Smalltalk
NNetwork>>predict: inputs
	"Make a prediction. This method assume that the number of outputs is the same than the number of different values the network can output"
	"The index of a collection begins at 1 in Pharo"
	| outputs |
	outputs := self feed: inputs.
	^ (outputs indexOf: (outputs max)) - 1
```

Another example of using the syntax we have just introduced:

```Smalltalk
n := NNetwork new.
n configure: 3 hidden: 8 nbOfOutputs: 8.

data := {
	{0 . 0 . 0 . 0}.
	{0 . 0 . 1 . 1}.
	{0 . 1 . 0 . 2}.
	{0 . 1 . 1 . 3}.
	{1 . 0 . 0 . 4}.
	{1 . 0 . 1 . 5}.
	{1 . 1 . 0 . 6}.
	{1 . 1 . 1 . 7} }.
n train: data nbEpoch: 1000.
```

The code above builds a neural network trained to convert binary numbers into a decimal number. As an example, you can evaluate the following:

```Smalltalk
n predict: {0 . 1 . 1}
"==> 3"
```

The way `train:nbEpoch:` and `predict:` are implemented enforces the training data to follow some rules. Each element contained in `data` must be a collection of numbers. All but the last numbers represents the inputs values. The last value of an example is a number representing the expected output. The expected output is a positive value ranging from 0 and the number of output of the neural network.


## Neural network as a Hashmap

Let's step back a bit. We have spent more than five chapters motivating, describing, incrementally building neural networks. But we are using a neural network pretty much the way we would use a regular hash map. Consider the following example:

```Smalltalk
data := {
	{0 . 0 . 0 . 0}.
	{0 . 0 . 1 . 1}.
	{0 . 1 . 0 . 2}.
	{0 . 1 . 1 . 3}.
	{1 . 0 . 0 . 4}.
	{1 . 0 . 1 . 5}.
	{1 . 1 . 0 . 6}.
	{1 . 1 . 1 . 7} }.
	
d := Dictionary new.
data do: [ :anExample |
	d at: anExample allButLast put: anExample last ].
d at: #(1 0 1)
```

The variable `d` is a dictionary filled with the example data. The values we used as input in the neural network are used as keys in the dictionary. Indeed, using a dictionary has many benefits here: filling a dictionary is significantly faster than training a neural network (with several order of magnitude), and getting a value for a particular key is also significantly faster then feed forwarding a network.

However, a hash map requires the exact same key (or at least adequately answers to the message `=`). A neural network does not requires the exact same input values. Consider the following expression:

```Smalltalk
n predict: {0.4 . 0.7 . 0.6}
"==> 3"
```

The network somehow matches the input values `{0.4 . 0.7 . 0.6}` to `{0 . 1 . 1}`, which returns the value `3`. 

## Visualizing the error and the topology

We have seen that the first step of the backpropagation is to actually evaluate the network with the provided inputs. The output values are then compared with the expected output values. The difference between the actual output and the expected output is then used to adjust the weights and biases by back-propagating this difference to the network. 

The method `NNetwork>>train:nbEpoch:` contains the statement `errors add: sumError`. This line of code has the effect to record the value of the `sumError`, indicating how well the network has performed for the provided example. This list of errors can be visualized.

We define the method `viewErrorCurve` on the class `NNetwork`:

```Smalltalk
NNetwork>>viewErrorCurve
	| b ds |
	errors ifEmpty: [ 
		^ RTView new 
			add: (RTLabel elementOn: 'Should first run the network'); 
			yourself ].
	
	b := RTGrapher new.
	
	"We define the size of the charting area"
	b extent: 500 @ 300.
	
	ds := RTData new.
	ds noDot. 
	ds connectColor: Color blue.
	ds points: errors.
	ds dotShape rectangle color: Color blue.
	b add: ds.
	
	b axisX noDecimal; title: 'Epoch'.
	b axisY title: 'Error'.
	^ b
```

The following method makes the visualization of the `errors` variable always shown:
```Smalltalk
NNetwork>>viewErrorCurveIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: 'Error';
		initializeView: [
			self viewErrorCurve ]
```

The method `NNetwork>>viewErrorCurveIn:` uses the GTInspector framework to add particularized tab in the inspector.

Inspecting the following code snippet displays the error curve (Figure @fig:errorCurve):

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {
    #(0 0 0) .
    #(0 1 1) .
    #(1 0 1) .
    #(1 1 0) }.
n train: data nbEpoch: 10000.
```

![Representing the perceptron.](06-Data/figures/errorCurve.png){#fig:errorCurve width=400px}

The error curve indicates the effect of the number of epochs on making the neural network learn. Being that close to 0 is a strong indicator that the neural network is properly learning. 


Similarly, we can visualize the topology of the network using the following method:

```Smalltalk
NNetwork>>viewNetwork
	| b lb |
	b := RTMondrian new.
	
	b nodes: layers forEach: [ :aLayer |
		b shape circle size: 20.
		b nodes: aLayer neurons.
		b layout verticalLine.
	].

	b shape arrowedLine; withShorterDistanceAttachPoint.
	b edges connectTo: #nextLayer.
	b layout horizontalLine gapSize: 30; center.
	
	b build.
	
	lb := RTLegendBuilder new.
	lb view: b view.
	lb addText: self numberOfNeurons asString, ' neurons'.
	lb addText: self numberOfInputs asString, ' inputs'.
	lb build.
	^ b view
```

Similarly, we need to extend GTInspector to consider the visualization within GTInspector (Figure @fig:networkTopology):

```Smalltalk
NNetwork>>viewNetworkIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: 'network';
		initializeView: [
			self viewNetwork ]
```

![Representing the perceptron.](06-Data/figures/networkTopology.png){#fig:networkTopology width=400px}


## Classifying data & one hot encoding

The two examples given below may be considered as two classification models, based on neural networks. 

Classification can be defined as grouping elements based on their features. Elements shared the similar features are grouped together. 



One hot encoding is a simple mechanism that convert a categorical variable into a numerical form, eligible to be fed into a neural network. 


## Iris Dataset

The Iris flower dataset is a popular dataset used by the machine learning community (http://archive.ics.uci.edu/ml/datasets/Iris). This dataset was collected in 1936 by Ronald Fisher and presented in the seminal paper _The use of multiple measurements in taxonomic problems_.

The data set contains 50 samples of three families of Iris, called _Iris setosa_, _Iris virginica_ and _Iris versicolor_. We refer to these families as _classes_.

We provide a copy of this dataset on https://agileartificialintelligence.github.io/Datasets/iris.csv. Within Pharo, you can fetch the dataset using the expression:

```Smalltalk
(ZnEasy get: 'https://agileartificialintelligence.github.io/Datasets/iris.csv') contents.
```

## Normalization

Before 
