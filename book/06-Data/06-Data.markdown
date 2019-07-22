
# Data Classification

Neural networks have an incredible large range of applications. Classifying data is a prominent one, and this chapter is devoted to it.

## Easily train a network

In the previous chapter, we have seen that we can obtain a trained neural network to express the XOR logical gate. In particular, we have seen the following script:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 1.

20000 timesRepeat: [ 
	n train: #(0 0) desiredOutputs: #(0).	
	n train: #(0 1) desiredOutputs: #(1).
	n train: #(1 0) desiredOutputs: #(1).
	n train: #(1 1) desiredOutputs: #(0).
].
```

After evaluating this script, the expression `n feed: #(1 0)` evaluates to `#(0.9530556769505442)`, an array having an expected float value close to 1. If we step back a bit, we see that the script is actually very verbose. For example, why should we manually handle the repetition? Why having the message `train:desiredOutputs:` sent so many times? We can greatly simplify the way network are trained by providing a bit of infrastructure.

Consider the following method:

```Smalltalk
NNetwork>>train: train nbEpochs: nbEpochs
    "Train the network using the train data set."
    | sumError outputs expectedOutput epochPrecision t |
    1 to: nbEpochs do: [ :epoch |
        sumError := 0.
		  epochPrecision := 0.
        train do: [ :row |
            outputs := self feed: row allButLast.
            expectedOutput := (1 to: self numberOfOutputs) collect: [ :notUsed | 0 ].
            expectedOutput at: (row last) + 1 put: 1.
            (row last = (self predict: row allButLast)) ifTrue: [ epochPrecision := epochPrecision + 1 ].
            t := (1 to: expectedOutput size) 
                    collect: [ :i | ((expectedOutput at: i) - (outputs at: i)) squared ].
            sumError := sumError + t sum.
            self backwardPropagateError: expectedOutput.
            self updateWeight: row allButLast.
        ].
        errors add: sumError.
		  precisions add: (epochPrecision / train size) asFloat.
    ] 
```

Predicting the output for a given set of input values may be implemented using a `predict:` method:

```Smalltalk
NNetwork>>predict: inputs
	"Make a prediction. This method assumes that the number of outputs is the same as the number of different values the network can output"
	"The index of a collection begins at 1 in Pharo"
	| outputs |
	outputs := self feed: inputs.
	^ (outputs indexOf: (outputs max)) - 1
```


These two methods make the network training significantly less verbose. The script that trains a network with XOR logical gate may now be written:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {#(0 0 0) .
	#(0 1 1) .
	#(1 0 1) .
	#(1 1 0) }.
n train: data nbEpochs: 20000
```

The `data` variable is an array of arrays of numbers. Each row represents an example and it contains the input values and the output value. For example, the row `#(0 1 1)` represents the line `n train: #(0 1) desiredOutputs: #(1)` given above. Note that the neural network has two output neurons. This is the result of using a one-hot encoding for the output. The examples have two different output values, either `0` or `1`, therefore using the one-hot encoding we have two output neurons, each neuron for a particular value. Later on in this chapter we will detail this encoding.

Another example of using the syntax we have just introduced:

```Smalltalk
n := NNetwork new.
n configure: 3 hidden: 8 nbOfOutputs: 8.

data := {#(0 0 0 0).
	#(0 0 1 1).
	#(0 1 0 2).
	#(0 1 1 3).
	#(1 0 0 4).
	#(1 0 1 5).
	#(1 1 0 6).
	#(1 1 1 7) }.
n train: data nbEpochs: 1000.
```

The code above builds a neural network trained to convert binary numbers into a decimal number. The binary number is encoded using 3 bits, we therefore need a neural network with 3 inputs. Since the decimal value ranges from 0 to 7, we need 8 output neurons of the network. As an example, the conversion of the binary number you can evaluate the following:

```Smalltalk
...
n predict: #(0 1 1)
"==> 3"
```

The way `train:nbEpochs:` and `predict:` are implemented enforces the training data to follow some rules. Each element contained in `data` must be a collection of numbers. All but the last numbers represents the inputs values. The last value of an example is a number representing the expected output. The expected output is a positive value ranging from 0 and the number of outputs of the neural network minus one.


## Neural network as a Hashmap

Let's step back a bit. We have spent six chapters to motivate, describe, and incrementally build neural networks. But we are using a neural network pretty much the way we would use a regular hash map. Consider the following example:

```Smalltalk
data := {#(0 0 0 0).
	#(0 0 1 1).
	#(0 1 0 2).
	#(0 1 1 3).
	#(1 0 0 4).
	#(1 0 1 5).
	#(1 1 0 6).
	#(1 1 1 7) }.
	
d := Dictionary new.
data do: [ :anExample |
	d at: anExample allButLast put: anExample last ].
d at: #(0 1 1)
"==> 3"
```

The variable `d` is a dictionary filled with the example data. The values we used as input in the neural network are used as keys in the dictionary. Indeed, using a dictionary has many benefits here: filling a dictionary is significantly faster than training a neural network (by several order of magnitude!), and getting a value for a particular key is also significantly faster then feed forwarding a network.

However, a hash map requires the exact same key (or at least adequately answers to the message `=`). A neural network does not requires the exact same input values. Consider the following expression:

~~~~~~~~
n predict: #(0.4 0.7 0.6)
"==> 3"
~~~~~~~~

The network somehow matches the input values `#(0.4 0.7 0.6)` to `#(0 1 1)`, which returns the value `3`. A hashmap is not able to do such a connection without the programmer to explicitly doing so, and that is the whole point of neural networks: establishing connections between input data and identifying the most relevant data, without any intervention of the programmer.

## Visualizing the error and the topology

We have seen that the first step of the backpropagation is to actually evaluate the network with the provided inputs. The output values are then compared with the expected output values. The difference between the actual output and the expected output is then used to adjust the weights and biases by backpropagating this difference to the network. 

The method `NNetwork>>train:nbEpochs:` contains the statements `errors add: sumError` and `precisions add: (epochPrecision / train size) asFloat`. These two lines of code have the effect to record the value of `sumError`, indicating how well the network has performed for the provided example, and the value of precision per epoch. These two collections of numbers can be visualized as a helper to characterize the overall learning process for a given network and example set.

We define the method `viewLearningCurve` on the class `NNetwork`:

```Smalltalk
NNetwork>>viewLearningCurve
	"Draw the error and precision curve"
	| b ds |
	"No need to draw anything if the network has not been run"
	errors ifEmpty: [ ^ RTView new
				add: (RTLabel elementOn: 'Should first run the network');
				yourself ].

	b := RTDoubleGrapher new.

	"We define the size of the charting area"
	b extent: 500 @ 300.
	ds := RTData new.
	"A simple optimization that Roassal offers"
	ds samplingIfMoreThan: 2000.
	"No need of dots, simply a curve"
	ds noDot; connectColor: Color blue.
	ds points: (errors collectWithIndex: [ :y :i | i -> y ]).
	ds x: #key.
	ds y: #value.
	ds dotShape rectangle color: Color blue.
	b add: ds.
	ds := RTData new.
	ds samplingIfMoreThan: 2000.
	ds noDot.
	ds connectColor: Color red.
	ds points: (precisions collectWithIndex: [ :y :i | i -> y ]).
	ds x: #key.
	ds y: #value.
	ds dotShape rectangle color: Color blue.
	b addRight: ds.
	b axisX noDecimal; title: 'Epoch'.
	b axisY title: 'Error'.
	b axisYRight title: 'Precision'; color: Color red.
	^ b
```

The following method defines a visualization of the `errors` and `precisions` variables:

```Smalltalk
NNetwork>>viewLearningCurveIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: 'Learning';
		initializeView: [ self viewLearningCurve ]
```

The method `NNetwork>>viewLearningCurveIn:` uses the GTInspector framework to add particularized tab in the inspector.

Inspecting the following code snippet displays the error curve (Figure @fig:learningCurve):

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {#(0 0 0) .
    #(0 1 1) .
    #(1 0 1) .
    #(1 1 0) }.
n train: data nbEpochs: 10000.
```

![Visualizing the learning.](06-Data/figures/errorCurve.png){#fig:learningCurve}

The learning curves indicate the effect of the number of epochs on making the neural network learn. The fact that the blue lines is close to 0 is a strong indicator that the neural network is properly learning. And the fact that the red line reaches 1.0 means that the network is accurate.

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

We need to define the helper method:

```Smalltalk
NNetwork>>numberOfInputs
	"Return the number of inputs the network has"
    ^ layers first neurons size
```

and the method:

```Smalltalk
NNetwork>>numberOfNeurons
	"Return the total number of neurons the network has"
	^ (layers collect: #numberOfNeurons) sum
```

Similarly, we need to extend GTInspector to consider the visualization within GTInspector (Figure @fig:networkTopology):

```Smalltalk
NNetwork>>viewNetworkIn: composite
	<gtInspectorPresentationOrder: -5>
	composite roassal2
		title: 'Network';
		initializeView: [ self viewNetwork ]
```

![Visualizing the network topology.](06-Data/figures/networkTopology.png){#fig:networkTopology}

You can click on a neuron to reveals its weights and bias.

## Contradictory data

The blue error curve quantifies the error made by the network during the learning phase. It may happens that the error has some plateaux. In such a case, increasing the number of epochs may have the effect to lower the error curve. 

In some case, if the error curve cannot get close to 0 then it may indicate a contradiction in the data. Consider the following example:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {#(0 0 0) .
	#(0 0 1) }.
n train: data nbEpochs: 1000.
```

The script trains a neural network with two contradictory examples. The first example trains the network to output `0` with the inputs `0` and `0`. The second example trains the network to output `1` for the same input values. 

![Data contradiction.](06-Data/figures/contradictionInData.png){#fig:contradictionInData}

Figure @fig:contradictionInData illustrates the error and precision curves in presence of contradicting data. The script given above makes the neural network learn two different outputs for exactly the same input values. As a consequence, the network will have to make mistake during the learning phase. 

In a real and non-trivial dataset it is likely that this situation will happen. In case that the contradictory occurrences is low, then the network will consider this contradiction as pure noise and will has tendency to diminish it.

## Classifying data & one hot encoding

Classification can be defined as grouping elements based on their features. Elements sharing similar features are grouped together. The XOR dataset given above may be considered as a (simple) classification model, in which each group is made of two elements. The group 0 is made of the elements [0, 0] and [1, 1], while the group 1 is made of [0, 1] and [1, 0]. 

Have you noticed that when we introduced the `train:nbEpochs:` when we have to define a neural network with two outputs for the XOR dataset? The reason is that we encode the output value using the _one-hot encoding_. 

One hot encoding is a simple mechanism that converts a categorical variable into a numerical form, eligible to be fed into a neural network. Consider the variable $v$ which represents a word within the set { _"hello", "bonjour", "Buenos dias"_ }. Applying one-hot encoding would assign to each word a unique number. For example, _"hello"_ is associated to the index 0, _"bonjour"_ associated to index 1, and _"Buenos dias"_ to 2. The value of $v$ can then be encoded with 3 different bits, since the dataset has 3 different words. We can then encode the words:

- _"hello"_ = [1, 0, 0]
- _"bonjour"_ = [0, 1, 0] 
- _"Buenos dias"_ = [0, 0, 1]

If the variable $v$ has to be provided to a neural network, then 3 neurons can be used for that purpose. 

We have defined the XOR dataset as:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.

data := {#(0 0 0) .
	#(0 1 1) .
	#(1 0 1) .
	#(1 1 0) }.
n train: data nbEpochs: 10000
```

Since there are two different values of the datasets, 0 and 1, we have two output neurons: the value 0 is encoded [1, 0], and 1 is encoded [0, 1].

Now that we explained the one-hot encoding, we can proceed with a larger dataset.

## Iris Dataset

The Iris flower dataset is a popular dataset used by the machine learning community (http://archive.ics.uci.edu/ml/datasets/Iris). This dataset was collected in 1936 by Ronald Fisher and presented in the seminal paper _The use of multiple measurements in taxonomic problems_. The data set contains 50 samples of three families of Iris, called _Iris setosa_, _Iris virginica_ and _Iris versicolor_. We refer to these families as _classes_.

We provide a copy of this dataset on https://agileartificialintelligence.github.io/Datasets/iris.csv. Within Pharo, you can fetch the dataset using the expression:

```Smalltalk
(ZnEasy get: 'https://agileartificialintelligence.github.io/Datasets/iris.csv') contents.
```

The code above fetches the file `iris.csv` and returns its content. The file structure, as given in the CSV header is:
```
sepal_length,sepal_width,petal_length,petal_width,species
```

However, fetching the file is just the first small step toward making the file processable by a neural networks. For example, we need to convert each row of the file into a set of numerical values (remember that neural network can only accept numbers as input?). 

In order to feed a network with the iris data set, we need to perform the following steps:

1. Fetch the file from the internet;
2. Cut the file content, represented as a very long text, into textual lines;
3. Ignore the first line of the file, since it contains the CSV header, which is not relevant for the network;
4. Each row has 5 entries for which the first 4 ones are numerical values and the last one is the flower name. We need to cut into pieces a row, each substring piece is separated by a comma. The last column needs to be presented, which is processed in the next step;
5. We replace in the table each flower name by a numerical value, which could be 0, 1, or 2.

The following script exactly performs these five steps:

```Smalltalk
"The execution of this script initializes the variable irisData.
This variable is used in the subsequent scripts of this chapter"
irisCSV := (ZnEasy get: 'https://agileartificialintelligence.github.io/Datasets/iris.csv') contents.
lines := irisCSV lines. 
lines := lines allButFirst.
tLines := lines collect: [ :l | 
		| ss |
		ss := l substrings: ','.
		(ss allButLast collect: [ :w | w asNumber ]), (Array with: ss last) ].

irisData := tLines collect: [ :row | 
		| l |
		row last = 'setosa' ifTrue: [ l := #( 0 ) ].
		row last = 'versicolor' ifTrue: [ l := #( 1 ) ].
		row last = 'virginica' ifTrue: [ l := #( 2 ) ].
		row allButLast, l ].
	
irisData.
```

To summarize, the script converts a very long string similar to:

```
'sepal_length,sepal_width,petal_length,petal_width,species
5.1,3.5,1.4,0.2,setosa
4.9,3.0,1.4,0.2,setosa
4.7,3.2,1.3,0.2,setosa
...
'
```

into a collection of numbers:

```
#(#(5.1 3.5 1.4 0.2 0) #(4.9 3.0 1.4 0.2 0) #(4.7 3.2 1.3 0.2 0) ...
```


The result of the script is the value of the `irisData` variable. In the remaining of the chapter, when we will refer to the iris dataset, we actually mean the `irisData` value.

## Training a network with irisData

Training a network is actually easy since we carefully prepared the battlefield. The remaining of the chapter assumes that the variable `irisData` is defined as shown in the previous section. Consider the following code:

~~~~~~~
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n train: irisData nbEpochs: 1000.
~~~~~~~

The code above builds a network with 4 input values, one hidden layer with 6 neurons, and the output layer has 3 neurons. The number of inputs represents the size of a row in the iris dataset minus 1, the expected output value which is not part of the input. We pick an arbitrary 6 as the size of the hidden layer. A general thumb-rule for the hidden layer size, is to contain 50% more neurons than the number of inputs. We have three neurons in the output layers since there are three different families of Iris.

![Learning the Iris dataset.](06-Data/figures/networkOnIris.png){#fig:networkOnIris}

Figure @fig:networkOnIris represents the error curve of the network. The blue curve is very close to 0, which indicates that the network is learning and the dataset does not have a contradiction. The red curve is very close to 1.0, which means that the network has an excellent precision. The network is able to learn and achieve a good precision during that learning process.

The configuration of our network has two parameters: the number of neurons in the hidden layers, and the number of epochs to consider. There are no general rules on how to pick these parameters. For now, experiments and ad-hoc tries remain the easiest approach to configure a network. The third part of the book, about neuroevolution, will cover the search of hyperparameters using genetic algorithm

## Effect of the learning curve

When we defined the `Neuron` class, in Chapter 2, we defined the method `learningRate:` to set the learning rate of the neuron. In general, for a single neuron, higher the learning rate, quicker it will be to learn. This effect can be easily illustrated. Consider the following example (Figure @fig:learningRateSingleNeuron):

```Smalltalk
g := RTGrapher new.
#(0.001 0.01 0.1 0.2 0.3)
	doWithIndex: [ :lr :index | 
		learningCurveNeuron := OrderedCollection new.
		0 to: 1000 do: [ :nbOfTrained | 
			r := Random new seed: 42.
			p := Neuron new.
			p weights: #(-1 -1).
			p bias: 2.
			p learningRate: lr.
			nbOfTrained
				timesRepeat: [ p train: #(0 0) desiredOutput: 0.
					p train: #(0 1) desiredOutput: 0.
					p train: #(1 0) desiredOutput: 0.
					p train: #(1 1) desiredOutput: 1 ].
			res := ((p feed: #(0 0)) - 0) abs + ((p feed: #(0 1)) - 0) abs
				+ ((p feed: #(1 0)) - 0) abs + ((p feed: #(1 1)) - 1) abs.
			learningCurveNeuron add: res / 4 ].
		d := RTData new.
		d label: 'Sigmoid neuron lr = ' , lr asString.
		d noDot.
		d connectColor: (RTPalette c1 at: index).
		d points: learningCurveNeuron.
		d y: #yourself.
		g add: d ].
g legend addText: 'Learning rate effect'.
g
```

![Effect of the learning rate for a single neuron.](06-Data/figures/learningRateSingleNeuron.png){#fig:learningRateSingleNeuron}

Figure @fig:learningRateSingleNeuron represents the error curves during the training for five different values of the learning rate (0.001, 0.01, 0.1, 0.2, and 0.3). The graphs indicates that the higher the learning rate, the quicker it learns. 

The effect observed on a single sigmoid neuron _cannot_ be observed on a whole network. We can train a network for the Iris dataset for different values of the learning rate. Consider the script:

~~~~~~~~
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n learningRate: 0.3. " Repeat the script with a different value"
n train: irisData nbEpochs: 1000.
~~~~~~~~

We run the script for each of the value 0.001, 0.01, 0.1, and 0.3. Results are presented in Figure @fig:learningRateNetwork.

![Effect of the learning rate for neural network on the Iris dataset.](06-Data/figures/learningRateNetwork.png){#fig:learningRateNetwork}

We clearly see that for a low learning rate, the precision and error curves are rather stable. While for a relatively high learning rate, we experience very frequent peaks. 

Unfortunately, there is no general methodology to identify the adequate learning rate or the architecture of the network. Manual tuning is the norm so far. Some optimization algorithms, _e.g.,_ the Adam optimization algorithm, variates the learning rate. During the training the learning rate varies.  

## Test and Validation

So far, we built a network trained on the whole iris dataset: we consider all the entries in the `.csv` file to train the network. The network seems to properly learn as the network makes fewer errors while increasing the precision along the epochs (_i.e.,_ the error curve is getting very close to 0). 

The error curve indicates how well the network is learning for the provided dataset. If we wish to know how well the network classifies data, it would not make much sense to test it on data it was trained with. Asking a network how well it performs in presence of the very same data used for the training is not much of a challenge. However, an important question is how well does the network behave in presence of data that it has never seen. Said in other word: _how well the network classifies unknown data?_

One way to answer this question, is to divide the iris dataset in two distinct parts: 

- _Training dataset_: a portion of the `.csv` file used to train the network.
- _Test dataset_: a second portion used to see how effective the trained network is.

Consider the following script:

~~~~~~~
cut := 0.8.
cutTraining := (irisData size * cut) rounded.
cutTest := (irisData size * (1 - cut)) rounded.
trainingData := irisData first: cutTraining.
testData := irisData last: cutTest.
~~~~~~~

The variable `cut` represents the portion of the original iris dataset used for the training: 80% of `irisData` is used for training. The variable `cutTraining` represents the number of `irisData` elements used for the training. Similarly, `cutTest` represents the number of elements for the test. The message `rounded`, when sent to a float value, returns the integer nearest to the float value (_e.g.,_ `4.6 rounded` returns `5`, `4.3 rounded` returns 4, and `4.5 rounded` returns `5`).

We can train a network based on the `trainingData`:

~~~~~~~
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n train: trainingData nbEpochs: 1000.
~~~~~~~

We see that the network is able to properly learn `trainingData`, as the error curve is close to 0, similarly than in Figure @fig:networkOnIris.

Consider the script (it assumes the existence of the previously seen variable `irisData`):

~~~~~~~
cut := 0.8.
cutTraining := (irisData size * cut) rounded.
cutTest := (irisData size * (1 - cut)) rounded.
trainingData := irisData first: cutTraining.
testData := irisData last: cutTest.
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n train: trainingData nbEpochs: 1000.

(((testData collect: [ :d |
	(n predict: d allButLast) = d last
]) select: #yourself) size / testData size) asFloat round: 2 
~~~~~~~

Evaluating the script returns 0.9, which represents the accuracy of our network: 90% of the elements contained in `testData` are correctly predicted. 

We will now detail the last part of the script:

~~~~~~~
(((testData collect: [ :d |
	(n predict: d allButLast) = d last
]) select: #yourself) size / testData size) asFloat round: 2 
~~~~~~~

For all the elements of `testData`, we predict the classification of the input (`d allButLast`) and compare the network result with the expected result (`d last`). The result of the `collect:` instruction is a list of binary values (`true` or `false`). We only select the `true` values (`select: #yourself`), count how many they are (`size`). We then compute the ratio with the size of test data (`/ testData size`). Finally, we only consider a float value with two decimal digits.

*EXERCISE:* Determine the accuracy of the network when a cut of 0.6, 0.5, and 0.4.

Consider a cut of 0.7, as illustrated in the script:

~~~~~~~
cut := 0.7.
cutTraining := (irisData size * cut) rounded.
cutTest := (irisData size * (1 - cut)) rounded.
trainingData := irisData first: cutTraining.
testData := irisData last: cutTest.
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n train: trainingData nbEpochs: 1000.

(((testData collect: [ :d |
	(n predict: d allButLast) = d last
]) select: #yourself) size / testData size) asFloat round: 2 
~~~~~~~

The result is 0.0, indicating that the network is not able to make any prediction. Why so? Reducing the size of the training data, for example, if cut equals to 0.5, increases the accuracy of the network. This is an effect due to the data organization. 

If we inspect the 150 values of `irisData`, we see that they are actually ordered: the first 50 entries are Iris setosa (the expected value is 0), the subsequent 50 entries are Iris versicolor (the expected value is 1), and the last 50 entries are Iris virginica (the expected value is 2). The fact that the original dataset is ordered has an impact on the accuracy of the network. Luckily, this is a problem that is easy to solve: a simple shuffling of the original data will prevent our network to suffer from the entry order.

Consider this new script: 

~~~~~~~
shuffledIrisData := irisData shuffleBy: (Random seed: 42).
cut := 0.8.
cutTraining := (shuffledIrisData size * cut) rounded.
cutTest := (shuffledIrisData size * (1 - cut)) rounded.
trainingData := shuffledIrisData first: cutTraining.
testData := shuffledIrisData last: cutTest.
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.
n train: trainingData nbEpochs: 1000.

(((testData collect: [ :d |
	(n predict: d allButLast) = d last
]) select: #yourself) size / testData size) asFloat round: 2 
~~~~~~~

The script introduces a new variable, called `shuffledIrisData`. It is initialized with `irisData shuffleBy: (Random seed: 42)`, which as the effect to create a copy of `irisData` shuffled using a random number. In case we wish to not use a random number generator and therefore have slightly different result at each run, we could simply use `shuffled` instead of `shuffleBy: (Random seed: 42)`.

## Normalization

When we presented the perceptron and the sigmoid neuron, we have seen that the activation function is applied to the value $z = w.x + b$. Applied to a neuron with two inputs, we have $z = x_1 . w_1 + x_2 . w_2 + b$. In the examples we have considered so far, all the $x_i$ and output values ranges in the same interval, from 0 to 1. In the logical gate example, each $x_i$ is either 0 or 1. In the Iris dataset, we can compute the minimum and maximum for each input value:

~~~~~~~
max := OrderedCollection new.
min := OrderedCollection new.
(1 to: 4) collect: [ :i |
	max add: (irisData collect: [ :d | d at: i ]) max.
	min add: (irisData collect: [ :d | d at: i ]) min.
].
{ max . min }
~~~~~~~

The result of this script indicates that overall, the values ranges from 
0.1 to 7.9. Said in other words, all the input values have a range within the same magnitude.

Why is this important? Consider the example we have previously seen on converting binary numbers to decimal:

```Smalltalk
n := NNetwork new.
n configure: 3 hidden: 8 nbOfOutputs: 8.

data := {#(0 0 0 0).
    #(0 0 1 1).
    #(0 1 0 2).
    #(0 1 1 3).
    #(1 0 0 4).
    #(1 0 1 5).
    #(1 1 0 6).
    #(1 1 1 7) }.
n train: data nbEpochs: 1000.
```

![Learning the Iris dataset.](06-Data/figures/digitConvertion.png){#fig:digitConvertion}

Figure @fig:digitConvertion shows the error curve of the network. Each input values is either 0 or 1. We will produce a different, but equivalent dataset, by changing the scale of each column. In our revised example, we will make the first input either 0 or 0.1, and the second input either 0 or 1000. Consider:

```Smalltalk
n := NNetwork new.
n configure: 3 hidden: 8 nbOfOutputs: 8.

data := {#(0 0 0 0).
    #(0 0 1 1).
    #(0 1000 0 2).
    #(0 1000 1 3).
    #(0.1 0 0 4).
    #(0.1 0 1 5).
    #(0.1 1000 0 6).
    #(0.1 1000 1 7) }.
n train: data nbEpochs: 10000.
```

![The Iris dataset oddly scaled.](06-Data/figures/digitConvertionBiased.png){#fig:digitConvertionBiased}

Figure @fig:digitConvertionBiased shows the error curve and the precision along the epochs. The evolution of the error has reached a plateau and the precision does not go above 0.5. The reasons is that changing the scale of a particular input value affects the relevance of these values.

The sigmoid function returns a value between 0 and 1. Having the same range for the input improves the learning performance. One way to avoid distortion in our data, each input should range between 0 and 1. The process of transforming data from an arbitrary range to  a restricted range is called _normalization_. 

Luckily, normalizing data is rather simple. Consider the function $f$:

$$
f(x) = \frac{(x - d_L)(n_H - n_L)}{d_H - d_L} + n_L
$$

The function $f(x)$ normalizes a value $x$. The variables $d$ represents the high and low values of the data. The variables $n$ represents the desired high and low normalization range. In most of our cases, we will have $n_L = 0$ and $n_H = 1$.

We can therefore implement the following utility class: 

```Smalltalk
Object subclass: #Normalization
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We then define the method `normalizeData:`, which takes as argument some training data:

```Smalltalk
Normalization>>normalizeData: aCollectionOfTrainingDataWithExpectedOutput
	"Normalize the data provided as argument"
	
	| nbOfColumns min max |
	"We exclude the expected output"
	nbOfColumns := aCollectionOfTrainingDataWithExpectedOutput first size - 1.
	
	min := OrderedCollection new.
	max := OrderedCollection new.
	1 to: nbOfColumns do: [ :index |
		| column |
		column := aCollectionOfTrainingDataWithExpectedOutput collect: [ :row | row at: index ].
		min add: column min.
		max add: column max ].

	^ self normalizeData: aCollectionOfTrainingDataWithExpectedOutput min: min max: max
```

The real work happens in this second method:

```Smalltalk
Normalization>>normalizeData: aCollectionOfTrainingDataWithExpectedOutput min: minimumValues max: maximumValues
	| nbOfColumns result mn mx |
	nbOfColumns := aCollectionOfTrainingDataWithExpectedOutput first size - 1.

	result := OrderedCollection new.
	aCollectionOfTrainingDataWithExpectedOutput do: [ :row |
		| t v |
		t := OrderedCollection new.
		1 to: nbOfColumns do: [ :index |
			v := row at: index.
			mn := minimumValues at: index.
			mx := maximumValues at: index.
			t add: ((v - mn) / (mx - mn)) asFloat
		].
		t add: row last.
		result add: t asArray ].
	^ result asArray
```

We can test these methods. First we can create a unit test `NormalizationTest`:

```Smalltalk
TestCase subclass: #NormalizationTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```


```Smalltalk
NormalizationTest>>testSimpleNormalization
	| input expectedNormalizedInput |
	input := #( #(10 5 1) #(2 6 0) ).
	expectedNormalizedInput := Normalization new normalizeData: input.
	self assert: expectedNormalizedInput equals: #(#(1.0 0.0 1) #(0.0 1.0 0))
```

This small test method illustrates the result of a simple normalization. For example, the first column of the two entries of `input` has 10 as the highest value and 2 as the lowest. The normalization replaces the highest value by 1.0 and the lowest by 0.0. 

Note that the normalization makes sense only if two or more entries are provided as input. We can test erroneous cases:

```Smalltalk
NormalizationTest>>testError
	self should: [ Normalization new normalizeData: #( #(10 5 1) ) ] raise: Error.
	
```

```Smalltalk
NormalizationTest>>testEmptyError
	self should: [ Normalization new normalizeData: #() ] raise: Error.
```

When a neural network is used for regression returned values are normalized. We therefore need to _denormalize_ them. Consider the function $g$: 

$$
g(x) = \frac{(d_L - d_H)x - (n_H d_L) + d_H n_L}{n_L - n_H}
$$

We give the denormalization function for sake of completeness. We will not use it since we excluded data regression from this chapter.

## Integrating the Normalization into NNetwork

The previous section described the normalization functionality. Currently, it is disconnected from the class `NNetwork`. Integrating the normalization in our neural network is the natural next step to seamlessly benefit from it. The method `train:nbEpochs:` can be redefined as follow:

```Smalltalk
NNetwork>>train: train nbEpochs: nbEpochs
    "Train the network using the train data set."
    | sumError outputs expectedOutput epochPrecision t normalizedTrain |
	normalizedTrain := Normalization new normalizeData: train. 
    1 to: nbEpochs do: [ :epoch |
        sumError := 0.
          epochPrecision := 0.
        normalizedTrain do: [ :row |
            outputs := self feed: row allButLast.
            expectedOutput := (1 to: self numberOfOutputs) collect: [ :notUsed | 0 ].
            expectedOutput at: (row last) + 1 put: 1.
            (row last = (self predict: row allButLast)) ifTrue: [ epochPrecision := epochPrecision + 1 ].
            t := (1 to: expectedOutput size) 
                    collect: [ :i | ((expectedOutput at: i) - (outputs at: i)) squared ].
            sumError := sumError + t sum.
            self backwardPropagateError: expectedOutput.
            self updateWeight: row allButLast.
        ].
        errors add: sumError.
          precisions add: (epochPrecision / train size) asFloat.
    ] 
``` 

The revision of the method normalizes the input data with the expression `Normalization new normalizeData: train`. The result of this expression is used to actually train the network.

Running the following script indicates that a high precision is quickly reached (Figure @fig:learningCurve):

```Smalltalk
n := NNetwork new.
n configure: 3 hidden: 8 nbOfOutputs: 8.

data := {#(0 0 0 0).
    #(0 0 1 1).
    #(0 1000 0 2).
    #(0 1000 1 3).
    #(0.1 0 0 4).
    #(0.1 0 1 5).
    #(0.1 1000 0 6).
    #(0.1 1000 1 7) }.
n train: data nbEpochs: 10000.
```


![The Iris dataset oddly scaled.](06-Data/figures/irisWithNormalization.png){#fig:irisWithNormalization}

Figure @fig:learningCurve shows the precision reaching 1.0. Thanks to the normalization, all the input values have the same relevance for the network. As a consequence, the network is able to learn properly. Note that in this example we use a linear normalization. It may be that a non-linear transformation may improve the learning, especially in presence of outlier values in the training data. However, we consider non-linear data transformation as out of the scope of this book. Keep in mind that you may need it in case of dataset with relevant outliers.

## What have we seen in this chapter

This chapter was like a long road exploring different aspects of data manipulation. In particular, it explores:

- A simple visualization to monitor the learning of a network
- The one-hot encoding technique to make a network operates on non-numeric data
- The Iris dataset as a complete example of applying network network to classify data
- The relevance of normalizing data before processing

We invite the reader to explore different dataset. The website [https://archive.ics.uci.edu/ml/datasets.html](https://archive.ics.uci.edu/ml/datasets.html) gives many relevant datasets to be employed with a neural network or any other machine learning algorithm.
