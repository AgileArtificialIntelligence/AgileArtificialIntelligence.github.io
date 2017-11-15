# Perceptron

This chapters plays two roles. The first one, is to describes how and why a perceptron plays a role so important in the meaning of deep learning. The second role of this chapter, is to provide a gentle introduce to the Pharo programming language.

## Biological Connection

The primary visual cortex contains 140 millions of neurons, with tens of billions of connections. A typical neuron propagates electrochemical stimulation received from other neural cells using *dendrite*. An *axon* conducts electrical impulses away from the neuron ([Neuron](#Neuron)).

[Neuron]: 02-Perceptron/figures/neuron.png "Image Title" 
![Neuron][Neuron] 

Expressing a computation in terms of artificial neurons was first thought in 1943, by Warren S. Mcculloch and Walter Pitts in their seminal article *A logical calculus of the ideas immanent in nervous activity*. This paper has been cited more than 14 000 times.

## Perceptron

A perceptron is a kind of miniature machine that produces an output for a provided input ([Perceptron](#Perceptron)). A perceptron may accept 0, 1, or more inputs, and result in a small and simple computation. A perceptron operates on numerical values, which means that the inputs and the output are numbers (integer or float, as we will see later).

[Perceptron]: 02-Perceptron/figures/perceptron.png
![Perceptron][Perceptron]

The figure depicts a perceptron with three inputs, noted _x1_, _x2_, and _x3_. Each input is indicated with an incoming arrow and the output with the outgoing arrow. The $y = x^2 \hbox{ when $x > 2$}$ $\sum{1}{2}a^2 + b^2 = c^2$

Not all inputs have the same importance for the perceptron. For example, an input may be more important than the others. Relevance of an input is expressed using a weight associated to that input. In our figure, the input _x1_ has the weight _w1_, _x2_ has the weight _w2_, and _x3_ has _w3_. 

How likely is the perceptron responding to the input stimulus? The bias is a value that indicates whether 

## From transistor to logical gates

% http://www.cs.bu.edu/~best/courses/modules/Transistors2Gates/
A perceptron is a kind of artificial neuron, developed in the 50s and 60s by Frank Rosenblatt, Warren McCulloch, and Walter Pitts. 


## A Perceptron in action

We have seen so far a great deal of theory. We will implement a perceptron from scratch.

We first need to open a code _system browser_ by selecting the corresponding entry in the World menu. The system browser is where you read and write source code. Most of the programming activity will actually happens in a system browser. A system browser is composed of five different parts. The above part is composed of four lists. The left-most provides the packages, the following list gives the classes that belongs to a selected package. The third list gives the method cateogies for the selected class. A method category is a bit like a package, but for methods. The left-most list gives the methods that belongs in the class under a particular method category. The below part of a system browser gives source code, which is either a class template to fill in order to create a class, the source code of the selected class, or the source code of a selected method.

Right-click on the left-most top list to create a new package, let's call it `NeuralNetwork`. This package will contain most of the code we will write in this book. 

[systemBrowser]: 02-Perceptron/figures/systemBrowser.png
![systemBrowser][systemBrowser]


When you select our package, a class template appears in the below code. Fill it to have the following:

~~~~~~~
Object subclass: #Perceptron
	instanceVariableNames: 'weights bias'
	classVariableNames: ''
	package: 'NeuralNetwork'
~~~~~~~

You then need to compile the code by ''accept''-ing the source code. Right click on the text pane and select the option ''Accept''. The class we have defined contains two instance variables, `weights` and `bias`. 
We now have to add a few methods that manipulate these variables before some actual work. Let first focus on manipulating the `weights` variable. We will define two methods to write a value to that variable and another to read from it.

Here is the code of the `weights:` method defined in the class `Perceptron`:

~~~~~~~
Perceptron>>weights: someWeightsAsNumbers
	weights := someWeightsAsNumbers copy
~~~~~~~

To define this method, you need to select the class in the class panel (second top list panel). Then write the code given above *without* `Perceptron>>`. Then you should accept the code, by right clicking on the 'Accept' menu item. Accepting a method has the effect to compile it. The code define the method named `weights:` which accepts one argument, provided as a variable named `someWeightsAsNumbers`. 

The expression `weights := someWeightsAsNumbers copy` creates a copy of the provided argument and assign it to the variable `weights`. The copy is not necessary, but it is useful to prevent some hard-to-debug issues.

[systemBrowserAndMethodWeight]: 02-Perceptron/figures/systemBrowserAndMethodWeight.png
![systemBrowserAndMethodWeight][systemBrowserAndMethodWeight]

We know need a method to read the content of that variable. Here is the apropriate method: 

~~~~~~~
Perceptron>>weights
	^ weights
~~~~~~~

The character `^` returns the value of an expression, the value of the variable `weights` in that case.

Similarly we need to define a method to assign a value to the `bias` variable and to read its content. The method `bias:` can be defined as:

~~~~~~~
Perceptron>>bias: aNumber
	bias := aNumber
~~~~~~~

And the reading may be defined using: 

~~~~~~~
Perceptron>>bias
	^ bias
~~~~~~~

So far, we have defined the class `Perceptron` which contains two variables (`weights` and `bias`), and 4 methods (`weights:`, `weights`, `bias:`, and `bias`). The last piece to add is applying a set of inputs values and obtaining the output value. The method `feeds:` can be defined as:

~~~~~~~
Perceptron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias .
	output := z > 0 ifTrue: [ 1 ] ifFalse: [ 0 ].
	^ output
~~~~~~~

The method `feed:` simply phrase the formula to model the activation of a perceptron into the Pharo programming language.
The expression `inputs with: weights collect: [ :x :w | x * w ]` collects for each pair of elements (one from `inputs` and another from `weights`) using a function. Consider the following example:

~~~~~~~
#(1 2 3) with: #(10 20 30) collect: [ :a :b | a + b ]
~~~~~~~

The above expression evaluates to `#(11 22 33)`. Syntactically, it means that the literal value `#(1 2 3)` receives a message called `with:collect:`, with two arguments, the literal `#(10 20 30)` and the block `[ :a :b | a + b ]`.
You can verify the value of that expression by opening a playground, accessible from the World menu. A playground is a kind of command terminal (''e.g.,'' xterm in the Unix World).

[playground]: 02-Perceptron/figures/playground.png
![playground][playground]


## Testing our code


## Formulating Logical expressions


~~~~~~~
TestCase subclass: #NNPerceptronTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetworksTests'
~~~~~~~


~~~~~~~
NNPerceptronTest>>testAND
	| p |
	p := MPPerceptron new.
	p weights: { 1 . 1 }.
	p bias: -1.5.
	
	self assert: (p feed: { 0 . 0 }) equals: 0.
	self assert: (p feed: { 0 . 1 }) equals: 0.
	self assert: (p feed: { 1 . 0 }) equals: 0.
	self assert: (p feed: { 1 . 1 }) equals: 1
~~~~~~~

~~~~~~~
NNPerceptronTest>>testOR
	| p |
	p := MPPerceptron new.
	p weights: { 1 . 1 }.
	p bias: -0.5.
	
	self assert: (p feed: { 0 . 0 }) equals: 0.
	self assert: (p feed: { 0 . 1 }) equals: 1.
	self assert: (p feed: { 1 . 0 }) equals: 1.
	self assert: (p feed: { 1 . 1 }) equals: 1
~~~~~~~

~~~~~~~
Perceptron>>train: inputs desiredOutput: desiredOutput
	| c newWeights output |
	output := self feed: inputs.
	c := 0.1.
	"Works well"
	desiredOutput = output
		ifTrue: [ ^ self ].

	"Basic check"
	self assert: [ weights size = inputs size ] description: 'Wrong size'.
	desiredOutput = 0
		ifTrue: [ "we should decrease the weight"
			newWeights := (1 to: weights size) collect: [ :i | (weights at: i) - (c * (inputs at: i)) ].
			bias := bias - c ]
		ifFalse: [ "We have: designedOutput = 1"
			newWeights := (1 to: weights size) collect: [ :i | (weights at: i) + (c * (inputs at: i)) ].
			bias := bias + c ].
	weights := newWeights
~~~~~~~


~~~~~~~
NNPerceptronTest>>testTrainingOR
	| p |
	p := MPPerceptron new.
	p weights: { -1 . -1 }.
	p bias: 2.
	
	100 timesRepeat: [ 
		p train: { 0 . 0 } desiredOutput: 0.
		p train: { 0 . 1 } desiredOutput: 1.
		p train: { 1 . 0 } desiredOutput: 1.
		p train: { 1 . 1 } desiredOutput: 1.
	].
	
	self assert: (p feed: { 0 . 0 }) equals: 0.
	self assert: (p feed: { 0 . 1 }) equals: 1.
	self assert: (p feed: { 1 . 0 }) equals: 1.
	self assert: (p feed: { 1 . 1 }) equals: 1
~~~~~~~

~~~~~~~
p := MPPerceptron new.
p weights: { -1 . -1 }.
p bias: 2.
	
100 timesRepeat: [ 
	p train: { 0 . 0 } desiredOutput: 0.
	p train: { 0 . 1 } desiredOutput: 1.
	p train: { 1 . 0 } desiredOutput: 1.
	p train: { 1 . 1 } desiredOutput: 1.
].
p feed: { 1 . 0 }
~~~~~~~

## Exercises

The method `train:desiredOutput:` defines the learning rate `c` with the value `0.1`. Try using different values.

