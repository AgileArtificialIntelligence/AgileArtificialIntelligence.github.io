
# Perceptron

A neuron, as contained in a mammal brain, is a specialized cell that transmits electrochemical stimulation using an *axon* to other neurons. A neuron receives this nerve impulse via a *dendrite*. Since the early age of computers scientists have tried to produce a computational model of a neuron. The perceptron was one of the first model to mimic the behavior of a neuron.

This chapter plays two essential role in the book. First, it presents the notion of perceptron, a fundamental model on which neural networks are built. Second, it provides a gentle introduction to the Pharo programming language.

## Perceptron

A perceptron is a kind of artificial neuron that models the behavior of a real neuron.
A perceptron is a machine that produces an output for a provided input (Figure @fig:perceptron). 

A perceptron may accept 0, 1, or more numerical values, considered as inputs. It produces a numerical value as output, result of a simple equation. A perceptron operates on numbers, which means that the inputs and the output are numerical values (_e.g.,_ integers or floating point values).

![Representing the perceptron.](02-Perceptron/figures/perceptron.png){#fig:perceptron width=200px}


Figure @fig:perceptron depicts a perceptron with three inputs, noted _x1_, _x2_, and _x3_. Each input is indicated with an incoming arrow and the output with an outgoing arrow. 

Not all inputs have the same importance for the perceptron. For example, an input may be more important than the others inputs. Relevance of an input is expressed using a weight (also a numerical value) associated to that input. In our figure, the input _x1_ is associated to the weight _w1_, _x2_ to the weight _w2_, and _x3_ to _w3_. 

In addition to the weighted input value, a perceptron requires a _bias_, a numerical value acting as a threshold. We denote the bias as _b_.

A perceptron receives a stimulus as input and respond to that stimulus by producing an output value. The output obeys a very simple rule. First, we compute the sum of the weighted inputs and the bias. If this sum is above 0, then the perceptron produce 1, else it produces 0.

More formally, for the perceptron given in Figure @fig:perceptron, we write $z = x1 * w1 + x2 * w2 + x3 * w3 + b$. In the general case, we write $z = \sum_i{x_i * w_i}~ + b$. If $z$ is greater then 0, then the perceptron produces 1, else it produces 0.

In the next section we will implement the perceptron model that is both extensible and maintainable. You may wonder what is the big deal about this. After all, the perceptron model may be implemented in a few lines of code. Yes, focusing on the functionalities is just a fraction of our job. Implementing the perceptron model that is testable, well tested, and extensible requires some more work. 

## Implementing the perception

In this section we put our hand to work and implement the perceptron model in the Pharo programming language. We will produce an object-oriented implementation of the model. We will implement a class `Neuron` in a package called `NeuralNetwork`. Our class will have a method called `feed` which will be used to compute the $z$ and output values of the perceptron.

To create a new package, we first need to open a _system browser_ by selecting the corresponding entry in the Pharo menu. The system browser is where you read and write source code. Most of the programming activity will actually happens in a system browser. 

Figure @fig:systemBrowser represents the system browser. A system browser is composed of five different parts. The above part is composed of four lists. The left-most list gives the available and ready-to-be-used packages. The second list gives the classes that belongs to a selected package. The third list gives the method categories for the class you have selected. A method category is a container of methods. It is for methods what a package is for classes. The right-most list gives the methods that belongs in the class under a particular method category. If no category is selected, all the methods that belongs to the selected class are listed. The below part of a system browser gives source code, which is either a class template to be filled in order to create a class, the source code of the selected class, or the source code of a selected method.

![The Pharo system browser.](02-Perceptron/figures/systemBrowser.png){#fig:systemBrowser}

Right-click on the left-most top list to create a new package, let's call it `NeuralNetwork`. This package will contain most of the code we will write in this book. 

Select the package `NeuralNetwork` you have just created and type the following:

~~~~~~~
Object subclass: #Neuron
	instanceVariableNames: 'weights bias'
	classVariableNames: ''
	package: 'NeuralNetwork'
~~~~~~~

You then need to compile the code by accepting the source code. Right click on the text pane and select the option `Accept`. The class we have defined contains two instance variables, `weights` and `bias`. We need to add some methods to give a meaning to our class.
In particular, we need a few methods that manipulate these variables in addition to the logic, which is to compute the $z$ and output values. Let first focus on the `weights` variable. We will define two methods to write a value to that variable and another to read from it.

You may wonder why we define a class `Neuron` and not `Perceptron`. In the next chapter we will expand our class `Neuron` by turning it into an open abstraction for artificial neuron. Our `Neuron` class is therefore a placeholder for improvements we will do in the subsequent chapters.

Here is the code of the `weights:` method defined in the class `Neuron`:

~~~~~~~
Neuron>>weights: someWeightsAsNumbers
	weights := someWeightsAsNumbers
~~~~~~~

To define this method, you need to select the `Neuron` class in the class panel (second top list panel). Then write the code given above *without* `Neuron>>`. Then you should accept the code, by right clicking on the `Accept` menu item. In the Pharo Jargon, accepting a method means to compile it. Once compiled, it may be invoked. The code defines the method named `weights:` which accepts one argument, provided as a variable named `someWeightsAsNumbers`. 

The expression `weights := someWeightsAsNumbers` assigns the value `someWeightsAsNumbers` to the variable `weights`.

![The `weights:` method of the `Neuron` class.](02-Perceptron/figures/systemBrowserAndMethodWeight.png){#fig:systemBrowserAndMethodWeight}

You should now have a similar content than Figure @fig:systemBrowserAndMethodWeight.
The method `weights:` write a value to the variable `weights`. A method that returns the value of it is:

~~~~~~~
Neuron>>weights
	^ weights
~~~~~~~

The character `^` returns the value of an expression, the value of the variable `weights` in that case.

Similarly we need to define a method to assign a value to the `bias` variable and to read its content. The method `bias:` can be defined as:

~~~~~~~
Neuron>>bias: aNumber
	bias := aNumber
~~~~~~~

And reading the variable `bias` is supported with the method:

~~~~~~~
Neuron>>bias
	^ bias
~~~~~~~

So far, we have defined the class `Neuron` which contains two variables (`weights` and `bias`), and 4 methods (`weights:`, `weights`, `bias:`, and `bias`). We need to define the logic of our perceptron by applying a set of inputs values and obtaining the output value. The method `feeds:` can be defined as:

~~~~~~~
Neuron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
	^ z > 0 ifTrue: [ 1 ] ifFalse: [ 0 ].
~~~~~~~

The method `feed:` simply follows the formula to model the activation of a perceptron in the Pharo programming language.
The expression `inputs with: weights collect: [ :x :w | x * w ]` collects for each pair of elements (one from `inputs` and another from `weights`) using a function. Consider the following example:

~~~~~~~
#(1 2 3) with: #(10 20 30) collect: [ :a :b | a + b ]
~~~~~~~

The above expression evaluates to `#(11 22 33)`. Syntactically, it means that the literal value `#(1 2 3)` receives a message called `with:collect:`, with two arguments, the literal `#(10 20 30)` and the block `[ :a :b | a + b ]`.
You can verify the value of that expression by opening a playground, accessible from the main Pharo menu. A playground is a kind of command terminal (_e.g.,_ xterm in the Unix World). Figure @fig:playground illustrates the evaluation of the expression given above.

![The Playground.](02-Perceptron/figures/playground.png){#fig:playground width=400px}

We can now play a little bit with a perceptron. Evaluate the following code in the playground we just opened:

~~~~~~~
p := Neuron new.
p weights: #(1 2).
p bias: -2.
p feed: #(5 2)
~~~~~~~

This piece of code evaluates to `1` (since `(5*1 + 2*2) - 2` equals to `7`, which is greater than `0`).

![Evaluating the perceptron.](02-Perceptron/figures/playgroundAndPerceptron.png){#fig:playgroundAndPerceptron width=400px}

## Testing our code

Now is time to talk about testing. Testing is an essential activity  whenever we write code. Software and code testing is essential in agile methodologies and is about raising the confidence that the code we write does what it is supposed to do.

Testing is a central concept in the field of Software Engineering. Although this book is not about writing large software artifacts, we _do_ write source code. And making sure that this code can be tested in an automatic fashion significantly improve the quality of what we are doing. More importantly, it is not only the author of the code (you) that will appreciate the quality of the code, but anyone who will look at it. Along the chapters, we will improve our codebase. It is therefore very important to make sure that our improvement do not break some of the functionalities. 

For example, above we defined a perceptron, and we informally tested it in a playground. This informal test cost us a few keystrokes and a little bit of time. What if we can automatically repeat this test each time we modify our definition of perceptron? This is exactly what _unit testing_ is all about. 

We now define a class called `PerceptronTest`, defined as:

~~~~~~~
TestCase subclass: #PerceptronTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
~~~~~~~


The class `TestCase` belongs to the Pharo codebase and subclassing it is the first step to create a unit test. Tests can now be added to our `PerceptronTest`. Define the following method:

~~~~~~~
PerceptronTest>>testSmallExample
	| p result |
	p := Neuron new.
	p weights: #(1 2).
	p bias: -2.
	result := p feed: #(5 2).
	self assert: result equals: 1.
~~~~~~~

The method `testSmallExample` tests that the code snippet we previously gave returns the value `1`. 
The test can be run by clicking on the gray circle located next to the method name (Figure @fig:testingPerceptron01).

![Testing the perceptron.](02-Perceptron/figures/testingPerceptron01.png){#fig:testingPerceptron01}


The green bullet next to the method name indicates that the test passes (_i.e.,_ no assertion failed and no error got raised). The method `testSmallExample` sends the message  `assert:equals:` which tests whether the first argument equals the second argument. 

*EXERCISE:* So far, we have only shallowly tested our perceptron. We can improve our tests in two ways:

- Create a new test, called `testSmallExample`, that tests that feeding our perceptron `p` with different values (_e.g.,_ `-2` and `2` gives `0` as result)
- Test our perceptron with different weights and bias

In general, it is a very good practice to write a good amount of tests, even for a single component unit as for our class `Neuron`.


## Formulating Logical expressions

A canonical example of using perceptron (or any other artificial neuron) is to express boolean logical gates. The idea is to have a perceptron with two inputs (each being a boolean value), and the result of a logical gate as output. 

A little bit of arithmetic indicates that a perceptron with the weights `#(1 1)` and the bias `-1.5` formulates the AND logical gate. The AND gate may be represented as the following table:

A | B | A AND B 
--- | --- | :---:
0 | 0 | 0
0 | 1 | 0
1 | 0 | 0
1 | 1 | 1

We could therefore verify this with a new test method:

~~~~~~~
PerceptronTest>>testAND
	| p |
	p := Neuron new.
	p weights: #(1 1).
	p bias: -1.5.
	
	self assert: (p feed: #(0 0)) equals: 0.
	self assert: (p feed: #(0 1)) equals: 0.
	self assert: (p feed: #(1 0)) equals: 0.
	self assert: (p feed: #(1 1)) equals: 1.
~~~~~~~

Similarly, a perceptron can formulate the OR logical gate:

A | B | A OR B 
--- | --- | :---:
0 | 0 | 0
0 | 1 | 1
1 | 0 | 1
1 | 1 | 1

Consider the following test:

~~~~~~~
PerceptronTest>>testOR
	| p |
	p := Neuron new.
	p weights: #(1 1).
	p bias: -0.5.
	
	self assert: (p feed: #(0 0)) equals: 0.
	self assert: (p feed: #(0 1)) equals: 1.
	self assert: (p feed: #(1 0)) equals: 1.
	self assert: (p feed: #(1 1)) equals: 1.
~~~~~~~~

Negating the weights and bias results in the negated logical gate:

~~~~~~~
PerceptronTest>>testNOR
	| p |
	p := Neuron new.
	p weights: #(-1 -1).
	p bias: 0.5.
	
	self assert: (p feed: #(0 0)) equals: 1.
	self assert: (p feed: #(0 1)) equals: 0.
	self assert: (p feed: #(1 0)) equals: 0.
	self assert: (p feed: #(1 1)) equals: 0.
~~~~~~~

So far we built perceptrons with two inputs. A perceptron accepts the same number of inputs than the number of weights. Therefore, if only one weight is provided, only one input is required. Consider the NOT logical gate:

~~~~~~~
PerceptronTest>>testNOT
	| p |
	p := Neuron new.
	p weights: #(-1).
	p bias: 0.5.
	
	self assert: (p feed: #(1)) equals: 0.
	self assert: (p feed: #(0)) equals: 1.
~~~~~~~

## Handling error

In the `testNOT` test, we have defined a perceptron with only one weight. The array provided when calling`feed:` _must_ have only one entry. But what happens if we have two entries instead of one? An error should occurs as we are wrongly using the (small) API we have defined.

We should also test this behavior to make sure errors are properly generated. Define the following tests:

~~~~~~~
PerceptronTest>>testWrongFeeding
	| p |
	p := Neuron new.
	p weights: #(-1).
	p bias: 0.5.
	
	self should: [ p feed: #(1 1) ] raise: Error
~~~~~~~

The test `testWrongFeeding` passes only if the expression `p feed: #(1 1)` raises an error, which it does. 

![Running our tests.](02-Perceptron/figures/runningTests.png){#fig:runningTests}

Until now, we have defined the class `Neuron` with five methods, and the unit test `PerceptronTest` with six test methods. All the tests can be run by pressing the circle next to the unit test name (Figure @fig:runningTests).

## Combining perceptrons

So far, we have defined the AND, NOR, NOT, and OR logical gates. Logical gates become interesting when combined. A digital comparator circuit is such a combination. It is useful to compare two values, A and B. We have three possible outcomes:

- A is greater than B
- A is equal to B
- A is lesser than B

We can therefore model our circuit with two inputs and three outputs. The following table summarizes the circuit:

A | B | A < B | A = B | A > B
--- | --- | :---: | :---: | :---:
0 | 0 | 0 | 1 | 0
0 | 1 | 1 | 0 | 0
1 | 0 | 0 | 0 | 1
1 | 1 | 0 | 1 | 0


![Digital Comparator Circuit.](02-Perceptron/figures/digitalComparator.png){#fig:digitalComparator width=400px}

Figure @fig:digitalComparator illustrates the circuit. Three different logical gates are necessary: AND, NOT, and NOR. We then need to make the connection between these gates. As we previously did, some tests will drive our effort. The method `digitalComparator:`, defined in our unit test for convenience, models the digital comparator circuit:

~~~~~~~
PerceptronTest>>digitalComparator: inputs
	"Return an array of three elements"
	| not and nor A B AgB AeB AlB notA notB |
	A := inputs first.
	B := inputs second.

	and := Neuron new weights: #(1 1); bias: -1.5.
	not := Neuron new weights: #(-1); bias: 0.5.
	nor := Neuron new weights: #(-1 -1); bias: 0.5.	

	notA := not feed: { A }. 
	notB := not feed: { B }.
	
	AlB := and feed: { notA . B }.
	AgB := and feed: { A . notB }.
	AeB := nor feed: { AgB . AlB }.
	^ { AgB . AeB . AlB }
~~~~~~~

The method accepts a set of inputs as argument. We first extract the first and second elements of these inputs and assign them to the temporary variables `A` and `B`. 

We then create our three logical gates as perceptrons. We then wire then using the variables `notA`, `notB`, `AgB` (standing for `A` greater than `B`), `AlB` (`A` lesser than `B`), and `AeB` (`A` equals to `B`). 

We then compute `notA` and `notB`. We use an alternative way to define array. The expression `{ A }` creates an array with the object referenced by `A`. We use the notation `#(...)` only for array of numbers (_e.g.,_ `#(1 -1)`). Note that we can also write numbers using the `{...}` syntax (_e.g.,_ `{1 . -1}`). It is important to keep in mind these two notations as we will heavily use them along the book.

The method `digitalComparator:` returns an array with the result of the circuit evaluation. We can test it using a test method:

~~~~~~~
PerceptronTest>>testDigitalComparator
	self assert: (self digitalComparator: #(0 0)) equals: #(0 1 0).
	self assert: (self digitalComparator: #(0 1)) equals: #(0 0 1).
	self assert: (self digitalComparator: #(1 0)) equals: #(1 0 0).
	self assert: (self digitalComparator: #(1 1)) equals: #(0 1 0).
~~~~~~~

The digital comparator circuit example show how perceptrons may be "manually" combined. 
The overall behavior is cut down into parts, each referenced with a variable. These variables then must be combined to express the logical flow (_e.g.,_ the variable `notA` must be computed before computing an output). When we will discuss about training a neural network, we will come back to that particular example. A training will actually (i) compute some weights and bias and (ii) establish the wire between the neurons automatically. 

## Training a Perceptron

Neurons have the ability to learn from examples. This _training_ is essential to actually do something useful. Learning typically involves a set of input examples with some known outputs. The learning process assesses how good the artificial neuron is against the desired output. In particular, as defined by Frank Rosenblatt in the late 1950s, each weight of the perceptron is modified by an amount that is proportional to (i) the product of the input and (ii) the difference between the real output and the desired output. Learning in neural networks means adjusting the weights and the bias in order to make the output close to the set of training examples. 

The way a perceptron learns simply follows the rule: $w_i(t+1) = w_i(t) + (d - z) * x_i * \alpha$, in which:

- $i$ is the weight index
- $w_i(t+1)$ is the weight $i$ at a given time $t+1$
- $d$ is the difference between the desired value and the actual value
- $z$ is the actual output of the perceptron
- $x_i$ corresponds to the provided input at index $i$
- $\alpha$ is the learning rate

A way to make perceptron learn is given by the method `train:desiredOutput:`, as follow:

~~~~~~~
Neuron>>train: inputs desiredOutput: desiredOutput
	| learningRate theError output newWeight |
	output := self feed: inputs.
	learningRate := 0.1.
	theError := desiredOutput - output.
	inputs
		withIndexDo: [ :anInput :index | 
			newWeight := (weights at: index) + (learningRate * theError * anInput).
			weights at: index put: newWeight ].
	bias := bias + (learningRate * theError)
~~~~~~~

Before doing any adjustment of the weights and bias, we need to know how well the perceptron evaluates the set of inputs. We therefore need to evaluate the perceptron with the argument `inputs`. The result is assigned to the variable `output`. The variable `theError` represents the difference between the desired output and the actual output. We also need to decide how fast the perceptron is supposed to learn. The `learningRate` value is a value between `0.0` and `1.0`. We arbitrarily picked the value `0.1`. 


Let's see how to use the training in practice. Consider the perceptron `p` given as (you can evaluate the following code in a playground):

~~~~~~~
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
p feed: #(0 1).
~~~~~~~


We have `p feed: #(0 1)` equals to `1`. What if we wish the perceptron to actually output `0` for the input `#(0 1)`? We therefore need to train `p` to actually output `0`. As we said, this training will adjust the weights and the bias. Let's try the following:

~~~~~~~
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
p train: #(0 1) desiredOutput: 0.
p feed: #(0 1).
~~~~~~~

Evaluating this expression still outputs `1`. Well... Were we not supposed to train our perceptron? A perceptron learns slowly. We therefore need to actually train the perceptron a few times what the desired output is. We can repeatably train the perceptron as follows:

~~~~~~~
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
10 timesRepeat: [ p train: #(0 1) desiredOutput: 0 ].
p feed: #(0 1).
~~~~~~~

Evaluating the code given above produces `0`, as we were hopping for (Figure @fig:playgroundWithLearningPerceptron). Our perceptron has learned!

![Teaching a perceptron.](02-Perceptron/figures/playgroundWithLearningPerceptron.png){#fig:playgroundWithLearningPerceptron width=400px}

We can now train a perceptron to actually learn how to express the logical gates. Consider the following `testTrainingOR`:

~~~~~~~
PerceptronTest>>testTrainingOR
	| p |
	p := Neuron new.
	p weights: { -1 . -1 }.
	p bias: 2.
	
	40 timesRepeat: [ 
		p train: { 0 . 0 } desiredOutput: 0.
		p train: { 0 . 1 } desiredOutput: 1.
		p train: { 1 . 0 } desiredOutput: 1.
		p train: { 1 . 1 } desiredOutput: 1.
	].
	
	self assert: (p feed: { 0 . 0 }) equals: 0.
	self assert: (p feed: { 0 . 1 }) equals: 1.
	self assert: (p feed: { 1 . 0 }) equals: 1.
	self assert: (p feed: { 1 . 1 }) equals: 1.
~~~~~~~

The method `testTrainingOR` first creates a perceptron with some arbitrary weights and bias. We successfully train it with the four possible combinations of the OR logical gate. After the training, we test the perceptron to see if it has actually properly learn. 

In `testTrainingOR`, we train the perceptron 40 times the complete set of examples. Training a perceptron (or a large neural network) with the complete set of examples is called _epoch_. So, in our example, we train `p` with 40 epochs. The epoch is the unit of training.

*EXERCISE:*
- What is the necessary minimum number of epochs to train `p`? You can try to modify `25` by a lower value and run the test to see if it still passes.
- We have shown how to train a perceptron to learn the OR logical gate. Write a method `testTrainingNOR`, `testTrainingAND`, and `testTrainingNOT` for the other gates we have seen.
- How the value of the `learningRate` impacts the minimum number of epochs for the training?

## Predicting side of a 2D point

A perceptron can be used to classify data and make some predictions. We will pick a simple classification problem. Consider the following:

- A space composed of red and blue points
- A straight line divides the red points from the blue points

Some questions arise:

- Can we teach a perceptron to correctly assign the color of a point?
- How many example points do we need to train the perceptron with in order to make good prediction?

Let's pick a linear function, such as $f(x) = -2x - 3$. A given point $(x, y)$ is colored in red if $y > f(x)$, else it is blue. Consider the following script:

~~~~~~~
somePoints := OrderedCollection new.
500 timesRepeat: [ 
	somePoints add: {(50 atRandom - 25) . (50 atRandom - 25)}
].

f := [ :x | (-2 * x) - 3 ].

"We use the Grapher engine to plots our points"
g := RTGrapher new.
d := RTData new.
d dotShape 
	color: [ :p | (p second > (f value: p first)) 
					ifTrue: [ Color red trans ] 
					ifFalse: [ Color blue trans ] ].
d points: somePoints.
d x: #first.
d y: #second.
g add: d.
g
~~~~~~~

Inspecting this code snippet produces a graph with 500 colored dots (Figure @fig:simpleLine).

![Classifying dots along a line.](02-Perceptron/figures/simpleLine.png){#fig:simpleLine}

The script begins by defining a set of 500 points, ranging within a squared area of 50 (from -25 to +25). The expression `50 atRandom` returns a random number between 1 and 50. The expression `{(50 atRandom - 25) . (50 atRandom - 25)}` creates an array with two random values in it. Each point is represented as an array of two numbers. Our 500 points are kept in a collection, an instance of the class `OrderedCollection`.

We assign to the variable `f` a block representing our function $f(x)$, written in the Pharo syntax. A block may be evaluated with the message `value:`. For example, we have `f value: 3` that returns `-9` and `f value: -2` that returns `1`.

The remaining of the script uses Grapher to plot the points. A point `p` is red if `p y` is greater than `f value: p x`, else it is blue. The expression `Color red trans` produces a transparent red color. 

We can add the actual line defined by `f` in our graph. Consider the small revision (Figure @fig:simpleLine2):

~~~~~~~
somePoints := OrderedCollection new.
500 timesRepeat: [ 
	somePoints add: {(50 atRandom - 25) . (50 atRandom - 25)}
].

f := [ :x | (-2 * x) - 3 ].

g := RTGrapher new.
d := RTData new.
d dotShape 
	color: [ :p | (p second > (f value: p first)) 
					ifTrue: [ Color red trans ] 
					ifFalse: [ Color blue trans ] ].
d points: somePoints.
d x: #first.
d y: #second.
g add: d.

"Added code below"
d2 := RTData new.
d2 noDot.
d2 connectColor: Color red.
d2 points: (-15 to: 15 by: 0.1).
d2 y: f.
d2 x: #yourself.
g add: d2.
g
~~~~~~~

![Adding a separation line.](02-Perceptron/figures/simpleLine2.png){#fig:simpleLine2}



We will now add a perceptron in our script and see how good it performs to guess on which side of the line a point is. Consider the following script:

~~~~~~~
f := [ :x | (-2 * x) - 3 ].
p := Neuron new.
p weights: { 1 . 2 }.
p bias: -1.
r := Random new seed: 42.

"We are training the perceptron"
500 timesRepeat: [ 
	anX := (r nextInt: 50) - 25.
	anY := (r nextInt: 50) - 25.
	designedOutput := (f value: anX) >= anY 
								ifTrue: [ 1 ] ifFalse: [ 0 ].
	p train: { anX . anY } desiredOutput: designedOutput 
].

"Test points"
testPoints := OrderedCollection new.
2000 timesRepeat: [ 
	testPoints add: { ((r nextInt: 50) - 25) . ((r nextInt: 50) - 25) }
].

g := RTGrapher new.
d := RTData new.
d dotShape 
	color: [ :point | (p feed: point) > 0.5 
					ifTrue: [ Color red trans ] 
					ifFalse: [ Color blue trans ] ].
d points: testPoints.
d x: #first.
d y: #second.
g add: d.

d2 := RTData new.
d2 noDot.
d2 connectColor: Color red.
d2 points: (-15 to: 15 by: 0.1).
d2 y: f.
d2 x: #yourself.
g add: d2.
g
~~~~~~~

As earlier, the script begins with the definition of the block function `f`. It then creates a perceptron with some arbitrary weights and bias. Subsequently, a random number generator is created. In our previous scripts, to obtain a random value between 1 and 50, we simply wrote `50 atRandom`. Using a random number generator, we need to write:

~~~~~~~
r := Random new seed: 42.
r nextInt: 50.
~~~~~~~

Why this? First of all, being able to generate random numbers is necessary in all stochastic approaches, which includes neural networks and genetic algorithms. Although randomness is very important, we usually not want to let such random value creates situations that cannot be reproduced. Imagine that our code behaves erratically, likely due to a random value. How can we track down the anomaly in our code? If we have truly random numbers, it means that executing twice the same piece of code may produce (even slightly) different behaviors. It may therefore be complicated to properly test. Instead, we will use a random generator with a known seed to produce a known sequence of random numbers. Consider the expression:

~~~~~~~
(1 to: 5) collect: [ :i | 50 atRandom ]
~~~~~~~

Each time you will evaluate this expression, you will obtain a _new_ sequence of 5 random numbers. Using a generator you have:

~~~~~~~
r := Random new seed: 42.
(1 to: 5) collect: [ :i | r nextInt: 50 ]
~~~~~~~

Evaluating several times this small script always produces the same sequence. This is key to have reproducible and deterministic behavior. In the remaining of the book, we will intensively use random number generators.

Our script then follow with training a perceptron with 500 points. We then create 2,000 test points, which will be then displayed on the screen, using Grapher. We wrote the condition `(p feed: point) > 0.5` to color a point as red. We could have `(p feed: point) = 1` instead, however in the future chapter we will replace the perceptron with another kind of artificial neuron, which will not exactly produce the value 1.

We see that our the area of red points goes closely follows the red line. This means that our perceptron is able to classify points with a good accuracy. 

What if reduce the number of training of our perceptron? You can try this by changing the value `500` by, let's say, `100`. What is the result? The perceptron does not classify points as accurately than with 500 trainings. This follow the intuition we elaborated when we first mentioned the training. More training a perceptron has, more accurate it will be (however, this is not always true with neural networks, as we will see later on).

*EXERCISE:* Reduce the number of time the perceptron is trained. Verify that varying the value 500 to lower value leads to some errors made by the perceptron, illustrated at a mismatch between the red line and the area of colored points. 


## Measuring the precision

We have seen that the number of times we train a perceptron matters very much on how accurate the perceptron is able to classify points. How much training do we need to have a good precision? Keeping track of the precision and the training is essential to see how good our system is to do some classification.

Consider the script:

~~~~~~~
learningCurve := OrderedCollection new.
f := [ :x | (-2 * x) - 3 ].

0 to: 2000 by: 10 do: [ :nbOfTrained |
	r := Random new seed: 42.
	p := Neuron new.
	p weights: { 1 . 2 }.
	p bias: -1.

	nbOfTrained timesRepeat: [ 
		anX := (r nextInt: 50) - 25.
		anY := (r nextInt: 50) - 25.
		trainedOutput := (f value: anX) >= anY ifTrue: [1] ifFalse: [0].
		p train: (Array with: anX with: anY) desiredOutput: trainedOutput ].
	
	nbOfGood := 0.
	nbOfTries := 1000.
	nbOfTries timesRepeat: [ 
		anX := (r nextInt: 50) - 25.
		anY := (r nextInt: 50)- 25.
		realOutput := (f value: anX) >= anY ifTrue: [1] ifFalse: [0].
		((p feed: { anX . anY }) - realOutput) abs < 0.2
			ifTrue: [ nbOfGood := nbOfGood + 1 ].
	].
	learningCurve add: { nbOfTrained . (nbOfGood / nbOfTries) }.
].

g := RTGrapher new.
d := RTData new.
d noDot.
d connectColor: Color blue.
d points: learningCurve.
d x: #first.
d y: #second.
g add: d.
g axisY title: 'Precision'.
g axisX noDecimal; title: 'Training iteration'.
g
~~~~~~~

![Precision of the dot classification task.](02-Perceptron/figures/perceptronPrecision.png){#fig:perceptronPrecision width=400px}

The script produces a curve with the precision on the Y-axis and the number of trainings on the X-axis (Figure @fig:perceptronPrecision). We see that the perceptron started with a rather poor performance, around 0.25. However, it quickly steps up to reach a precision close to 1.0.


## Historical Perspective

Expressing a computation in terms of artificial neurons was first thought in 1943, by Warren S. McCulloch and Walter Pitts in their seminal article *A logical calculus of the ideas immanent in nervous activity*. This paper has a significant impact in the field of artificial intelligence. It is interesting to realize the knowledge we had about neurons at that time. The perceptron model presented originate from this seminal paper.

## What have we seen in this chapter
This chapter covers the following topics:

* _Providing the concept of perceptron._ We have seen what is a perceptron. The perceptron is an essential abstraction on which we will built on top of in the next chapters.
* _A step-by-step guide on programming with Pharo._ While we implemented the perceptron, we have sketched out how programming happens in Pharo. This chapter is by no means an introduction to Pharo. Instead, it is an overview on how to use the Pharo programming environment. In particular, we have seen how to write code using the system browser and how to run code using the playground. These two tools are fundamental and deserve to be well understood. 
* _Implemented a perceptron._ We implemented and tested the perceptron.  Testing is important as it is a way to formalize the behavior we wish for the perceptron.
* _Making a perceptron learn._ We have seen a rudimentary way to make a perceptron learn. It is rather simple, but, as we will see in the future chapters, the very same technique can bring us very far. 

## Exercises

- We have seen how the perceptron can be used to implement some logical gates. In particular, we have seen how the AND, OR, and NOT can be implemented. What about the XOR gate? Can you train a perceptron to learn the XOR behavior? (As a remainder, we have `0 XOR 0 = 0`, `0 XOR 1 = 1`, `1 XOR 0 = 1`, and `1 XOR 1 = 0`). 
- We have seen how five perceptrons may be combined to form a digital comparator. Do you think you can train the combination of these five prceptrons as a whole to learn the behavior of the digital comparator? 

## Further reading about Pharo
Pharo is a wonderful programming language and programming environment. This first chapter may be used as an introduction to programming with Pharo. However, it is highly recommended to seek for complementary reading in order to feel confortable with Pharo. In particular, the _Pharo by example_ book is an excellente introduction to learn and master Pharo. The website *http://books.pharo.org* contains a free copy of the book. Check it out!


