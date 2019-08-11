
# Perceptron

All major animal groups have brains made of neurons. 
A neuron is a specialized cell that transmits electrochemical stimulation using an axon to other neurons. A neuron receives this nerve impulse via a *dendrite*. Since the early age of computers, scientists have tried to produce a computational model of a neuron. The perceptron was one of the first models to mimic the behavior of a neuron.

This chapter plays two essential roles in the book. First, it presents the _perceptron_, a fundamental model on which neural networks are based on. Second, it also provides a gentle introduction to the Pharo programming language. The chapter builds a simple perceptron model in Pharo.

## Perceptron

A perceptron is a kind of artificial neuron that models the behavior of a biological neuron.
A perceptron is a machine that produces an output for a provided set of input values. Figure @fig:perceptron gives a visual representation of a perceptron.

A perceptron accepts 1, 2, or more numerical values as inputs. It produces a numerical value as output (result of a simple equation that we will shortly see). A perceptron operates on numbers, which means that the inputs and the output are numerical values (_e.g.,_ integers or floating point values).

![Representing the perceptron.](02-Perceptron/figures/perceptron.png){#fig:perceptron width=200px}

Figure @fig:perceptron depicts a perceptron. A perceptron is usually represented as a circle with some inputs and one output. Inputs are represented as incoming arrows located on the left of the central circle and the output as an outgoing arrow on the right of it. In the figure, our perceptron has three inputs, noted _x1_, _x2_, and _x3_.

Not all inputs have the same importance for the perceptron. For example, an input may be more important than other inputs. Relevance of an input is expressed using a weight (also a numerical value) associated to that input. In our figure, the input _x1_ is associated to the weight _w1_, _x2_ to the weight _w2_, and _x3_ to _w3_. Different relevances of some inputs allows the network to model a specialized behavior. For example, for an image recognition task, pixels located at the border of the picture have usually less relevance than the pixels located in the middle. Weights associated to the inputs corresponding to the border pixels will therefore be rather close to zero.

In addition to the weighted input value, a perceptron requires a _bias_, a numerical value acting as a threshold. We denote the bias as _b_.

A perceptron receives a stimulus as input and responds to that stimulus by producing an output value. The output obeys a very simple rule: if the sum of the weighted inputs is above a particular given value, then the perceptron fires 1, else it fires 0. Programmatically, we first compute the sum of the weighted inputs and the bias. If this sum is strictly above 0, then the perceptron produces 1, else it produces 0.

Formally, based on the perceptron given in Figure @fig:perceptron, we write $z = x1 * w1 + x2 * w2 + x3 * w3 + b$. In the general case, we write $z = \sum_i{x_i * w_i}~ + b$. The variable $i$ ranges over all the inputs of the perceptron. If $z > 0$, then the perceptron produces 1, else if $z \leq 0$ it produces 0.

In the next section, we will implement a perceptron model that is both extensible and maintainable. You may wonder, "What is the big deal about this?" After all, the perceptron model may be implemented in a few lines of code. However, implementing the perceptron functionality is just a fraction of the job. Creating a perceptron model that is testable, well tested, and extensible is the real value of this chapter. Soon we will see how we can train a network of artificial neurons, and it is important to build this network framework on a solid base.

## Implementing the perceptron

In this section, we will put our hands to work and implement the perceptron model in the Pharo programming language. We will produce an object-oriented implementation of the model. We will implement a class `Neuron` in a package called `NeuralNetwork`. Our class will have a method called `feed` which will be used to compute two values, $z$ and the perceptron output.

Our code will be contained in a package. To create a new package, we first need to open a _system browser_ by selecting the corresponding entry in the Pharo menu. The system browser is an essential tool in Pharo. It allows one to read and write code. Most of the programming activity in Pharo typically happens in a system browser. 

Figure @fig:systemBrowser shows a system browser, which is composed of five different parts. The top part is composed of four lists. The left-most list gives the available and ready-to-be-used packages. In the figure, the names `Announcement`, `AST-Core` and `Alien` are examples of packages. The `Announcement` package is selected in the figure. 

The second list gives the classes that belong to the selected package. Many classes are part of the `Announcement` package, including the classes called `Announcement`, `AnnouncementSet`, and `Announcer`.

The third list shows the method categories of the selected class. Method categories sort methods into logical groups to clarify their purpose and make them easier to find. Think of them as a kind of package for methods. Since no class is selected in the figure, no method category is listed.

The right-most list shows the methods of the selected class, filtered by the selected method category if any. Since no class is selected, no methods are listed.
The bottom part of a system browser displays source code, which is one of the following:

|Selection|Code Displayed|
|---------|--------------|
|Method|Selected method source code|
|Class|Selected class definition|
|None|New class Template|

![The Pharo system browser.](02-Perceptron/figures/systemBrowser.png){#fig:systemBrowser}

Right-click on the left-most top list to create a new package, named `NeuralNetwork`. This package will contain most of the code we will write in this first part of the book. 

Select the package `NeuralNetwork` you have just created and modify the template in the bottom pane as follows:

```Smalltalk
Object subclass: #Neuron
	instanceVariableNames: 'weights bias'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

You then need to compile the code by "accepting" the source code. Right click on the text pane and select the `Accept` option. The class `Neuron` defines two instance variables, `weights` and `bias`. Note that we do not need to have variables for the inputs and output values. These values will be provided as message arguments and returned values. 
We need to add some methods to define the logic of our perceptron.
In particular, we need to compute the intermediate $z$ and the output values. Let's first focus on the `weights` variable. We will define two methods to write a value in that variable and another one to read from it.

You may wonder why we define a class `Neuron` and not `Perceptron`. In the next chapter we will extend our class `Neuron` by turning it into an open abstraction for an artificial neuron. Our `Neuron` class is therefore a placeholder for improvements we will make in the subsequent chapters. In this chapter we consider a perceptron, but in the coming chapter we will move toward an abstract neuron implementation. The name `Neuron` is better suited therefore.

Here is the code of the `weights:` method defined in the class `Neuron`:

```Smalltalk
Neuron>>weights: someWeightsAsNumbers
	weights := someWeightsAsNumbers
```

To define this method, you need to select the `Neuron` class in the class panel (second top list panel). Then, write the code given above *without* `Neuron>>`, which is often prepended in documentation to indicate the class that should host the method, but it is not needed in the browser because the class is selected in the top pane. Figure @fig:systemBrowserAndMethodWeight illustrates this. Next, you should accept the code (again by right clicking on the `Accept` menu item). In  Pharo jargon, accepting a method has the effect to actually compile it (_i.e.,_ using the Pharo compiler to translate the Pharo source code into some bytecodes understandable by the Pharo virtual machine). Once compiled, a method may be executed. The code defines the method named `weights:` which accepts one argument, provided as a variable named `someWeightsAsNumbers`. 

The expression `weights := someWeightsAsNumbers` assigns the value `someWeightsAsNumbers` to the variable `weights`.

![The `weights:` method of the `Neuron` class.](02-Perceptron/figures/systemBrowserAndMethodWeight.png){#fig:systemBrowserAndMethodWeight}

Your system browser should now look like Figure @fig:systemBrowserAndMethodWeight.
The method `weights:` writes a value to the variable `weights`. Its sibling method that returns its value is:

```Smalltalk
Neuron>>weights
	"Return the weights of the neuron."
	^ weights
```

The character `^` returns the value of an expression, in this case the value of the variable `weights`.



Similarly, we need to define methods to assign a value to the `bias` variable and to read its content. The method `bias:` is defined as follow:

```Smalltalk
Neuron>>bias: aNumber
	bias := aNumber
```

And reading the variable `bias` is provided by:

```Smalltalk
Neuron>>bias
	^ bias
```

So far, we have defined the class `Neuron` which contains two variables (`weights` and `bias`), and 4 methods (`weights:`, `weights`, `bias:`, and `bias`). We now need to define the logic of our perceptron by applying a set of input values and obtaining the output value. Let's add a `feed:` method that does exactly this small computation:

```Smalltalk
Neuron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
	^ z > 0 ifTrue: [ 1 ] ifFalse: [ 0 ].
```

The `feed:` method simply translates the mathematical perceptron activation formula previously discussed into the Pharo programming language.
The expression `inputs with: weights collect: [ :x :w | x * w ]` transforms the `inputs` and `weights` collections using the supplied function. Consider the following example:

```Smalltalk
#(1 2 3) with: #(10 20 30) collect: [ :a :b | a + b ]
```

The above expression evaluates to `#(11 22 33)`. Syntactically, the expression means that the literal value `#(1 2 3)` receives a message called `with:collect:`, with two arguments, the literal array `#(10 20 30)` and the block `[ :a :b | a + b ]`.
You can verify the value of that expression by opening a playground (accessible from the main Pharo menu). A playground is a kind of command terminal for Pharo (_e.g.,_ xterm in the Unix World). Figure @fig:playground illustrates the evaluation of the expression given above (evaluated either by choosing "Print It" from the right click menu, or using the adequate shortcut (Cmd + p on OSX or Alt + p on other operating systems).

![The Playground.](02-Perceptron/figures/playground.png){#fig:playground width=400px}

We can now play a little bit with a perceptron. Evaluate the following code in the playground we just opened:

```Smalltalk
p := Neuron new.
p weights: #(1 2).
p bias: -2.
p feed: #(5 2)
```

This piece of code evaluates to `1` (since `(5*1 + 2*2)-2` equals to `7`, which is greater than `0`).

![Evaluating the perceptron.](02-Perceptron/figures/playgroundAndPerceptron.png){#fig:playgroundAndPerceptron width=400px}

## Testing our code

Now it is time to talk about testing. Testing is an essential activity whenever we write code using agile methodologies. Testing is about raising the confidence that the code we write does what it is supposed to do.

Although this book is not about writing large software artifacts, we _do_ write source code. And making sure that this code can be tested in an automatic fashion significantly improves the quality of our work. More importantly, most code is read far more often than it is written. Testing helps us produce maintainable and adaptable code. Throughout this book, we will improve our codebase. It is therefore very important to make sure that our improvements do not break existing functionalities. 

For example, above we defined a perceptron, and we informally tested it in a playground. This informal test costs us a few keystrokes and a little bit of time. What if we can repeat this test each time we modify our definition of perceptron? This is exactly what _unit testing_ is all about. 

We now define a class called `PerceptronTest`:

```Smalltalk
TestCase subclass: #PerceptronTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```


The class `TestCase` belongs to the built-in Pharo codebase. Subclassing it is the first step to create a unit test. Tests can now be added to our `PerceptronTest`. Define the following method:

```Smalltalk
PerceptronTest>>testSmallExample
	| p result |
	p := Neuron new.
	p weights: #(1 2).
	p bias: -2.
	result := p feed: #(5 2).
	self assert: result equals: 1.
```

The method `testSmallExample` tests that the code snippet we previously gave returns the value `1`. 
The test can be run by clicking on the gray circle located next to the method name. The gray circle turns green to indicate the test passes (Figure @fig:testingPerceptron01).

![Testing the perceptron.](02-Perceptron/figures/testingPerceptron01.png){#fig:testingPerceptron01}

A green test means that no assertion failed and no error was raised during the test execution. The method `testSmallExample` sends the message `assert:equals:` which tests whether the first argument equals the second argument. 

*EXERCISE:* So far, we have only shallowly tested our perceptron. We can improve our tests in two ways:

- Expand `testSmallExample` by feeding our perceptron `p` with different values (_e.g.,_ `-2` and `2` gives `0` as result).
- Test our perceptron with different weights and biases.

In general, it is a very good practice to write a thorough suite of tests, even for a small component such as our `Neuron` class.


## Formulating logical expressions

A canonical example of using a perceptron is to express boolean logical gates. The idea is to have a perceptron with two inputs (each being a boolean value), and the result of the modeled logical gate as output. 

A little bit of arithmetic indicates that a perceptron with the weights `#(1 1)` and the bias `-1.5` formulates the AND logical gate. The AND gate may be represented as the following table:

A | B | A  AND  B 
--- | --- | :---:
0 | 0 | 0
0 | 1 | 0
1 | 0 | 0
1 | 1 | 1

We could therefore verify this with a new test method:

```Smalltalk
PerceptronTest>>testAND
	| p |
	p := Neuron new.
	p weights: #(1 1).
	p bias: -1.5.
	
	self assert: (p feed: #(0 0)) equals: 0.
	self assert: (p feed: #(0 1)) equals: 0.
	self assert: (p feed: #(1 0)) equals: 0.
	self assert: (p feed: #(1 1)) equals: 1.
```

Similarly, a perceptron can formulate the OR logical gate:

A | B | A OR B 
--- | --- | :---:
0 | 0 | 0
0 | 1 | 1
1 | 0 | 1
1 | 1 | 1

Consider the following test:

```Smalltalk
PerceptronTest>>testOR
	| p |
	p := Neuron new.
	p weights: #(1 1).
	p bias: -0.5.
	
	self assert: (p feed: #(0 0)) equals: 0.
	self assert: (p feed: #(0 1)) equals: 1.
	self assert: (p feed: #(1 0)) equals: 1.
	self assert: (p feed: #(1 1)) equals: 1.
```

Negating the weights and bias results in the negated logical gate:

```Smalltalk
PerceptronTest>>testNOR
	| p |
	p := Neuron new.
	p weights: #(-1 -1).
	p bias: 0.5.
	
	self assert: (p feed: #(0 0)) equals: 1.
	self assert: (p feed: #(0 1)) equals: 0.
	self assert: (p feed: #(1 0)) equals: 0.
	self assert: (p feed: #(1 1)) equals: 0.
```

So far we have built perceptrons with two inputs. The number of input values has to be the same than the number of weights. Therefore, if only one weight is provided, only one input is required. Consider the NOT logical gate:

```Smalltalk
PerceptronTest>>testNOT
	| p |
	p := Neuron new.
	p weights: #(-1).
	p bias: 0.5.
	
	self assert: (p feed: #(1)) equals: 0.
	self assert: (p feed: #(0)) equals: 1.
```

## Handling error

In `testNOT`, we have defined a perceptron with only one weight. The array provided when calling `feed:` _must_ have only one entry. But what would happen if we had two entries instead of one? An error should occur as we are wrongly using the (small) API we have defined.

We should also test this behavior to make sure errors are properly generated. Define the following test:

```Smalltalk
PerceptronTest>>testWrongFeeding
	| p |
	p := Neuron new.
	p weights: #(-1).
	p bias: 0.5.
	
	self should: [ p feed: #(1 1) ] raise: Error
```

The test `testWrongFeeding` passes only if the expression `p feed: #(1 1)` raises an error, which it does. 

![Running our tests.](02-Perceptron/figures/runningTests.png){#fig:runningTests}

Until now, we have defined the class `Neuron` with five methods, and the unit test `PerceptronTest` with six test methods. All the tests can be run by pressing the circle next to the unit test name, `PerceptronTest` (Figure @fig:runningTests).

It is important to emphasize that rigorously testing our code, which also involves verifying that errors are properly handled, is important when implementing a neural network from scratch. Facing errors due to mismatched size of inputs and weights is unfortunately too frequent to be lax on that front. 

## Combining perceptrons

Until now, we have defined the AND, NOR, NOT, and OR logical gates. Logical gates become interesting when combined. A digital comparator circuit is a combination of two NOT gates with two AND gates and one NOR gate. The overall combination is useful to compare two values, A and B. We have three possible outcomes:

- A is greater than B
- A is equal to B
- A is less than B

We can therefore model our circuit with two inputs and three outputs. The following table summarizes the circuit:

A | B | A < B | A = B | A > B
--- | --- | :---: | :---: | :---:
0 | 0 | 0 | 1 | 0
0 | 1 | 1 | 0 | 0
1 | 0 | 0 | 0 | 1
1 | 1 | 0 | 1 | 0


![Digital Comparator Circuit.](02-Perceptron/figures/digitalComparator.png){#fig:digitalComparator width=400px}

Figure @fig:digitalComparator illustrates the circuit. Three different logical gates are necessary: AND, NOT, and NOR. We then need to make the connection between these gates. As we previously did, some tests will drive our effort. The method `digitalComparator:`, defined in our unit test for convenience, models the digital comparator circuit:

```Smalltalk
PerceptronTest>>digitalComparator: inputs
    "Return an array of three elements"
    | not and nor a b aGb aEb aLb notA notB |
    a := inputs first.
    b := inputs second.

    and := Neuron new weights: #(1 1); bias: -1.5.
    not := Neuron new weights: #(-1); bias: 0.5.
    nor := Neuron new weights: #(-1 -1); bias: 0.5. 

    notA := not feed: { a }. 
    notB := not feed: { b }.
    
    aLb := and feed: { notA . b }.
    aGb := and feed: { a . notB }.
    aEb := nor feed: { aGb . aLb }.
    ^ { aGb . aEb . aLb }
```

The method accepts a set of inputs as its argument. We begin by extracting the first and second elements of these inputs and assign them to the temporary variables `a` and `b`. 

Next, we create our three logical gates as perceptrons. Then we wire them together using the variables `notA`, `notB`, `aGb` (standing for `a` greater than `b`), `aLb` (`a` less than `b`), and `aEb` (`a` equals `b`).

We then compute `notA` and `notB`. Here, we use an alternative syntax to define an array. The expression `{ A }` creates an array with the object referenced by `A`. The elements of this array syntax will be evaluated at run time, unlike the `#(...)` notation, which is evaluated at compile time. Therefore, for "literal" objects like numbers always use `#(...)` (_e.g.,_ `#(1 -1)`). To create an array which contains results of expressions always use `{...}`. Note that technically we can also write numbers using the `{...}` syntax (_e.g.,_ `{1 . -1}`), but this is rarely done due to the performance penalty of runtime evaluation without any advantage. It is important to keep these two notations in mind as we will use them heavily throughout the book.

The method `digitalComparator:` returns the result of the circuit evaluation as an array. We can test it using the following test method:

```Smalltalk
PerceptronTest>>testDigitalComparator
	self assert: (self digitalComparator: #(0 0)) equals: #(0 1 0).
	self assert: (self digitalComparator: #(0 1)) equals: #(0 0 1).
	self assert: (self digitalComparator: #(1 0)) equals: #(1 0 0).
	self assert: (self digitalComparator: #(1 1)) equals: #(0 1 0).
```

The digital comparator circuit example shows how perceptrons may be "manually" combined. 
The overall behavior is divided into parts, each referenced with a variable. These variables must then be combined to express the logical flow (_e.g.,_ the variable `notA` must be computed before computing an output). 

## Training a Perceptron

So far, we have used perceptron with a particular set of weights and bias. For example, we have defined the AND logical gate with the value 1 for its two weights and a bias of -1.5. Consider the following exercise: manually compute the weights and bias to model the NAND logical gate (_e.g.,_ we recall that table for NAND is `#( #(0 0 1) #(0 1 1) #(1 0 1) #(1 1 0))`. Doing so require a moment to compute some simple arithmetics. Imagine a perceptron taking thousands of inputs. Identifying some adequate values for the weights and bias cannot be realistically done by hand. Well... this is exactly what training a perceptron is about: finding adequate weights and bias to make the perceptron behave to solve a particular problem.

Learning typically involves a set of input examples with some known outputs. The learning process assesses how good the artificial neuron is against the desired output. In particular, as defined by Frank Rosenblatt in the late 1950s, each weight of the perceptron is modified by an amount that is proportional to (i) the product of the input and (ii) the difference between the real output and the desired output. Learning in neural networks means adjusting the weights and the bias in order to make the output close to the set of training examples. 

The way a perceptron learns simply follows the rules: 

$$w_i(t+1) = w_i(t) + (d - z) * x_i * \alpha$$
$$b(t+1) = b(t) + (d - z) * \alpha$$

in which:

- $i$ is the weight index
- $w_i(t)$ is the weight $i$ at a given time $t$
- $b(t)$ is the bias at a given time $t$
- $d$ is the desired value
- $z$ is the actual output of the perceptron
- $x_i$ corresponds to the provided input at index $i$
- $\alpha$ is the learning rate

We have $w_i(0)$ equals to a random number, usually within a narrow range centered on 0. The two equations given above can be translated into the following pseudocode:

~~~~~~~~~
diff = desiredOutput - realOutput
alpha = 0.1
For all N:
   weightN = weightN + (alpha * inputN * diff)
bias = bias + (alpha * diff)
~~~~~~~~~

This pseudocode can be written in Pharo with the method `train:desiredOutput:`. But before that, we need to slightly adjust the definition of the class `Neuron` by adding the instance variable `learningRate`. The definition is:

```Smalltalk
Object subclass: #Neuron
	instanceVariableNames: 'weights bias learningRate'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We can also provide the necessary methods to modify the variable `learningRate`:

```Smalltalk
Neuron>>learningRate: aNumber
	"Set the learning rate of the neuron"
	learningRate := aNumber
```

To obtain the value of the variable:

```Smalltalk
Neuron>>learningRate
	"Return the learning rate of the neuron"
	^ learningRate
```

The variable can be initialized in the constructor

```Smalltalk
Neuron>>initialize
	super initialize.
	learningRate := 0.1
```

We can now define the method `train:desiredOutput:` to make a perceptron learn.

```Smalltalk
Neuron>>train: inputs desiredOutput: desiredOutput
	| theError output newWeight |
	output := self feed: inputs.
	theError := desiredOutput - output.
	inputs
		withIndexDo: [ :anInput :index | 
			newWeight := (weights at: index) + (learningRate * theError * anInput).
			weights at: index put: newWeight ].
	bias := bias + (learningRate * theError)
```

Before doing any adjustment of the weights and bias, we need to know how well the perceptron evaluates the set of inputs. We therefore need to evaluate the perceptron with the argument `inputs`. The result is assigned to the variable `output`. The variable `theError` represents the difference between the desired output and the actual output. We also need to decide how fast the perceptron is supposed to learn. The `learningRate` value ranges between `0.0` and `1.0`. We arbitrarily picked the value `0.1`. 


Let's see how to use the training in practice. Consider the perceptron `p` in the following example:

```Smalltalk
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
p feed: #(0 1).
```

You can evaluate the code above in a playground. We have `p feed: #(0 1)` is equal to `1`. What if we wish the perceptron to actually output `0` for the input `#(0 1)`? We would need to train `p`. As we said, this training will adjust the weights and the bias. Let's try the following:

```Smalltalk
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
p train: #(0 1) desiredOutput: 0.
p feed: #(0 1).
```

Evaluating this expression still outputs `1`. Huh?! Were we not supposed to train our perceptron? A perceptron learns slowly. We therefore actually need to train the perceptron a few times on the desired output. We can repeatedly train the perceptron as follows:

```Smalltalk
p := Neuron new.
p weights: #(-1 -1).
p bias: 2.
10 timesRepeat: [ p train: #(0 1) desiredOutput: 0 ].
p feed: #(0 1).
```

Evaluating the code given above produces `0`, as we were hoping for (Figure @fig:playgroundWithLearningPerceptron). Our perceptron has learned!

![Teaching a perceptron.](02-Perceptron/figures/playgroundWithLearningPerceptron.png){#fig:playgroundWithLearningPerceptron width=400px}

We can now train a perceptron to actually learn how to express the logical gates. Consider the following `testTrainingOR`:

```Smalltalk
PerceptronTest>>testTrainingOR
	| p |
	p := Neuron new.
	p weights: #(-1 -1).
	p bias: 2.
	
	40 timesRepeat: [ 
		p train: #(0 0) desiredOutput: 0.
		p train: #(0 1) desiredOutput: 1.
		p train: #(1 0) desiredOutput: 1.
		p train: #(1 1) desiredOutput: 1.
	].
	
	self assert: (p feed: #(0 0)) equals: 0.
	self assert: (p feed: #(0 1)) equals: 1.
	self assert: (p feed: #(1 0)) equals: 1.
	self assert: (p feed: #(1 1)) equals: 1.
```

The method `testTrainingOR` first creates a perceptron with some arbitrary weights and bias. We successfully train it with the four possible combinations of the OR logical gate. After the training, we verify whether the perceptron has properly learned.

In `testTrainingOR`, we train the perceptron 40 times on the complete set of examples. Training a perceptron (or a large neural network) with the complete set of examples is called an _epoch_. So, in our example, we train `p` with 40 epochs. The epoch is the unit of training.

Similarly, we can define a test that train a perceptron to model the NOT logical gate:

```Smalltalk
PerceptronTest>>testTrainingNOT
	| p |
	p := Neuron new.
	p weights: #(-1).
	p bias: 2.
	
	40 timesRepeat: [ 
		p train: #(0) desiredOutput: 1.
		p train: #(1) desiredOutput: 0.
	].
	
	self assert: (p feed: #(0)) equals: 1.
	self assert: (p feed: #(1)) equals: 0.
```


*EXERCISE:*

- What is the necessary minimum number of epochs to train `p`? Try to reduce the number of epochs run the test to see if it still passes.
- We have shown how to train a perceptron to learn the OR logical gate. Write a method `testTrainingNOR` and `testTrainingAND` for the other gates we have seen.
- How does the value of the `learningRate` impact the minimum number of epochs for the training?

## Drawing graphs

Drawing graphs is often necessary to monitor progresses made by the network. We will use the Roassal visualization engine to visualize such evolution. Roassal offers the Grapher API, dedicated to draw graphs. You can load Roassal by executing the following in a playground:

```Smalltalk
Metacello new
    baseline: 'Roassal2';
    repository: 'github://ObjectProfile/Roassal2/src';
    load.
```

The coming section uses Roassal. Make sure you have it loaded, else part of the code given below will not work or even compile. More information about Roassal may be found on [http://AgileVisualization.com](http://AgileVisualization.com) and detailed loading instruction may be found on [https://github.com/ObjectProfile/Roassal2](https://github.com/ObjectProfile/Roassal2).


Here is an example of drawing a simple graph (Figure @fig:exampleGraph):

```Smalltalk
g := RTGrapher new.
d := RTData new.
d connectColor: Color blue.
d points: (1 to: 100).
d y: [ :x | (x / 3.14) sin  ].
g add: d.
g
```

![Example of a graph.](02-Perceptron/figures/exampleGraph.png){#fig:exampleGraph}

We will make an intense use of graphs along the book. More information about drawing graph can be found in the examples of Roassal.


## Predicting side of a 2D point

We will now see a new application of the perceptron. A perceptron can be used to classify data and make some predictions. We will pick a simple classification problem. Consider the following:

- A space composed of red and blue points
- A straight line divides the red points from the blue points

Consider the following interaction between two (real) people, a teacher and a student. The goal of the teacher is to let the student infer where is the straight separation line between the blue and the red points. First, the teacher can give an arbitrary number of examples. Each example is given to the student as a location and a color. After a few examples, the students is able to guess the color of a random location. Intuitively, more examples the teacher will give to the student, more the student will be likely to correctly predict the color of a location.

Some questions arise:

- Can we teach a perceptron to correctly assign the color of a point?
- How many example points do we need to train the perceptron in order to make good prediction?

Let's pick a linear function, such as $f(x) = -2x - 3$. A given point $(x, y)$ is colored in red if $y > f(x)$, else it is blue. Consider the following script:

```Smalltalk
somePoints := OrderedCollection new.
500 timesRepeat: [ 
	somePoints add: {(50 atRandom - 25) . (50 atRandom - 25)}
].

f := [ :x | (-2 * x) - 3 ].

"We use the Roassal Grapher engine to plots our points"
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
```

Inspecting this code snippet produces a graph with 500 colored dots (Figure @fig:simpleLine).

![Classifying dots along a line.](02-Perceptron/figures/simpleLine.png){#fig:simpleLine}

The script begins by defining a set of 500 points, ranging within a squared area of 50 (from -25 to +25). The expression `50 atRandom` returns a random number between 1 and 50. The expression `{(50 atRandom - 25) . (50 atRandom - 25)}` creates an array with two random values in it. Each point is represented as an array of two numbers. Our 500 points are kept in a collection, an instance of the class `OrderedCollection`.

We assign to the variable `f` a block representing our function $f(x)$, written in the Pharo syntax. A block may be evaluated with the message `value:`. For example, we have `f value: 3` that returns `-9` and `f value: -2` that returns `1`.

The remainder of the script uses Grapher to plot the points. A point `p` is red if `p second` is greater than `f value: p first`, else it is blue. The expression `Color red trans` evaluates to a transparent red color. 

We can add the actual line defined by `f` to our graph. Consider the small revision (Figure @fig:simpleLine2):

```Smalltalk
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
```

![Adding a separation line.](02-Perceptron/figures/simpleLine2.png){#fig:simpleLine2}



We will now add a perceptron to our script and see how well it guesses on which side of the line a point falls. Consider the following script (Figure @fig:dotColorPrediction):

```Smalltalk
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
```

![Predicting the color of the dot.](02-Perceptron/figures/runningThePerceptron.png){#fig:dotColorPrediction}

Figure @fig:dotColorPrediction gives the result of the prediction. We can see that some red dots are not properly classified: some red dots at located on the right of the line. However, in general, the precision is good since most of the dots have the right color.

As in a previous script, the script begins with the definition of the block function `f`. It then creates a perceptron with some arbitrary weights and bias. Subsequently, a random number generator is created. In our previous scripts, to obtain a random value between 1 and 50, we simply wrote `50 atRandom`. Using a random number generator, we need to write:

```Smalltalk
r := Random new seed: 42.
r nextInt: 50.
```

Why is this? First of all, being able to generate random numbers is necessary in all stochastic approaches, including neural networks. Although randomness is very important, we usually do not want to let such a random value create situations that cannot be reproduced. Imagine that our code behaves erratically, likely due to a random value. How can we track down the anomaly in our code? If we have truly random numbers, it means that executing the same piece of code twice may produce (even slightly) different behaviors. It may therefore be complicated to properly test. Instead, we will use a random generator with a known seed to produce a known sequence of random numbers. Consider the expression:

```Smalltalk
(1 to: 5) collect: [ :i | 50 atRandom ]
```

Each time you evaluate this expression, you will obtain a _new_ sequence of 5 random numbers. Using a generator you have:

```Smalltalk
r := Random new seed: 42.
(1 to: 5) collect: [ :i | r nextInt: 50 ]
```

Evaluating this small script several times always produces the same sequence. This is the key to have reproducible and deterministic behavior. In the remainder of the book, we will frequently use random number generators.

Our script then trains a perceptron with 500 points. Next, we create 2,000 test points, which will be displayed on the screen using Grapher. We wrote the condition `(p feed: point) > 0.5` to color a point as red. We could instead have `(p feed: point) = 1`, however in an upcoming chapter we will replace the perceptron with another kind of artificial neuron, which will not exactly produce the value 1.

We see that the area of blue and red points goes very close to the straight line. This means that our perceptron is able to classify points with a relatively good accuracy. 

What if we reduce the number of training of our perceptron? You can try this by changing the value `500` by, let's say, `100`. What is the result? The perceptron does not classify points as accurately as with 500 trainings. This follows the intuition we elaborated when we first discussed training. In general, the more training a perceptron has, the more accurate it will be (however, this is not always true with neural networks, as we will see later on).

*EXERCISE:* Reduce the number of times the perceptron is trained. Verify that varying the value `500` to lower values leads to some errors by the perceptron, illustrated as a mismatch between the red line and the area of colored points. 


## Measuring the precision

We have seen that the accuracy of a perceptron in classifying points is very dependent on the number of times we train it. How much training do we need to have acceptable precision? Keeping track of the precision and the training is essential to see how good our system is at classification.

Consider the script:

```Smalltalk
learningCurve := OrderedCollection new.
f := [ :x | (-2 * x) - 3 ].

0 to: 2000 by: 10 do: [ :nbOfTrained |
	r := Random new seed: 42.
	p := Neuron new.
	p weights: #(1 2).
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
```

![Precision of the dot classification task.](02-Perceptron/figures/perceptronPrecision.png){#fig:perceptronPrecision width=400px}

The script produces a curve with the precision on the Y-axis and the number of trainings on the X-axis (Figure @fig:perceptronPrecision). We see that the perceptron started with a rather poor performance, around 0.25. However, it quickly steps up to reach a precision close to 1.0. After a few epochs, our perceptron is able to guess the color of a dot with a good precision.


## Historical perspective

Expressing a computation in terms of artificial neurons was first explored in 1943, by Warren S. McCulloch and Walter Pitts in their seminal article *A logical calculus of the ideas immanent in nervous activity*. This paper had a significant impact in the field of artificial intelligence. It is interesting to read about the knowledge we had about biological neurons at that time. The perceptron model presented in this chapter originated from this seminal paper.

## Review
This chapter covered the following topics:

* _Providing the concept of a perceptron._ We have defined a perceptron, an essential abstraction on which we will build in upcoming chapters.
* _A step-by-step guide to programming with Pharo._ While we implemented the perceptron, we have sketched out how programming happens in Pharo. This chapter is by no means an introduction to Pharo. Instead, it is an example of how to use the Pharo programming environment. In particular, we have seen how to write code using the system browser and how to run code using the playground. These two tools are fundamental and deserve to be well understood. 
* _Implementing a perceptron._ We implemented and tested the perceptron.  Testing is important as it is a way to formalize the behavior we wish for the perceptron.
* _Making a perceptron learn._ We have seen a rudimentary way to make a perceptron learn. It is rather simple, but, as we will see in future chapters, the very same technique can bring us very far. 

## Exercises

- We have seen how the perceptron can be used to implement some logical gates. In particular, we have seen how the AND, OR, and NOT can be implemented. What about the XOR gate? Can you train a perceptron to learn the XOR behavior? (As a reminder, we have `0 XOR 0 = 0`, `0 XOR 1 = 1`, `1 XOR 0 = 1`, and `1 XOR 1 = 0`). 
- We have seen how five perceptrons may be combined to form a digital comparator. Do you think you can train the combination of these five perceptrons as a whole to learn the behavior of the digital comparator? 

## Further reading about Pharo
Pharo is a wonderful programming language and live, dynamic programming environment. This first chapter has given you a taste of programming with Pharo. However, it is highly recommended that you seek further material in order to feel truly comfortable with Pharo and learn what makes it powerful. In particular, the _Pharo by Example_ book is an excellent introduction to learn and master Pharo. The website *http://books.pharo.org* contains a free copy of the book as well as many others. Check it out!
