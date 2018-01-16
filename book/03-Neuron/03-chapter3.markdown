
# Artificial Neuron

In the previous chapter we have seen how a perceptron operates and how a simple learning algorithm can be implemented. However, the perceptron, as we have seen it, has some serious limitations, which will motivate us to formulate a more robust artificial neuron, called the sigmoid neuron.

## Limit of the Perceptron

A perceptron works well as an independent small machine. We have seen that we can compose a few perceptrons to express a complex behavior such as the digital comparator. We have also seen that a single perceptron can learn a behavior that is not too complex. However, there are two main restrictions with combining perceptrons:

- Perceptrons only output 0 or 1. In case we chain some perceptrons, using binary values significantly reduce the space we live in. Not everything in this work can be reduced as a set of 0 and 1.
- A chain of perceptrons cannot learn. In particular, a perceptron does not work for backpropagation, the most common learning algorithm for supervised learning using gradient descent. 

We have written that $z = w.x + b$, for which $w$ is a vector of weights, $b$ a vector of bias, and $x$ the input vector. We said that the output of perceptron is 1 if $z > 0$, else the output is 0. One important problem with the formulation of the perceptron is that a small variation of $z$ can produce a large variation of the output: the output can goes from 0 to 1, or to 1 from 0.

Learning algorithms that are commonly employed in neural networks require a very important property: a small variation of $z$ _must_ produce a small variation of the output. And the perceptron does not fulfill this since a small variation of $z$ can produce a large variation of the output. 

## Activation Function

One way to improve the learning ability of a perceptron is to structure the behavior of perceptron a bit. Let's introduce a function called $\sigma$. The perceptron behavior can therefore be summarized as: $\sigma(z) = 1$ if $z > 0$, else $\sigma(z) = 0$. 

By adding the $\sigma$ function, we are separating the computation of $w.x + b$ from the conditional. We call $\sigma$ the _activation function_. It describes the activation of the perceptron (_i.e.,_ when it fires 1) according to the value of $z$.

The activation function used by the perceptron is called the _step function_.

## The Sigmoid Neuron

We described the perceptron as a powerful machine with a limitation to learn when combined with other perceptrons. We will therefore adopt another activation function. Consider the function $\sigma(z)=\frac{1}{1+e^{-z}}$.

This function can be plotted as:

[SigmoidNeuron]: 03-Neuron/figures/sigmoid.png "Image Title"  {width = 200}
![SigmoidNeuron][SigmoidNeuron] 


This sigmoid function has several advantages:

- It is differentiable everywhere on the curve. Since its curve has no angle, we can easily draw a straight line for any value $z$ that indicates the slope of $\sigma(z)$. When plotted, $\sigma(z)$ is very smooth by having no angle, which is a very good property.
- Its derivative has some interesting properties, as we will see later.
- It is an acceptable mathematical representation of a biological neuron behavior. 

Another interesting aspect, is that the sigmoid function behave similarly than the step function for very small and very large $z$ values. As a consequence, a small increment in $z$ will produce a small variation of $\sigma(z)$. 

The training has to be slightly adjusted to take advantage of the fact that $\sigma(z)$ is derivable.

## Implementing the activation functions 

In the previous chapter we have defined the class `Neuron`. We will improve this class to accept an activation function. First, let's introduce a small class hierarchy for activation functions. 

Let's define the abstract class `ActivationFunction`:

```Smalltalk
Object subclass: #ActivationFunction
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

An activation function object has two main responsibility: computing (i) the activation value and (ii) the transfer derivative. This transfer derivative is essential for the backpropagation learning algorithm.

We define the following two abstract methods:
```Smalltalk
ActivationFunction>>eval: z
	^ self subclassResponsibility
```

```Smalltalk
ActivationFunction>>derivative: z
	^ self subclassResponsibility
```

We can now define the two activation functions. The sigmoid function can be defined as:

```Smalltalk
ActivationFunction subclass: #SigmoidAF
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

```Smalltalk
SigmoidAF>>>eval: z
	^ 1 / (1 + z negated exp)
```

```Smalltalk
SigmoidAF>>>derivative: z
	| t |
	t := self eval: z.
	^ t * (1 - t)
```

$\sigma(z)' = \sigma(z) * (1 - \sigma(z))$

The step function can be defined as:
```Smalltalk
ActivationFunction subclass: #StepAF
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

```Smalltalk
StepAF>>>eval: z
	^ (z > 0) ifTrue: [ 1 ] ifFalse: [ 0 ]
```

```Smalltalk
StepAF>>>derivative: z
	^ z
```


## Extending the neuron with the activation functions

We can now extend our definition of neuron to use an activation function. We can do so by adding a new instance variable `activationFunction` to `Neuron`:


```Smalltalk
Object subclass: #Neuron
	instanceVariableNames: 'weights bias activationFunction'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

Feeding has to be adapted:
```Smalltalk
Neuron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
	^ activationFunction eval: z
```

Similarly, the training has to consider the derivative:

```Smalltalk
Neuron>>train: inputs desiredOutput: desiredOutput
	| learningRate theError output delta |
	output := self feed: inputs.
	learningRate := 0.1.

	theError := desiredOutput - output.
	delta := theError * (activationFunction derivative: output).	

	inputs withIndexDo: [ :anInput :index | 
		weights at: index put: ((weights at: index) + (learningRate * delta * anInput)) ].

	bias := bias + (learningRate * delta)
```

We now need to initialize a neuron as being a sigmoid:

```Smalltalk
Neuron>>initialize
	super initialize.
	self sigmoid
```

We can also define the two utility methods:

```Smalltalk
Neuron>>sigmoid
	activationFunction := SigmoidAF new
```

```Smalltalk
Neuron>>step
	activationFunction := StepAF new
```


## Adapting the existing tests

If you run `PerceptronTest` you will see that several of the test fail. The reason is that a neuron is initialized with a sigmoid activation function. We therefore need to adapt each test method, by adding a call to `step`. For example, the method `testAND` has to be rewritten:

```Smalltalk
PerceptronTest>>testAND
    | p |
	p := Neuron new.
	p step. "<= new line"
    p weights: { 1 . 1 }.
    p bias: -1.5.
    
    self assert: (p feed: { 0 . 0 }) equals: 0.
    self assert: (p feed: { 0 . 1 }) equals: 0.
    self assert: (p feed: { 1 . 0 }) equals: 0.
    self assert: (p feed: { 1 . 1 }) equals: 1.
```

Adding the call to `step` make the neuron behaves as a perceptron. Omitting this line would instead use a sigmoid neuron, and the tests would fail since the output would not exactly be `0` or `1`.

*EXERCISE:* Adapt all the test methods of `PerceptronTest`.

## Testing the sigmoid neuron

Since the behavior of a sigmoid neuron is very similar to a perceptron, we will reuse some of the tests. Define the class `NeuronTest`:

```Smalltalk
TestCase subclass: #NeuronTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We can then train a neuron to learn some logical gates. The following method is very similar to what we have seen with the perceptron:

```Smalltalk
NeuronTest>>testTrainingAND
	| p |
	p := Neuron new.
	p weights: {-1 . -1}.
	p bias: 2.
	
	5000
		timesRepeat: [ 
			p train: {0 . 0} desiredOutput: 0.
			p train: {0 . 1} desiredOutput: 0.
			p train: {1 . 0} desiredOutput: 0.
			p train: {1 . 1} desiredOutput: 1 ].
		
	self assert: ((p feed: {0 . 0}) closeTo: 0 precision: 0.1).
	self assert: ((p feed: {0 . 1}) closeTo: 0 precision: 0.1).
	self assert: ((p feed: {1 . 0}) closeTo: 0 precision: 0.1).
	self assert: ((p feed: {1 . 1}) closeTo: 1 precision: 0.1).
```

There are two differences:
- The number of epochs is significantly increased. The reason is that the sigmoid neuron learns slower than the perceptron. 
- The result of feeding the neuron is compared using the call `closeTo:precision:`. 

Similarly we can train a sigmoid neuron to learn the OR behavior:
```Smalltalk
NeuronTest>>testTrainingOR
	| p |
	p := Neuron new.
	p weights: {-1 . -1}.
	p bias: 2.
	
	5000
		timesRepeat: [ 
			p train: {0 . 0} desiredOutput: 0.
			p train: {0 . 1} desiredOutput: 1.
			p train: {1 . 0} desiredOutput: 1.
			p train: {1 . 1} desiredOutput: 1 ].
		
	self assert: ((p feed: {0 . 0}) closeTo: 0 precision: 0.1).
	self assert: ((p feed: {0 . 1}) closeTo: 1 precision: 0.1).
	self assert: ((p feed: {1 . 0}) closeTo: 1 precision: 0.1).
	self assert: ((p feed: {1 . 1}) closeTo: 1 precision: 0.1).
```

## Sigmoid neuron is slower to learn

This chapter is based on some limitation of the perceptron to be composed with other perceptrons. This has motivated us to formulate the sigmoid neuron. We see one drawback of the sigmoid neuron: it is slower to learn than the perceptron. We are here making a bet, which is trading efficiency for flexibility. 


We can easily make the comparison between the sigmoid neuron and perceptron. Consider the following script:

```Smalltalk
learningCurve := OrderedCollection new.
0 to: 1000 do: [ :nbOfTrained |
    r := Random new seed: 42.
    p := Neuron new.
    p weights: {-1 . -1}.
    p bias: 2.

    nbOfTrained timesRepeat: [ 
       	p train: {0 . 0} desiredOutput: 0.
		p train: {0 . 1} desiredOutput: 0.
		p train: {1 . 0} desiredOutput: 0.
		p train: {1 . 1} desiredOutput: 1 ].
   
    res :=  ((p feed: {0 . 0}) - 0) abs + 
			((p feed: {0 . 1}) - 0) abs +
			((p feed: {1 . 0}) - 0) abs +
			((p feed: {1 . 1}) - 1) abs.
	 learningCurve add: res / 4.
    
].

g := RTGrapher new.
d := RTData new.
d noDot.
d connectColor: Color blue.
d points: learningCurve.
d y: #yourself.
g add: d.
g axisY title: 'Error'.
g axisX noDecimal; title: 'Epoch'.
g
```

[SigmoidLearningAND]: 03-Neuron/figures/SigmoidLearningAND.png "Image Title"  {width=200}
![SigmoidLearningAND][SigmoidLearningAND] 

Adding a line `p step` after the creation of the neuron will produce the error curve for the perceptron. 

## The derivative of the sigmoid function
We have $\sigma(x)=\frac{1}{1+e^{-x}}$.
So, we also have:

$\frac{d}{dx}\sigma(x)=\frac{d}{dx}~\frac{1}{1+e^{-x}}$

$=\frac{d}{dx} (1+e^{-z})^{-1}$

Since the derivative of $x^n$ is $nx^{n-1}$, we have 

$=-(1 + e^{-x})^{-2}(-e^{-x})$

By simplifying we have:

$=\frac{e^{-x}}{(1 + e^{-x})^{2}}$

$=\frac{1}{1 + e^{-x}} . \frac{e^{-x}}{1 + e^{-x}}$

$=\frac{1}{1 + e^{-x}} . \frac{(1 + e^{-x}) - 1}{1 + e^{-x}}$

$=\frac{1}{1 + e^{-x}} . (\frac{1 + e^{-x}}{1 + e^{-x}} - \frac{1}{1 + e^{-x}})$

$=\frac{1}{1 + e^{-x}} . (1 - \frac{1}{1 + e^{-x}})$

$=\sigma(x) . (1 - \sigma(x))$


## What have we have
This chapter covers the following topics:

- _Briefly discussed the limitation of the perceptron._ The perceptron cannot learn when combined with other perceptrons. Although we have not discussed this aspect further, you need to trust me for now. In the next chapter we will develop this further.
- _Definition of the sigmoid neuron._ The sigmoid neuron is an improvement of the perceptron since it can be combined with other sigmoid neurons and this combination can learn. In the next chapter we will detail the backpropagation algorithm, a central aspect when making a neural network learn.
- _Activation functions._ We have seen two activation functions, the step and sigmoid functions. Many other activation functions are around. We will develop activation functions later on in the book.

