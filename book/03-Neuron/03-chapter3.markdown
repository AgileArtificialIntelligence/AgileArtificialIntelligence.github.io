
# Artificial Neuron

In the previous chapter we have seen how a perceptron operates and how a simple learning algorithm can be implemented. However, the perceptron has some serious limitations, which will motivate us to formulate a more robust artificial neuron, called the sigmoid neuron.

This chapter uses Roassal to plot values. As seen in the previous chapter, Roassal may be loaded in Pharo by executing the following script in a playground:

```Smalltalk
Metacello new
    baseline: 'Roassal2';
    repository: 'github://ObjectProfile/Roassal2/src';
    load.
```

A complete description of Roassal may be found in the book Agile Visualization ([http://agilevisualization.com](http://agilevisualization.com)).

## Limit of the Perceptron

A perceptron works well as an independent small machine. We have seen that we can compose a few perceptrons to express a complex behavior such as the digital comparator. We have also seen that a single perceptron can learn a simple behavior. However, there are two main restrictions with combining perceptrons:

- _Only 0 or 1 as output_: The fact that a perceptron can have only two different output values, 0 or 1, seriously limits the kind of problem it can solve. In particular, when some perceptrons are chained, using binary values significantly reduce the space we live in. Not everything can be reduced as a set of 0 and 1 without leading to an explosion of perceptrons.
- _A chain of perceptrons cannot learn_: We have seen how to combine perceptrons, and we have seen how a single perceptron can learn. But, can a combination of perceptrons also learn? The answer is no. This is another consequence of having only two output values. An essential property of most common learning algorithms is to be able to express a smooth learning curve, which cannot be expressing using two different values. How can we tell if a perceptron is learning well, poorly, or not at all with only two different output values?

We have written that $z = w.x + b$, for which $w$ is a vector of weights, $b$ a vector of bias, and $x$ the input vector. We said that the output of perceptron is 1 if $z > 0$, else the output is 0. One important problem with the formulation of the perceptron is that a small variation of $z$ can produce a large variation of the output: the output can goes from 0 to 1, or from 1 to 0.

Learning algorithms that are commonly employed in neural networks require a very important property: a small variation of $z$ _must_ produce a small variation of the output. And the perceptron does not fulfill this since a small variation of $z$ can produce a large variation of the output. 

## Activation Function

Before mentioning a better way to improve the learning, it is important to decouple the perceptron logic. Let's introduce a function called $\sigma$ that takes as parameter the $z = w.x + b$ value. The perceptron behavior can therefore be written as: $\sigma(z) = 1$ if $z > 0$, else $\sigma(z) = 0$. 

By adding the $\sigma$ function, we are separating the computation of $w.x + b$ from the conditional. We call $\sigma$ the _activation function_. It describes the activation of the perceptron (_i.e.,_ when it fires 1) according to the value of $z$.

The activation function used by the perceptron is called the _step function_ and may be graphically represented as in Figure @fig:stepFunction.

![The step function](03-Neuron/figures/stepFunction.png){#fig:stepFunction width=400px}

Figure @fig:stepFunction is the result of executing the script:

```Smalltalk
g := RTGrapher new.
d := RTData new.
d connectColor: Color blue.
d noDot.
d points: (-7.0 to: 7.0 by: 0.01).
d x: #yourself.
d y: [ :x | x > 0 ifTrue: [ 1 ] ifFalse: [ 0 ] ].
g add: d.
g
```

You can recognize the step function provided to the `y:` instruction. Note that the function provided to `y:` refers to the input as $x$ while $z$ is provided to $\sigma$. This is an inoffensive renaming.

Consider a value $z = 0$. We therefore have $\sigma(z) = 0$. If we add $0.00001$, a small value, to $z$ then we have $\sigma(z) = 1$. A small value added to $z$ produces a large change in $\sigma(z)$, which goes from $0$ to $1$. The fact that a small change in $z$ produces a big change in $\sigma(z)$ is actually a serious problem: a chain of perceptron is not able to learn.

The step function is characterized for having a vertical step, which produces two angles in its curve. These angles are makes the step function as non-derivable, which is quite a problem as we will shortly see.

## The Sigmoid Neuron

We will express a new kind of artificial neuron, called the _sigmoid neuron_. The increment we are here making is to use a new activation function, called the _sigmoid function_. Consider the function $\sigma(z)=\frac{1}{1+e^{-z}}$, plotted in Figure @fig:sigmoidFunction.

![The sigmoid function](03-Neuron/figures/sigmoid.png){#fig:sigmoidFunction width=400px}

Figure @fig:sigmoidFunction is the result of executing the script:

```Smalltalk
g := RTGrapher new.
d := RTData new.
d connectColor: Color blue.
d noDot.
d points: (-7.0 to: 7.0 by: 0.01).
d x: #yourself.
d y: [ :x | 1 / (1 + (x negated exp)) ].
g add: d.
g
```

This sigmoid function has several advantages:

- It is differentiable everywhere on the curve, or said in other words, it has no vertical lines, and even better, no angle. We can easily draw a straight line for any value $z$ that indicates the slope of $\sigma(z)$. When plotted, $\sigma(z)$ is very smooth by having no angle, which is a very good property.
- Its derivative has some interesting properties, as we will see later.
- The sigmoid function behaves similarly than the step function for very small and very large $z$ values. 
- A small increment in $z$ produces a small variation of $\sigma(z)$, and as we have previously said, this is important for learning. 

We define a sigmoid neuron as a neuron having the sigmoid function as activation function. The sigmoid neuron is widely accepted as a mathematical representation of a biological neuron behavior. 

As we will later see, the training has to be slightly adjusted to take advantage of the fact that $\sigma(z)$ is derivable.

## Implementing the activation functions 

In the previous chapter we have defined the class `Neuron`. We will improve this class to accept an activation function. First, let's introduce a small class hierarchy for activation functions. 

The abstract class `ActivationFunction` may be defined as:

```Smalltalk
Object subclass: #ActivationFunction
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

An activation function object has the responsibily of computing two things: (i) the activation value and (ii) the transfer derivative. This transfer derivative is an essential piece of the backpropagation learning algorithm. Implementation of the backpropagation is given in this chapter, while the theoretical background is covered in Chapter 5.

We define the following two abstract methods. The method `eval:` computes the activation value:
```Smalltalk
ActivationFunction>>eval: z
	^ self subclassResponsibility
```
and the method `derivative:` computes the transfer derivative:

```Smalltalk
ActivationFunction>>derivative: output
	^ self subclassResponsibility
```

The two methods we have just defined are abstract methods, which means they are placeholder for subclasses of `ActivationFunction` to actually provide an adequate implementation of these methods.

We can now define the two activation functions, each as being a subclass of `ActivationFunction`. The sigmoid function may be defined as:

```Smalltalk
ActivationFunction subclass: #SigmoidAF
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We first implement the `eval:` function:

```Smalltalk
SigmoidAF>>eval: z
	^ 1 / (1 + z negated exp)
```

We then implement the `derivative:` method, which represents the mathematical derivative of `eval:`:

```Smalltalk
SigmoidAF>>derivative: output
	^ output * (1 - output)
```

Without entering into details, we have $\sigma(z)' = \sigma(z) * (1 - \sigma(z))$. We will come back on that point in Chapter 5.

Similarly, we can define the step function as follows:
```Smalltalk
ActivationFunction subclass: #StepAF
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'NeuralNetwork'
```

We implement the `eval:`
```Smalltalk
StepAF>>eval: z
	^ (z > 0) ifTrue: [ 1 ] ifFalse: [ 0 ]
```

We also need to implement the `derivative:`. We will simply make this method return the argument:

```Smalltalk
StepAF>>derivative: output
	^ 1
```

The formulation of the `derivative:` of the step function does not match the mathematical truth, which is 0 with an undefined value for $z = 0$. However, returning $1$ instead eases the implementation of the revised `Neuron` as we will see in the next section.


## Extending the neuron with the activation functions

We can now generalize the way an artificial neuron can learn from examples. Assume an example value $(x, d)$, in which $x$ is example input and $d$ is the example output. At the beginning, when providing the input $x = (x_1, ..., x_i, ..., x_N)$ to a sigmoid neuron, the output is likely to be different than $d$, a number between $0$ and $1$. This is not surprising since the weights and bias are randomly chosen. This is exactly why we are training the neuron with that example, to have the neuron output $d$ if $x$ is provided.


The learning mechanism may be summarized with the following rules:

$$\delta = (d - z) * \sigma'(z)$$
$$w_i(t+1) = w_i(t) + \delta * x_i * \alpha$$
$$b(t+1) = b(t) + \delta * \alpha$$


in which:

- $\delta$ is the difference between the desired output and the actual output of the neuron;
- $d$ is the example output, which is the desired value;
- $z$ is the actual output of the perceptron;
- $\sigma$ is the activation function (either the step or sigmoid function);
- $\sigma'$ is the derivative function of $\sigma$;
- $i$ is the weight index, which ranges from $1$ to $N$, the number of weights contained in the neuron;
- $w_i(t)$ is the weight $i$ at a given time $t$;
- $b(t)$ is the bias at a given time $t$;
- $x_i$ corresponds to the provided input at index $i$;
- $\alpha$ is the learning rate, a small positive value close to $0$.

With little or no training, the neuron will output a value $z$ which is very different from $d$. As a consequence, $\delta$ will also be large. With an adequate number of trainings, the $\delta$ should get close to $0$.

These equations will be explained in Chapter 5. For now, the most important is that they can be translated into the following pseudocode:

```
diff = desiredOutput - realOutput
delta = diff * derivative(realOutput)
alpha = 0.1
For all N:
   weightN = weightN + (alpha * inputN * delta)
bias = bias + (alpha * diff)
```

We are here assuming that the neuron has $N$ inputs, and therefore $N$ weights.
We can now extend our definition of neuron to use an activation function. We can do so by adding a new instance variable `activationFunction` to the`Neuron` class:


```Smalltalk
Object subclass: #Neuron
	instanceVariableNames: 'weights bias learningRate activationFunction'
	classVariableNames: ''
	package: 'NeuralNetwork'
```

The variable `learningRate` must be accessed from outside:

```Smalltalk
Neuron>>learningRate: aLearningRateAsFloat
	"Set the learning rate of the neuron. The argument should be a small floating value. For example, 0.01"
	learningRate := aLearningRateAsFloat
```

```Smalltalk
Neuron>> learningRate
	"Return the learning rate of the neuron"
	^ learningRate 
```

Feeding has to be adapted:
```Smalltalk
Neuron>>feed: inputs
	| z |
	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
	^ activationFunction eval: z
```

We are now ready to implement the algorithm to train a sigmoid neuron. Here is the method:

```Smalltalk
Neuron>>train: inputs desiredOutput: desiredOutput
    | diff output delta |
    output := self feed: inputs.
    diff := desiredOutput - output.
    delta := diff * (activationFunction derivative: output).    

    inputs withIndexDo: [ :anInput :index | 
        weights at: index put: ((weights at: index) + (learningRate * delta * anInput)) ].

    bias := bias + (learningRate * delta)
```

The method `train:desiredOutput:` is very similar to what we have seen with the perceptron. We have introduced a `delta` local variable, which represents the error multiplied by the transfer derivative. We use the transfer derivative to formulate a _gradient descent_. We will explore that topic in detail in Chapter 5.

We now need to initialize a neuron as being a sigmoid:

```Smalltalk
Neuron>>initialize
	super initialize.
	learningRate := 0.1.
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

If you run `PerceptronTest` you will see that several test methods fail. The reason is that a neuron is initialized with a sigmoid activation function. We therefore need to adapt each test method, by adding a call to `step`. For example, the method `testAND` has to be rewritten:

```Smalltalk
PerceptronTest>>testAND
    | p |
	p := Neuron new.
	p step. "<= new line"
    p weights: #(1 1).
    p bias: -1.5.
    
    self assert: (p feed: #(0 0)) equals: 0.
    self assert: (p feed: #(0 1)) equals: 0.
    self assert: (p feed: #(1 0)) equals: 0.
    self assert: (p feed: #(1 1)) equals: 1.
```

Adding the call to `step` makes the neuron behave as a perceptron. Omitting this line would instead use a sigmoid neuron, and the tests would fail since the output would not exactly be `0` or `1`.

*EXERCISE:* Adapt all the test methods of `PerceptronTest` to use a neuron with a step function.

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
	p weights: #(-1 -1).
	p bias: 2.
	
	5000
		timesRepeat: [ 
			p train: #(0 0) desiredOutput: 0.
			p train: #(0 1) desiredOutput: 0.
			p train: #(1 0) desiredOutput: 0.
			p train: #(1 1) desiredOutput: 1 ].
		
	self assert: ((p feed: #(0 0)) closeTo: 0 precision: 0.1).
	self assert: ((p feed: #(0 1)) closeTo: 0 precision: 0.1).
	self assert: ((p feed: #(1 0)) closeTo: 0 precision: 0.1).
	self assert: ((p feed: #(1 1)) closeTo: 1 precision: 0.1).
```

There are two differences:

- The number of epochs has significantly increased. The reason is that the sigmoid neuron learns slower than the perceptron. 
- The result of feeding the neuron is compared using the call `closeTo:precision:`. Since the result of the `feed:` method is now a floating value and not an integer, we need to adapt our way of comparing these values. If you are still unsure what's wrong in using `==` between floats, evaluate the expression `0.1 + 0.2 - 0.3`. It returns `5.551115123125783e-17` and not `0` as one would expect. The way that float values are encoded causes this behavior.

Similarly we can train a sigmoid neuron to learn the OR behavior:
```Smalltalk
NeuronTest>>testTrainingOR
	| p |
	p := Neuron new.
	p weights: #(-1 -1).
	p bias: 2.
	
	5000
		timesRepeat: [ 
			p train: #(0 0) desiredOutput: 0.
			p train: #(0 1) desiredOutput: 1.
			p train: #(1 0) desiredOutput: 1.
			p train: #(1 1) desiredOutput: 1 ].
		
	self assert: ((p feed: #(0 0)) closeTo: 0 precision: 0.1).
	self assert: ((p feed: #(0 1)) closeTo: 1 precision: 0.1).
	self assert: ((p feed: #(1 0)) closeTo: 1 precision: 0.1).
	self assert: ((p feed: #(1 1)) closeTo: 1 precision: 0.1).
```

As you can see, using a sigmoid neuron does not mess up our tests. We simply need (i) to increase the number of epochs to which we train the neuron, and we need (ii) to be more careful when comparing floating values.

*EXERCISE:* We wrote an adapted version of the OR and AND logical gates for the sigmoid neuron. Adapt the other logical gates to use the Sigmoid neuron.

## Slower to learn

This chapter started by pointing out a strong limitation of the perceptron. This has motivated us to formulate the sigmoid neuron. We see one drawback of the sigmoid neuron: it is slower to learn than the perceptron. We are here making a bet, which is trading efficiency for flexibility: as we will see in the next chapter, sigmoid neuron can be nicely combined.

We can easily compare the learning of the sigmoid neuron versus the perceptron. Consider the following script:

```Smalltalk
learningCurveNeuron := OrderedCollection new.
0 to: 1000 do: [ :nbOfTrained |
    r := Random new seed: 42.
    p := Neuron new.
    p weights: #(-1 -1).
    p bias: 2.

    nbOfTrained timesRepeat: [ 
        p train: #(0 0) desiredOutput: 0.
        p train: #(0 1) desiredOutput: 0.
        p train: #(1 0) desiredOutput: 0.
        p train: #(1 1) desiredOutput: 1 ].
   
    res :=  ((p feed: #(0 0)) - 0) abs + 
            ((p feed: #(0 1)) - 0) abs +
            ((p feed: #(1 0)) - 0) abs +
            ((p feed: #(1 1)) - 1) abs.
     learningCurveNeuron add: res / 4.
    
].

learningCurvePerceptron := OrderedCollection new.
0 to: 1000 do: [ :nbOfTrained |
    r := Random new seed: 42.
    p := Neuron new.
	p step.
    p weights: #(-1 -1).
    p bias: 2.

    nbOfTrained timesRepeat: [ 
        p train: #(0 0) desiredOutput: 0.
        p train: #(0 1) desiredOutput: 0.
        p train: #(1 0) desiredOutput: 0.
        p train: #(1 1) desiredOutput: 1 ].
   
    res :=  ((p feed: #(0 0)) - 0) abs + 
            ((p feed: #(0 1)) - 0) abs +
            ((p feed: #(1 0)) - 0) abs +
            ((p feed: #(1 1)) - 1) abs.
     learningCurvePerceptron add: res / 4.
    
].

g := RTGrapher new.
d := RTData new.
d label: 'Sigmoid neuron'.
d noDot.
d connectColor: Color blue.
d points: learningCurveNeuron.
d y: #yourself.
g add: d.

d := RTData new.
d label: 'Perceptron'.
d noDot.
d connectColor: Color green.
d points: learningCurvePerceptron.
d y: #yourself.
g add: d.
g axisY title: 'Error'.
g axisX noDecimal; title: 'Epoch'.
g legend addText: 'Perceptron vs Sigmoid neuron'.
g
```

![Perceptron vs Sigmoid neuron](03-Neuron/figures/perceptronVsSigmoid.png){#fig:perceptronVsSigmoid width=400px}

The script produces Figure @fig:perceptronVsSigmoid. No matter the learning rate defined, the perceptron is indeed much faster to learn than the sigmoid neuron. 

The next chapter will reveals the true power of sigmoid neuron, which will offset the fact it is slower to learn.


## What have we seen in this chapter
This chapter covers the following topics:

- _Briefly discussed the limitation of the perceptron._ The perceptron cannot learn when combined with other perceptrons. Although we have not discussed this aspect further, you need to trust me for now. In the next chapter we will develop this further.
- _Definition of the sigmoid neuron._ The sigmoid neuron is an improvement of the perceptron since it can be combined with other sigmoid neurons and this combination can learn. In the next chapter we will detail the backpropagation algorithm, a central aspect when making a neural network learn.
- _Activation functions._ We have seen two activation functions, the step and sigmoid functions. Many other activation functions are around. 

The next chapter is about composing sigmoid neuron to build artificial neural networks.

