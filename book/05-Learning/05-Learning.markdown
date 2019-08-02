
# Theory on Learning

Understanding the learning algorithm of neural network involves a fair dose of mathematical notations. This chapters is by no means a complete description of how networks do learn. As indicated at the end of this chapter, many other people have done an excellent job at accurately describing what is the theoretical foundation of learning and optimization mechanisms. Instead, this chapter is meant to backup some aspects of the implementation we gave in the previous chapters. It is assumed that you are comfortable with basic differential calculus. 

You can safely skip this chapter if it does not cover your interest. 

## Loss function

The purpose for a network to learn is to reduce the amount of errors it does when making a prediction. Such a prediction could be used either to classify data or to make regression. It is therefore essential to have a way to measure the error made by a network. This is exactly what a loss function is about.

A _loss function_ is a measure of the amount of error made by a particular model. The loss function is also commonly called the _error function_ or the _cost function_. To illustrate the use and need of a loss function, let us consider the following problem: for a given set of points, what is the straight line that is the closest to these points?

We consider a set of 4 points:

```Smalltalk
points :={(1 @ 3.0). (3 @ 5.2). (2 @ 4.1). (4 @ 7.5)}.

g := RTGrapher new.
d := RTData new.
d dotShape color: Color red.
d points: points.
d x: #x; y: #y.
g add: d.
g
```

![Plotting four points.](05-Learning/figures/plottingPoints.png){#fig:plottingSomePoints width=400px}

Figure @fig:plottingSomePoints shows the plot of these four points. Identifying a straight line that is close to these points means that we need to find the best value of `a` and `b` to have the function $y = f(x) = a*x + b$ that is the closest to these points. Since the points are not perfectly aligned, there is no line that exactly passes by the points.

Let's pick some arbitrary `a` and `b` and draw a line:

```Smalltalk
points :={(1 @ 3.0). (3 @ 5.2). (2 @ 4.1). (4 @ 7.5)}.

a := 0.5.
b := 3.
f := [ :x | a * x + b ].

g := RTGrapher new.
d := RTData new.
d dotShape color: Color red.
d points: points.
d x: #x; y: #y.
g add: d.
d := RTData new.
d noDot; connectColor: Color blue.
d points: (0 to: 5).
d x: #yourself.
d y: f.
g add: d.
g
```

![Points and a line.](05-Learning/figures/pointsAndLine.png){#fig:pointsAndLine width=400px}

Figure @fig:pointsAndLine shows the points and the blue line that we arbitrarily defined. As one can see, the value we picked for `a` and `b` are not really good since the blue line is rather far away from the first and the fourth points. If we want to look for better `a` and `b`, then we need to translate in some way how far the blue lines is from the points. We know our line is not great, but _how_ bad is it? It is important that we have some way of measuring how good our approximation is.

A _loss function_ is a mathematical function that maps an event, described as a set of values of one or more variables, into a numerical value. The numerical value given by the loss function intuitively represents the _cost_ associated with the event, generally a numerical value. In our case, the loss function approximates the distance between the straight blue line with the four points. If the blue line is close to the four points, then the cost will be relatively low. Oppositely, if it is far away from the points, then the cost will be high. In our case, let's make the loss function tell us how off is the straight blue line approximating the 4 points.

A common loss function is the _mean squared error_ (MSE). This function, in our case, is defined as the $J$ function as follows:

$$J(a,b) = \frac{1}{n} \sum_{i=1}^{n}(y_i - f_{a,b}(x_i))^2$$

The $J$ function is the mean squared difference between our line and each of the points. Note that $J$ is always positive. The $J$ function indicates how close the $f$ function is to the points $(x_i, y_i)$, for two given values of $a$ and $b$. Note that the variables $x_1, ..., x_n, y_1, ..., y_n$ represent the data for which we would like to tune our model for. 
In our case, these variables represent our points $(x_1, y_1), ..., (x_n, y_n)$. We can compute the value of $J$ as follows:

```Smalltalk
points :={(1 @ 3.0) . (3 @ 5.2) . (2 @ 4.1) . (4 @ 7.5)}.
a := 0.5.
b := 3.
f := [ :x | a * x + b ].
j := (points collect: [ :p | (p y - (f value: p x)) squared ]) sum / points size.
```

The script returns the value `1.75`. If we change `a` for `2` and `b` for `-0.5`, `j` equals to `0.67`. If you draw the line with `a := 2` and `b := -0.5`, you will see that the blue line is closer to the red dots. 

We are here highlighting an important use of the loss function. Changing parameters (`a` and `b` in our case) may increase or decrease the MSE. A decrease of the MSE indicates that our parameters are better since our model makes less mistakes. 

How does this simple line relate to the learning mechanism of a neural network? The backpropagation algorithm is directly based on this mechanism but at a larger scale. In this example we look for two values (`a` and `b`), in a neural networks we could look for thousands or millions of values, which correspond to the weights and biases. 

Let's come back to the points and lines example. Our original problem is to find the straight line that is the closest to the red points. This problem can therefore be translated into looking for `a` and `b` that minimize the MSE value. Looking for these two values manually is rather tedious and laborious. The natural next step is automatically finding the `a` and `b` that minimize the loss function. No need to say how much programers do like to automatize everything :-)

## Gradient Descent

We know that modifying the $a$ value changes the slope of our line, and modifying the $b$ value moves the point in which the line intersects the Y axis.
So, each of the values modifies our line in a particular way. We are indeed searching for the best $a$ and $b$, but we cannot try all the possibilities, essentially for two reasons: (i) it could be extremely expensive (trying all the combinations of possible values of $a$ and $b$ is a daunting task), and (ii) since $a$ and $b$ are continuous values, in theory, there is not a finite set of values to try out.

In a general case, we have many parameters to search and it is not clear what each of them do. So to express a small change in our model we introduce the derivative. Since we focus on a small change of a single parameter in a multivariable function, we need to use the partial derivative.

The gradient descent is a general mechanism to look for an optimal model configuration. Gradient descent is intensively used in the field of mathematical optimization, including making neural network learn. 

First, we need to calculate the partial derivative of $MSE(a,b)$ with respect to each variable value. Remember the $J$ function we gave above:

$$J(a,b) = \frac{1}{n} \sum_{i=1}^{n}(y_i - f_{a,b}(x_i))^2$$

If we expand the $f$ function in $J$, we obtain:

$$J(a,b) = \frac{1}{n} \sum_{i=1}^{n}(y_i - (a.x_i + b))^2$$

We can deduce the following partial derivatives, with respect to $a$ and $b$:

$$\frac{\partial J(a,b)}{\partial a} = \frac{-2}{n}\sum_{i}x_{i} .(y_i - (a.x_i + b))$$

$$\frac{\partial J(a,b)}{\partial b} = \frac{-2}{n}\sum_{i}(y_i - (a.x_i + b))$$

Applying the derivative functions  $\frac{\partial J(a,b)}{\partial a}$ and $\frac{\partial J(a,b)}{\partial b}$ to a given `a` and `b` returns the direction to move the parameter in order to decrease the overall $J(a,b)$.

We update `a` and `b` as follow:

$$a := a - \alpha . \frac{\partial J(a,b)}{\partial a}(a,b)$$
$$b := b - \alpha . \frac{\partial J(a,b)}{\partial b}(a,b)$$

The $\alpha$ value is the learning rate, indicating how fast the `a` and `b` should move toward the direction the derivative indicates.

Repeating the update of `a` and `b` will reduce the $J$ loss function, which over time indicates that our model is improving. The following script demonstrates the whole process (we name $\alpha$ as `learningRate`):

```Smalltalk
points :={(1 @ 3.0) . (3 @ 5.2) . (2 @ 4.1) . (4 @ 7.5)}.

a := 0.5.
b := 3.
f := [ :x | x * a + b ].

learningRate := 0.01.
1000 timesRepeat: [  
	deriMSEa := (2 / points size) * (points collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
deriMSEb := (2 / points size) * (points collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
	a := a - (learningRate * deriMSEa).
	b := b - (learningRate * deriMSEb).
].

g := RTGrapher new.
d := RTData new.
d dotShape color: Color red.
d points: points.
d x: #x; y: #y.
g add: d.

d := RTData new.
d noDot; connectColor: Color blue.
d dotShape color: Color blue.
d points: (0 to: 5).
d x: #yourself.
d y: f.
g add: d.
g
```

![Gradient descent.](05-Learning/figures/gradientDescent.png){#fig:gradientDescent}

Figure @fig:gradientDescent gives the result of the script execution. The script computes the values of `a` and `b` that make the blue line close to the four points. Said in other terms, the gradient descent technique is applied to minimize the $J(a,b)$ cost function. Actually, after 1000 iterations, we approximate the minimum of $J$ at the point $(1.42, 1.39)$.

## Parameter update

The script we gave above may look a bit mysterious. We repeatedly decrease the values `a` and `b` with a little step, result of multiplying a derivative value multiplied by `learningRate`. For some reason, the cost function decreases. Why? To answer this question we need to take a little step deeper in some essential mathematical concepts.

Assuming a function $f$ and a known value of it at $x$, written $f(x)$.
Knowing $f(x)$, the Taylor series is used to approximate the value of $f$ at $x + e$, where $e$ is a very small value.
Back at the beginning of the 18th century, it was discovered that in the case of an infinitely differentiable function (as neural network deal with), we can approximate the value of $f(x+e)$ as:

$$
f(x+e) = f(x) + e f'(x)/1! +  e^2 f''(x)/2! + ...
$$

<!-- So, we want to know in which direction to go. We know that the derivative tell us the value we should change, but do not tell us in which direction. We use the Taylor series to solve this problem. -->
Why is computing $f(x+e)$ such a thing? Well, neural network is about making prediction / regression, and learning is about determining which changes in the weights and biased makes the network perform better, which is indirectly expressed with $f(x+e)$. If $f$ is our loss function, we would like to change weights and biases in such a way that $f(x+e)$ is closer to 0 than $f(x)$.


If we know $f(x)$ and we search for $f(x+e)$ to be less than $f(x)$, so we should changes the parameters of the network to follow a descending slope of $f$. 
For a linear function we can approximate up to the first derivative as $f(x+e) = f(x)+e f'(x)$ so to minimize $f(x+e)$ we need $e f'(x)$ to decrease $f(x)$. The only arbitrary value is $e$, so lets find $e$ that minimizes $f$. The derivate $e f'(x)$ with respect to $e$ is:

$$
\frac{d(e f'(x))}{d e} = f'(x)
$$

We can take $e=f'(x)$. But in this case, $f$ maximizes, so lets choose $e = -f'(x)$, this will minimize $f$. So replacing in our Taylor series:
$$
f(x+e) = f(x)+e f'(x)
$$

$$
f(x-f'(x)) = f(x) - f'(x)^2
$$

We can therefore deduce $f(x-f'(x)) < f(x)$ since _we have_ $f'(x)^2$ a positive value.
So if we update $x$ with by subtracting the derivative of $f$, then $f(x)$ is getting closer to 0. We can add the learning value with $e=- \alpha f'(x)$.

<!--
@@CHECK The Taylor series assumes that for a function $f(x)$ that operates on real numbers (_i.e.,_ $x$ and $f(x) \in \mathbb{R}$) and that is infinitely differentiable, then we have:

$$
f(x) = f(x_0) + (x-x_0)f'(x_0) + R(x)
$$

Where $a$ is a given and fixed value, and $R(x)$ is the approximation error which goes to 0 when $x$ is getting close to $x_0$. Assuming $x = x_0 + \epsilon$ and since $R(x)$ is a very small value anyway, we therefore have the following approximation:

$$
f(x_0 + \epsilon) \approx f(x_0) + \epsilon f'(x_0)
$$

If we replace $\epsilon$ by $\alpha f'(x_0)$, then we have:

$$
f(x_0 - \alpha f'(x_0)) \approx f(x_0) - \alpha f'(x_0)^2
$$

If $\alpha$ is positive, then $\alpha f'(x_0)^2$ is also positive. We can therefore deduce the relation:

$$
f(x_0 - \alpha f'(x_0)) < f(x_0)
$$

If we now consider $f$ as our cost function, then updating the variable $x_0$ by $x_0 - \alpha f'(x_0)$ reduces the value $f(x_0)$. 
-->

We can write the following expression:

$$
x := x - \alpha f'(x)
$$

This expression reduces the value of $f(x)$, if $f'(x) \neq 0$. Fortunately, we took care of choosing the cost function $J$ to comply with these requirements. Otherwise we would get stuck and stop learning.

Consider the following script:

```Smalltalk
points := {(1 @ 3.0) . (3 @ 5.2) . (2 @ 4.1) . (4 @ 7.5)}.

a := 0.5.
b := 3.

f := [ :x | x * a + b ].

learningRate := 0.01.

result := OrderedCollection new.
1000 timesRepeat: [  
	deriMSEa := (2 / points size) * (points collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
	deriMSEb := (2 / points size) * (points collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
	a := a - (learningRate * deriMSEa).
	b := b - (learningRate * deriMSEb).
	
	mse := (points collect: [ :aPoint | (aPoint y - (f value: aPoint x)) squared ]) sum / points size.
	result add: mse ].

g := RTGrapher new.
d := RTData new.
d noDot; connectColor: Color blue.
d points: result.
d y: #yourself.
g add: d.
g
```

![Variation of the MSE cost function](05-Learning/figures/gradientDescent2.png){#fig:gradientDescent2 width=400px}

Figure @fig:gradientDescent2 gives the variation of the cost function at each update of the `a` and `b` values. You can see that it gets closer to 0, but still remains far away. The reason is that since the points we used are not perfectly lined up, there is no `a` and `b` that makes the cost value equals to 0. If you pick points that are perfectly lined up, (_e.g.,_ `{(4 @ 6.5). (2 @ 3.5). (2 @ 3.5). (2 @ 3.5)}`), then the cost function is asymptotic to 0.

## Gradient Descent in our implementation

In Chapter 3, when we presented the activation function, we generalized the way an artificial neuron learn using the following rules:

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

<!--
@@SAY HOW DO WE GET THESE PARTIAL DERIVATIVE
-->

There is a strong similarity with the update rules we proposed for the `a` and `b` values. In this case, we have:

$$\frac{\partial J}{\partial w_i} = (d - z) * \sigma'(z) * x_i$$
$$\frac{\partial J}{\partial b} = (d - z) * \sigma'(z)$$

These formulas are exposed in the method `Neuron>>adjustDeltaWith:`, `NeuronLayer>>backwardPropagateError:` and `NeuronLayer>>backwardPropagateError`.

## Stochastic Gradient Descent

The gradient descent computes the gradient of the loss function from the whole dataset. This is often difficult because minimum local points and saddle points may be found in our way to search for the global minimum. Furthermore, gradient descent adjusts the parameters based on the sum of the accumulated errors over all samples. This means that parameters are updated only after predicting each point of the whole dataset. This is largely impracticable as soon as the dataset is large.
You can see this in the previous section where we used `sum` when computing `deriMSEa` and `deriMSEb`. 

An alternative to _gradient descent_ is called _stochastic gradient descent_ (SGD). With SCG, you first need to shuffle your training examples and divide them into small sets of datasets. Parameters are updated only after running a whole mini-batch. As a consequence, training over the whole dataset is faster using SGD. We will illustrate the idea with our regression problem.

Consider the script given above slightly updated to take a larger number of dataset:

```Smalltalk
nbOfPoints := 100.
r := Random seed: 42.
points := (1 to: nbOfPoints) collect: [ :i | (i / nbOfPoints) asFloat @ ((r next * 40 - 20 + i ) / nbOfPoints) asFloat ].

a := 0.5.
b := 3.
learningRate := 0.01.
f := [ :x | x * a + b ].

result := OrderedCollection new.
3000 timesRepeat: [  
	deriMSEa := (2 / points size) * (points collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
	deriMSEb := (2 / points size) * (points collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
	a := a - (learningRate * deriMSEa).
	b := b - (learningRate * deriMSEb).
	
	mse := (points collect: [ :aPoint | (aPoint y - (f value: aPoint x)) squared ]) sum / points size.
	result add: mse ].

g := RTGrapher new.
d := RTData new.
d noDot; connectColor: Color blue.
d points: result.
d y: #yourself.
g add: d.
g.
```

![Approximating a line passing by 100 points](05-Learning/figures/largerExampleGD.png){#fig:largerExampleGD}

The script creates a dataset large of 100 points. Points are located around the line $y = x$ (the following script will illustrate this). Figure @fig:largerExampleGD indicates that our model is able to learn from the dataset using a gradient descent. 

We can plot the 100 points and the line we have found using:

```Smalltalk
...
g := RTGrapher new.
d := RTData new.
d dotShape color: Color red.
d points: points.
d y: #y.
d x: #x.
g add: d.

d2 := RTData new.
d2 noDot; connectColor: Color blue.
d2 points: (0 to: 1.0 by: 0.01).
d2 x: #yourself.
d2 y: f.
g add: d2.
g
```

![Approximating a line passing by 100 points](05-Learning/figures/largerExampleGD2.png){#fig:largerExampleGD2}

Figure @fig:largerExampleGD2 show the layout of the dataset. Our model found a pretty approximation.

Let's rewrite the script above using a stochastic gradient descent algorithm: 

```Smalltalk
nbOfPoints := 100.
r := Random seed: 42.
points := (1 to: nbOfPoints) collect: [ :i | (i / nbOfPoints) asFloat @ ((r next * 40 - 20 + i ) / nbOfPoints) asFloat ].

currentBatch := OrderedCollection new.
miniBatches := OrderedCollection new.
batchSize := 5.
1 to: points size do: [ :index | 
	 currentBatch add: (points at: index).
	index \\ batchSize = 0 
		ifTrue: [ miniBatches add: currentBatch copy. currentBatch := OrderedCollection new. ]].
miniBatches.

a := 0.5.
b := 3.
learningRate := 0.01.
f := [ :x | x * a + b ].

result := OrderedCollection new.
1000 timesRepeat: [  
	accumulatedMse := 0.
	miniBatches do: [ :pointsBatch |
		deriMSEa := (2 / pointsBatch size) * (pointsBatch collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
		deriMSEb := (2 / pointsBatch size) * (pointsBatch collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
		a := a - (learningRate * deriMSEa).
		b := b - (learningRate * deriMSEb).
	
		mse := (pointsBatch collect: [ :aPoint | (aPoint y - (f value: aPoint x)) squared ]) sum / points size.
		accumulatedMse := accumulatedMse + mse
	].
	result add: accumulatedMse ].

g := RTGrapher new.
d := RTData new.
d noDot; connectColor: Color blue.
d points: result.
d y: #yourself.
g add: d.
g.
```

Our script is very similar to our version using gradient descent. The only differences are:

- We have the variable `miniBatches` that contains batches of points. Each batch has a size of `batchSize` points. 
- Instead of learning from `points`, the whole dataset, we incrementally update the $a$ and $b$ parameters after having run over the `pointsBatch` mini-batch.

The result of the script is very similar than with the gradient descent.

![Result of the stochastic gradient descent](05-Learning/figures/largerExampleSGD.png){#fig:largerExampleSGD}

Figure @fig:largerExampleSGD shows a very similar shape of the error function. This means our model is able to comfortably learn from our dataset, as when using the gradient descent. 

The difference between the stochastic and non-stochastic algorithm is reflected when measuring performance. 

Consider the script, which use the gradient descent algorithm:

```Smalltalk
[ nbOfPoints := 30000.
r := Random seed: 42.
points := (1 to: nbOfPoints) collect: [ :i | (i / nbOfPoints) asFloat @ ((r next * 40 - 20 + i ) / nbOfPoints) asFloat ].

a := 0.5.
b := 3.
f := [ :x | x * a + b ].
learningRate := 0.01.

result := OrderedCollection new.
3000 timesRepeat: [  
	deriMSEa := (2 / points size) * (points collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
	deriMSEb := (2 / points size) * (points collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
	a := a - (learningRate * deriMSEa).
	b := b - (learningRate * deriMSEb).
	
	mse := (points collect: [ :aPoint | (aPoint y - (f value: aPoint x)) squared ]) sum / points size.
	result add: mse ]. ] timeToRun
"==> "0:00:00:27.479"
```

Running this script takes over 27 seconds using 3.2GHz Intel Core i5, with 16Gb of RAM. 

Consider the stochastic version:

```Smalltalk
[ nbOfPoints := 30000.
r := Random seed: 42.
points := (1 to: nbOfPoints) collect: [ :i | (i / nbOfPoints) asFloat @ ((r next * 40 - 20 + i ) / nbOfPoints) asFloat ].

currentBatch := OrderedCollection new.
miniBatches := OrderedCollection new.
batchSize := 5.
1 to: points size do: [ :index | 
	 currentBatch add: (points at: index).
	index \\ batchSize = 0 
		ifTrue: [ miniBatches add: currentBatch copy. currentBatch := OrderedCollection new. ]].

a := 0.5.
b := 3.
f := [ :x | x * a + b ].
learningRate := 0.01.

result := OrderedCollection new.
1000 timesRepeat: [  
	accumulatedMse := 0.
	miniBatches do: [ :pointsBatch |
		deriMSEa := (2 / pointsBatch size) * (pointsBatch collect: [ :aPoint | aPoint x * (aPoint y - (f value: aPoint x)) negated ]) sum.
		deriMSEb := (2 / pointsBatch size) * (pointsBatch collect: [ :aPoint | 1 * (aPoint y - (f value: aPoint x)) negated ]) sum.
		a := a - (learningRate * deriMSEa).
		b := b - (learningRate * deriMSEb).
	
		mse := (pointsBatch collect: [ :aPoint | (aPoint y - (f value: aPoint x)) squared ]) sum / points size.
		accumulatedMse := accumulatedMse + mse
	].
	result add: accumulatedMse ].
] timeToRun 
"==> 0:00:00:18.847"
```

It takes almost 10 seconds less, without significantly reducing the quality of the training.


## The derivative of the sigmoid function

The method `SigmoidAF>>derivative:` is defined as:
```Smalltalk
SigmoidAF>>derivative: output
    ^ output * (1 - output)
```

This section describes why this method is defined that way. As we have previously seen, we have $\sigma(x)=\frac{1}{1+e^{-x}}$. So, we also have:

$\frac{d}{dx}\sigma(x)=\frac{d}{dx}~\frac{1}{1+e^{-x}}$

$=\frac{d}{dx} (1+e^{-x})^{-1}$

Since the derivative of $x^n$ is $nx^{n-1}$, we have 

$=-(1 + e^{-x})^{-2}(-e^{-x})$

By rearranging terms we have:

$=\frac{e^{-x}}{(1 + e^{-x})^{2}}$

$=\frac{1}{1 + e^{-x}} . \frac{e^{-x}}{1 + e^{-x}}$

$=\frac{1}{1 + e^{-x}} . \frac{(1 + e^{-x}) - 1}{1 + e^{-x}}$

$=\frac{1}{1 + e^{-x}} . (\frac{1 + e^{-x}}{1 + e^{-x}} - \frac{1}{1 + e^{-x}})$

$=\frac{1}{1 + e^{-x}} . (1 - \frac{1}{1 + e^{-x}})$

$=\sigma(x) . (1 - \sigma(x))$

This result is expressed in `SigmoidAF>>derivative:`. 

<!---
## Backpropagation

Backpropagation is a fundamental technique to make neural network learn. Once the network performs a prediction or a regression, the error is computed between what the network produces and what it is expected. This error is feed back to the network using backpropagation. Weights and biases are then updated using this error. 

### Loss function

At the beginning of the chapter we introduced the notion of loss function. 
Consider two values $y$ and $y'$, each being a vector living in $\mathbb{R}^n$. We pick the Euclidean distance between the vector $y$ and $y'$ as our loss function:

$$
E(y, y') = \frac{1}{2}||y - y'||^2
$$

The expression $|| ... ||$ represents the magnitude of the vector enclosed between the sets of double vertical bars. 

For example, if $v$ is a vector of length three with the elements $v_1$, $v_2$, $v_3$, then $||v|| = \sqrt{v_1^2+v_3^2+v_3^2}$.)


For example, considering elements of $\mathbb{R}^2$, if $y = (1, 0)$ and $y' = (0.5, 0.5)$, then $E(y, y') = \frac{1}{2}\sqrt{(1 - 0.5)^2 + (0 - 0.5)^2} = 0.353$. 

Note that the fact of $\frac{1}{2}$ canvas the exponent when the function $E$ is differentiated. In particular, we have:

$$
\frac{\partial E}{\partial y'} = y' - y
$$

The error function over $n$ training examples can simply be written as an average of losses over individual examples:

$$
E = \frac{1}{2n}\sum_{x \in X}||y(x) - y'(x)||^2
$$


## Applying gradient descent to weights

Let $N$ be a neural network with $e$ connections, $m$ inputs, and $n$ outputs. We consider $x_1, x_2, ...$ vectors in $\mathbb{R}^m$, and $y_1, y_2, ...$ vectors in $\mathbb{R}^n$, and $w_0, w_1, w_2, ...$ vectors in $\mathbb{R}^e$. We call these sets _inputs_, _outputs_, _weights_ respectively. The neural network corresponds to a function $y = f_N(w, x)$, which, given a weight $w$, maps an input $x$ to an output $y$. 

Training the neural network $N$ refers to producing a sequence of weights $w_0, w_1, ..., w_p$ for a sequence of _training examples_ $(x_1, y_1), ..., (x_p, y_p)$. At each The initial weight $w_0$ is randomly chosen and corresponds to the whole set of weights and biases of the network.

The weights are computed in turn. First compute $w_i$ using only $(x_i, y_i, w_{i-1})$ for $i = 1, ..., p$. The output of the backpropagation algorithm is $w_p$, which gives us a new function $x \longmapsto f_N(w_p, x)$. The computation is the same in each step, hence only the case $i = 1$ is described below.

Calculating $w_1$ from $(x_1, y_1, w_0)$ is done by considering a variable weight $w$ and applying gradient descent to the function $w \longmapsto E(f_N(w, x_1), y_1)$ to find a local minimum, starting at $w = w_0$. This make $w_1$ the minimizing weight found by gradient descent. 

-->

## What we have seen

This section presents some of theoretical foundations of the implementation found in the previous chapters. In particular, we have seen:

- The loss function as a measure of the amount of error made by a particular model, such as a neural network;
- We illustrate the notion of gradient descent and the benefits of the stochastic gradient descent;
- We also connected some aspects of our implementation with some theoretical properties of making a network learn.
<!--- 
- We briefly presented the backpropagation algorithm, a central algorithm in making learn neural networks.
-->

## Further reading

A number of excellent bibliographical references exist. _Deep Learning_ by Goodfellow _et al._, from MIT Press, is a reference of the field. Note that this book does not mention programming and implementation detail. It lay down the theoretical foundation of deep learning. A free version of the book is available from [https://www.deeplearningbook.org](https://www.deeplearningbook.org)

