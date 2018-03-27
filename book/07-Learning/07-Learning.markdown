
# Learning

Understanding the learning algorithm of neural network involves a fair dose of mathematical notations. This chapters is by no means a complete description of how network learn. Other people have done excellent job at accurately describing what is the theoretical foundation of learning and optimization mechanisms. Instead, this chapter is meant to backup some aspects of the implementation we gave in the previous chapters. 

It is assumed that you are comfortable with basic differential calculus. 

## Loss function

A loss function is a measure of the amount of error made by a particular model. 

To illustrate the use and need of a loss function, let's consider the following problem: for a given set of points, what is the straight line that is the closest to these points?. 

Let's choose and plot a set of points: 

~~~~~~
points :={(1@3.0). (3@5.2). (2@4.1). (4@7.5)}.

g := RTGrapher new.
d := RTData new.
d dotShape color: Color red.
d points: points.
d x: #x; y: #y.
g add: d.

g
~~~~~~

![Plotting four points.](07-Learning/figures/plottingPoints.png){#fig:plottingSomePoints width=400px}

Figure @fig:plottingSomePoints shows the plot of these four points. Solving the problem means that we need to find the best value of `a` and `b` to have the function `y = f(x) = a*x + b` that is the closest to these points. 

Let pick some arbitrary `a` and `b` and draw a line for two arbitrary `a` and `b`:

~~~~~~
points :={(1@3.0). (3@5.2). (2@4.1). (4@7.5)}.

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
~~~~~~

![Points and a line.](07-Learning/figures/pointsAndLine.png){#fig:pointsAndLine width=400px}

Figure @fig:pointsAndLine shows the result. As you can see, the value we picked for `a` and `b` are not really good since the blue line is rather far away from the first and the fourth points. If we want to look for better `a` and `b`, then we need to translate in some way how far the blue lines is from the points. And we need a _loss function_ for this. 

A common loss function is the _mean squared error_ (MSE), which we define as the $J$ function as follows:
$$J(a,b) = \frac{1}{n} \sum_{i}(y_i - f(x_i))^2$$

$J$ which is always positive, indicates how close the $f$ function is to the points $(x_i, y_i)$. Note that the variables $x_1, ..., x_n, y_1, ..., y_n$ represent the data for which we would like to tune our model for. 
We can compute it as follows:

~~~~~~
points :={(1@3.0) . (3@5.2) . (2@4.1) . (4@7.5)}.
a := 0.5.
b := 3.
f := [ :x | a * x + b ].
j := (points collect: [ :p | (p y - (f value: p x)) raisedTo: 2 ]) sum / points size.
~~~~~~

The script gives the value `1.75` to the variable `j`. If we change `a` for `2` and `b` for `-0.5`, `j` equals to `0.67`. If you draw the line with `a := 2` and `b := -0.5`, you will see that the blue line is closer to the red dots. 

We are here highlighting an important use of the loss function. Changing parameters (`a` and `b` in our case) may increase or decrease the MSE. A decrease of the MSE indicates that our parameters are better since our model makes less error. 

How this simple line relate to the fact that neural network are learning? The backpropagation algorithm is directly based on this mechanism but at a larger scale. In this example we look for two values, in a neural networks we could look for thousands of values, which correspond to the weights and biases. 

Let's come back to the points and lines example. Our original problem is to find the straight line that is the closest to the red point. This problem can therefore be translated into looking for `a` and `b` that minimize the MSE value. Looking for these two values manually is rather tedious and laborious. The natural next step is how to optimize this search?

## Gradient Descent

The gradient descent is a general mechanism to look for an optimal model configuration. Gradient descent is intensively used in the field of mathematical optimization. 

First, we need to calculate the partial derivative of $MSE(a,b)$ with respect to each value. We therefore have:
$$\frac{\partial J(a,b)}{\partial a} = \frac{-2}{n}\sum_{i}x_{i} .(y_i - (a.x_i + b))$$

$$\frac{\partial J(a,b)}{\partial b} = \frac{-2}{n}\sum_{i}(y_i - (a.x_i + b))$$

Applying the derivative functions  $\frac{\partial J(a,b)}{\partial a}$ and $\frac{\partial J(a,b)}{\partial b}$ to a given `a` and `b` returns the direction to move the parameter in order to decrease the overall $J(a,b)$.

We update the `a` and `b` as follow:

$$a := a - \alpha . \frac{\partial J(a,b)}{\partial a}(a,b)$$
$$b := b - \alpha . \frac{\partial J(a,b)}{\partial b}(a,b)$$

The $\alpha$ value is 
Repeating the update of `a` and `b` will reduce the $J$ loss function, which indicates that our model is improving. The following script 

~~~~~~~
points :={(1@3.0) . (3@5.2) . (2@4.1) . (4@7.5)}.

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
~~~~~~~

![Gradient descent.](07-Learning/figures/gradientDescent.png){#fig:gradientDescent}

Figure @fig:gradientDescent gives the execution of the script.


## Parameter update

The script we gave above may look a bit mysterious. We repeatedly decrease the values `a` and `b` with a little step, result of multiplying a derivative value multiplied by `learningRate`. For some reason, the cost function decreases. Why?

The Taylor series assumes that for a function $f(x)$ that operates on real numbers (_i.e.,_ $x$ and $f(x) \in \mathbb{R}$) and that is infinitely differentiable, then we have:

$$
f(x) = f(x_0) + (x-x_0)f'(x_0) + R(x)
$$

Where $a$ is a given and fixed value, and $R(x)$ is the approximation error which goes to 0 when $x$ is getting close to $x_0$. Assuming $x = x_0 + \epsilon$ and $R(x)$ is a very small value anyway, we therefore have:

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

If we now consider $f$ as our cost function, then updating the variable $x_0$ by $x_0 - \alpha f'(x_0)$ reduce the value $f(x_0)$. To conclude:

$$
x_0 := x_0 - \alpha f'(x_0)
$$

reduces the value of $f(x_0)$, if $f'(x_0) \neq 0$. Fortunately, we took care of choosing the cost function $J$ to not have that happen.

Consider the following script:

~~~~~
points := {(1@3.0) . (3@5.2) . (2@4.1) . (4@7.5)}.

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
	
	mse := (points collect: [ :aPoint | (aPoint y - (f value: aPoint x)) raisedTo: 2 ]) sum / points size.
	result add: mse ].

g := RTGrapher new.
d := RTData new.
d noDot; connectColor: Color blue.
d points: result.
d y: #yourself.
g add: d.
g
~~~~~

![Variation of the MSE cost function](07-Learning/figures/gradientDescent2.png){#fig:gradientDescent2 width=400px}

Figure @fig:gradientDescent2 gives the variation of the cost function at each update of the `a` and `b` values. You can see that it gets closer to 0, but still remains far away. The reason is that since the points we used are not perfectly lined up, there is no `a` and `b` that makes the cost value equals to 0. If you pick points that are perfectly lined up, (_e.g.,_ `{(4@6.5). (2@3.5). (2@3.5). (2@3.5)}`), then the cost function is asymptotic to 0.


## Stochastic gradient descent

## Cross-validation

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

## Backpropagation

https://brilliant.org/wiki/backpropagation/


## Further readings
