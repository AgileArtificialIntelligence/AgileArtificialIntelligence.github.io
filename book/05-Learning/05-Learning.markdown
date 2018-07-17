
# Theory on Learning

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

![Plotting four points.](05-Learning/figures/plottingPoints.png){#fig:plottingSomePoints width=400px}

Figure @fig:plottingSomePoints shows the plot of these four points. Solving the problem means that we need to find the best value of `a` and `b` to have the function `y = f(x) = a*x + b` that is the closest to these points. 

Let's pick some arbitrary `a` and `b` and draw a line:

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

![Points and a line.](05-Learning/figures/pointsAndLine.png){#fig:pointsAndLine width=400px}

Figure @fig:pointsAndLine shows the result. As you can see, the value we picked for `a` and `b` are not really good since the blue line is rather far away from the first and the fourth points. If we want to look for better `a` and `b`, then we need to translate in some way how far the blue lines is from the points. 

A _loss function_ is a mathematical function that maps an event, described as a set of values of one ore more variables, into a real number. This number intuitively represent the _cost_ associated with the event. In our case, the loss function approximate the distance between the straight blue line with the four points. 

The loss function is also commonly called the _error function_ or the _cost function_.

A common loss function is the _mean squared error_ (MSE), which we define as the $J$ function as follows:
$$J(a,b) = \frac{1}{n} \sum_{i=1}^{n}(y_i - f(x_i))^2$$

Note that $J$ is always positive. The $J$ function indicates how close the $f$ function is to the points $(x_i, y_i)$, for two given values of $a$ and $b$. Note that the variables $x_1, ..., x_n, y_1, ..., y_n$ represent the data for which we would like to tune our model for. 
We can compute the value of $J$ as follows:

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

![Gradient descent.](05-Learning/figures/gradientDescent.png){#fig:gradientDescent}

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

![Variation of the MSE cost function](05-Learning/figures/gradientDescent2.png){#fig:gradientDescent2 width=400px}

Figure @fig:gradientDescent2 gives the variation of the cost function at each update of the `a` and `b` values. You can see that it gets closer to 0, but still remains far away. The reason is that since the points we used are not perfectly lined up, there is no `a` and `b` that makes the cost value equals to 0. If you pick points that are perfectly lined up, (_e.g.,_ `{(4@6.5). (2@3.5). (2@3.5). (2@3.5)}`), then the cost function is asymptotic to 0.

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

### Loss function

Consider two values $y$ and $y'$, each being a vector living in $\mathbb{R}^n$. We pick the Euclidean distance between the vector $y$ and $y'$ as our loss function:

$$
E(y, y') = \frac{1}{2}||y - y'||^2
$$

The expression $|| ... ||$ represents the magnitude of the vector enclosed between the sets of double vertical bars. For example, if $v$ is a vector of length three with elements $v_1$, $v_2$, $v_4$, then $||v|| = \sqrt{v_1^2+v_3^2+v_3^2}$.

For example, considering elements of $\mathbb{R}^2$, if $y = (1, 0)$ and $y' = (0.5, 0.5)$, then $E(y, y') = \frac{1}{2}\sqrt{(1 - 0.5)^2 + (0 - 0.5)^2} = 0.353$. 

Note that the fact of $\frac{1}{2}$ canvas the exponent when the function $E$ is differentiated. In particular, we have:

$$
\frac{\partial E}{\partial y'} = y' - y
$$

The error function over $n$ training examples can simply be written as an average of losses over individual examples:

$$
E = \frac{1}{2n}\sum_{x \in X}||y(x) - y'(x)||^2
$$

### Applying gradient descent to weights

Let $N$ be a neural network with $e$ connections, $m$ inputs, and $n$ outputs. We consider $x_1, x_2, ...$ vectors in $\mathbb{R}^m$, and $y_1, y_2, ...$ vectors in $\mathbb{R}^n$, and $w_0, w_1, w_2, ...$ vectors in $\mathbb{R}^e$. We call these sets _inputs_, _outputs_, _weights_ respectively. The neural network corresponds to a function $y = f_N(w, x)$, which, given a weight $w$, maps an input $x$ to an output $y$. 

Training $N$ refers to producing a sequence of weights $w_0, w_1, ..., w_p$ for a sequence of _training examples_ $(x_1, y_1), ..., (x_p, y_p)$. The initial weight $w_0$ is randomly chosen.

The weights are computed in turn. First compute $w_i$ using only $(x_i, y_i, w_{i-1})$ for $i = 1, ..., p$. The output of the backpropagation algorithm is $w_p$, which gives us a new function $x \longmapsto f_N(w_p, x)$. The computation is the same in each step, hense only the case $i = 1$ is described below.

Calculating $w_1$ from $(x_1, y_1, w_0)$ is done by considering a variable weight $w$ and applying gradient descent to the function $w \longmapsto E(f_N(w, x_1), y_1)$ to find a local minimum, starting at $w = w_0$. This make $w_1$ the minimizing weight found by gradient descent. 



## What we have seen

This section presents some of theoretical foundations of the implementation found in the previous chapters. In particular, we have seen:

- The loss function as a measure of the amount of error made by a particular model, such as a neural network
- We briefly presented the backpropagation algorithm, a central algorithm in making learn neural networks.

## Further readings

- Wikipedia offers a great introduction to the backpropagation algorithm, from which the description presented in this chapter is heavily inspired from (https://en.wikipedia.org/wiki/Backpropagation)

Many blog entries are available across the net. For example:

- A more complete description of the backpropagation: https://brilliant.org/wiki/backpropagation/
- A nice illustration of the backpropagation algorithm: https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/

