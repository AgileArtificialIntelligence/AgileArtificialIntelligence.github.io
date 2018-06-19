
# Application: The Pong Game

Pong game is one of the earliest video game ever produced. It represents a tennis-like game in which a ball bounces upon two adversaries rackets. Each player can move the racket horizontally. 

Overall, this chapter is about training a neural network to play the pong game from a human. In particular this chapter presents:
- a discussion on how to model the pong game
- an implementation in Pharo of the whole game

## Why are we talking about pong?

So, what is so special about pong? What is the connection between pong and neural networks? After all, implementing a pong game may looks like an exemplary programming assignment for new students in computer science (and actually, it is). Before answering to these question, let's deviate a bit and phrase a more general question: How can we measure the progresses made in the field of artificial intelligence? A _benchmark_ is a standard point of reference in which some techniques may be compared. A common benchmark in the field of artificial intelligence is the _Atari Game Benchmark_ (https://blog.deepsense.ai/solving-atari-games-with-distributed-reinforcement-learning/). This benchmark consists in a compilation of a number of original Atari video games for which top libraries and research institutions are competing for the highest scores.

Pong is part of the Atari Game Benchmark. Modeling this game is therefore relevant in our context. We will definitely not go as far as Google DeepMind, but we will cover important topics such as game modeling and provide a complete implementation. 

## Modeling Pong

Figure @fig:pong represents what we will have implemented at the end of the chapter. The game is made of two red rackets, two gray walls, and one small blue ball. 

![Example of a pong game.](08-Pong/figures/pong.png){#fig:pong width=300px}

How can we make a neural network able to process a game configuration, such as the one shows in the figure? What are the important features that have to be considered by a network? Said in other words, what are the variables that one needs to consider in order to adequately play the game? It actually depends on the exact dynamic of the game.

![Modeling pong.](08-Pong/figures/pongModeling.png){#fig:pong width=300px}

We consider a very simple version of the game in which: 

- The ball bounces on the racket without affecting the angle. In other words: you cannot "attack" by favoring a particular angle. As a consequence, you do not need to know the position of the opponent in order play. For the network, it does not make a difference between playing against an opponent or playing against a wall
- A racket can only move horizontally, on the very same line. This means that the variable that the user can control is the X position of the racket.
- A racket horizontally moves with a constant increment. This is equivalent of steering the racket with two keys on a keyword. The racket can move left, right, or stay still. The fact there are three possible different actions one can take to handle racket, the neural network provides three outputs values.
- The ball has a position and has a constant speed. The speed is adjusted only when bouncing. As a consequence, the ball entity is described with four values, the X and Y positions, and the X and Y components of its speed. 

Figure @fig:pong illustrates the five variables we need to consider. The neural network we will therefore need for our game has five inputs and three outputs. 


## Learning playing pong

We will build a neural network that tries to mimic the action taken by a human player. This means that we will have two phases in the game. First, we need to collect data. Concretely, at each _beat_ of the game, we need to record six values: the five values previously mentioned with the action taken by the human player (left, right, or stay still). 

We refer to _beat_ the time unit in which the game dynamic makes an elementary increment. In video games, the simplest beat we can have is the refresh of the screen. Each time screen needs to be refreshed (which is decided by Pharo and the operating system), elements of our game will have the opportunity to express a small unit of behavior.

Once we have gathered a fair amount of records, the human player can switch the control of the rackets. A neural network will steer the rackets. The network training is performed upon switching the control. 

## Implementing Pong

The remaining of the chapter provides the complete implementation of pong. Our implementation is minimal and only feature the strict necessary.

Our implementation is made of five classes:

- `PGame` represents the main class of the game. A game contains all the bricks and balls.
- `PAbstractElement` is the root of all the elements that may compose the game. This class is abstract.
- `PBall` describes a ball that may bounce horizontally or vertically.
- `PBrick` represents a brick. The brick is fixed and cannot be moved.
- `PRacket` represents a racket, controlled either by the mouse or a neural network.

### The PGame class

The `PGame` class is defined as:

```Smalltalk
Object subclass: #PGame
	instanceVariableNames: 'bricks balls isRunning rackets isAIPlaying gameArea'
	classVariableNames: ''
	package: 'NeuralNetwork-Pong'
```

A pong game contains a set of fixed bricks, typically the boundary walls and obstacles, kept in the variable `bricks`. It furthers contains bouncing balls (`balls`). A boolean value indicates whether the game is running, kept in `isRunning`. The set of controllable rackets is kept in `rackets`. These rackets can be steered either by neural networks or by the human player. The variable `isAIPlaying` contains a boolean value to that purpose. Finally, balls can bounce and leave the game area. The area is kept in `gameArea`.


Some of the variable are initialized in the `initialize` method:

```Smalltalk
PGame>>initialize
	super initialize.
	bricks := OrderedCollection new.
	rackets := OrderedCollection new.
	balls := OrderedCollection new.
	isRunning := true.
	
	"Human is playing"
	isAIPlaying := false
```

A new game has no wall, no rackets, no bouncing balls. It is set as running and the human controls the rackets (which will be added later on, when configuring the game).

At the end of the chapter we will see a few configuration of the game. To configure a game, we should be able to add elements in it. We can define three methods for that purpose:

```Smalltalk
PGame>>addBall: aBall
	"Add a ball in the game"
	balls add: aBall
```

Similarly, we can add a brick:

```Smalltalk
PGame>>addBrick: aBrick
	bricks add: aBrick
```

We also need to add a racket:

```Smalltalk
PGame>>addRacket: aRacket
	rackets add: aRacket.
	aRacket game: self
```

Note that a racket knows about the game it belongs. This is necessary to have access to the bouncing balls. A racket may access the balls using:

```Smalltalk
PGame>>balls
	"Return the balls in the game"
	^ balls
```
More than one ball could be in the game. This is a relevant information when recording the game. The number of balls is obtained with the following method:

```Smalltalk
PGame>>numberOfBalls
	"Return the number of balls in the game. This is important to collect the training data"
	^ balls size
```

In case that a ball escape from the game area, the game has to be reset, which as the effect to locate all the ball in their initial position:

```Smalltalk
PGame>>reset
	balls do: #reset
```

The beat of the game is performed using the method `beat`:

```Smalltalk
PGame>>beat
	rackets do: #beat.
	balls do: #beat.
	
	"If the game space is not set, then the game is not open.
	Probably we are in the test here"
	gameArea ifNil: [ ^ self ].
	(balls anySatisfy: [ :b | (gameArea containsPoint: b element position) not ])
		ifTrue: [ self reset ]
```

The first two lines of the method performs a beat of the balls and rackets. The remaining checks if a ball is outside the game area. If this is the case, then the game is reset.
	

The game may be configured along a number of options. First, the AI may be activated using:

```Smalltalk
PGame>>toggleAI
	isAIPlaying := isAIPlaying not.
	rackets do: #toggleAI.
	self inform: 'AI playing: ', isAIPlaying asString.
```

Activating the AI simply affects the rackets and the forwarding of the mouse event, as described in the following methods.

Pause is simply toggled using the method:

```Smalltalk
PGame>>togglePause
	isRunning := isRunning not.
	self inform: 'Pause: ', isRunning asString.
```

The remaining methods of `PGame` involve the graphical aspect of the game. The method `open` is meant to be sent once the game is configured. The method simply create a Roassal view and performs a number of configurations:

```Smalltalk
PGame>>open
	| v |
	v := RTView new.
	self configureBricksIn: v.
	self configureRacketsIn: v.
	self configureKeyboardIn: v.
	self configureBouncingBallsIn: v.
	self configureBeatIn: v.
	self configureGameAreaIn: v.
	^ v open
```

Bricks are easy to be configured. They are simply added to the view:

```Smalltalk
PGame>>configureBricksIn: v
	v addAll: (bricks collect: #element)
```

A brick, a ball, and a racket answer to the message `element`, which simply returns a visual Roassal element. Rackets are also added to the view, then the collision against a wall is configured: a racket cannot go through a wall. Roassal takes care of with with the class `RTCollision`.

```Smalltalk
PGame>>configureRacketsIn: v
	v addAll: (rackets collect: #element).
	(rackets collect: #element) asGroup @
			(RTCollision new
				elementToCheckForCollision: (bricks collect: #element)).
```

Mouse and keyboard events are then handled. First, the human is playing, then rackets should received the mouse event to make the rackets move. Some of the keys are mapped to some actions. In particular, the key `r` reset the game, space activate or deactivate the IA, `i` opens an inspector, useful to inspect the state of the game, such as the neural network. Events are configured as follows:

```Smalltalk
PGame>>configureKeyboardIn: v
	v when: TRMouseMove
		do: [ :evt | 
			isAIPlaying ifFalse: [ rackets do: [ :r | r receiveMouseEvent: evt ] ].
			v signalUpdate ].
	v when: TRKeyboardEvent
	  do: [ :evt | 
			evt keyValue == $r codePoint ifTrue: [ self reset ].
			evt keyValue == $  codePoint ifTrue: [ self toggleAI ].
			evt keyValue == $p codePoint ifTrue: [ self togglePause ].
			evt keyValue == $i codePoint ifTrue: [ self inspect ] ]
```

Balls should bounce vertically or horizontally. Here is the incantation to do so:

```Smalltalk
PGame>>configureBouncingBallsIn: v
	| coll |
	v addAll: (balls collect: #element).
	balls do: [ :b | 
			coll := RTCollision new.
			coll elementToCheckForCollision: (bricks, rackets collect: #element).
			coll
				callback: [ :aShape :side | 
					(#(#top #bottom) includes: side)
						ifTrue: [ b bounceVertically ]
						ifFalse: [ b bounceHorizontally ] ].
			b element @ coll ]
```

The beat of the game is implemented using a Roassal active animation. The block is evaluated at each screen refresh:

```Smalltalk
PGame>>configureBeatIn: v
	v addAnimation: (RTActiveAnimation new blockToExecute: [ 
		isAIPlaying ifTrue: [ rackets do: [ :b | b mouseX: balls first element position x ] ].
		isRunning ifTrue: [ self beat ] ])
```

Finally, we need to compute the game area:

```Smalltalk
PGame>>configureGameAreaIn: v
	gameArea := v elements encompassingRectangle
```



### The PAbstractElement class

The class `PAbstractElement` is the root class of all the element abstractions in the game. It is a rather simple class that contains the initial position and an initial color. 

```Smalltalk
Object subclass: #PAbstractElement
	instanceVariableNames: 'element initialColor initialPosition'
	classVariableNames: ''
	package: 'NeuralNetwork-Pong'
```

```Smalltalk
PAbstractElement>>color
	^ initialColor
```

```Smalltalk
PAbstractElement>>color: aColor
	initialColor := aColor
```

```Smalltalk
PAbstractElement>>position
	^ initialPosition
```
```Smalltalk
PAbstractElement>>position: aPoint
	initialPosition := aPoint
```

```Smalltalk
PAbstractElement>>element
	element ifNil: [ self createElement ].
	^ element
```

The class `PAbstractElement` also defines the `createElement` abstract methods, which means that subclasses have to implement `createElement`:

```Smalltalk
PAbstractElement>>createElement
	self subclassResponsibility 
```

### The PBall class

The class `PBall` describes a ball in the game. It has a speed, made of two variables `speedX` and `speedY`. It has also an initial size. It is defined as:

```Smalltalk
PAbstractElement subclass: #PBall
	instanceVariableNames: 'speedX speedY initialSize'
	classVariableNames: ''
	package: 'NeuralNetwork-Pong'
```

The `initialize` method set the ball red and a size of 5 pixels.

```Smalltalk
PBall>>initialize
	super initialize.
	self color: Color red.
	self position: 0 @ 0.
	self size: 5.
	self speedX: 0.
	self speedY: 0.
```

Bouncing horizontally negates the `speedX` value:

```Smalltalk
PBall>>bounceHorizontally
	speedX := speedX negated
```

Similarly, bouncing vertically negates the `speedY` value:

```Smalltalk
PBall>>bounceVertically
	speedY := speedY negated
```

The visual representation of the ball is an ellipse, with the same `height` and `width`:

```Smalltalk
PBall>>createElement
	element := RTEllipse new 
		width: self size;
		height: self size;
		color: self color;
		elementOn: self.
	element translateTo: self position.
	^ element
```

Reseting a ball moves it to its original position:

```Smalltalk
PBall>>reset
	self element translateTo: initialPosition.
```
As we have previously seen, a ball is reset when it leaves the game area.

The size has to be set and accessed:

```Smalltalk
PBall>>size
	^ initialSize 
```

```Smalltalk
PBall>>size: anInitialSize
	initialSize := anInitialSize  
```

Similarly, the components of the speed have to be accessed and set:

```Smalltalk
PBall>>speedX
	^ speedX
```
```Smalltalk
PBall>>speedX: anInteger
	speedX := anInteger
```
```Smalltalk
PBall>>speedY
	^ speedY
```

```Smalltalk
PBall>>speedY: anInteger
	speedY := anInteger
```

At each beat, the ball moves by its speed:

```Smalltalk
PBall>>beat
	element ifNil: [ ^ self ].
	element translateBy: speedX @ speedY
```

### The PBrick class

The `PBrick` class describes a wall, or any kind of obstacles. 

```Smalltalk
PAbstractElement subclass: #PBrick
	instanceVariableNames: 'initialWidth initialHeight'
	classVariableNames: ''
	package: 'NeuralNetwork-Pong'
```

When built, a brick is a square-like gray obstacle:

```Smalltalk
PBrick>>initialize
	super initialize.
	self color: Color gray.
	self position: 0 @ 0.
	self width: 20.
	self height: 20.
```

Similarly as earlier, the method `createElement` has to return a Roassal visual element:

```Smalltalk
PBrick>>createElement
	element := RTBox new 
		width: self width;
		height: self height;
		color: self color;
		elementOn: self.
	element translateTo: self position.
	^ element
```

We also need a few methods to set the brick dimension:
```Smalltalk
PBrick>>height
	^ initialHeight
```
```Smalltalk
PBrick>>height: anInteger
	initialHeight := anInteger
```

```Smalltalk
PBrick>>width
	^ initialWidth
```
```Smalltalk
PBrick>>width: anInteger
	initialWidth := anInteger
```

### The PRacket class

The class `PRacket` is the last class to be implemented before being able to play the pong game. To keep the code short, we make the class `PRacket` a subclass of `PBrick`. Conceptually, this may not be true (_is a racket an obstacle?_). The class is defined as follows:

```Smalltalk
PBrick subclass: #PRacket
	instanceVariableNames: 'isLearning game records network mouseX minimums maximums'
	classVariableNames: ''
	package: 'NeuralNetwork-Pong'
```

A racket has a boolean variable `isLearning`, indicating whether the racket is controlled by a human, and therefore is gathering some data. It also has a reference to the game, with is useful to access the balls. The `records` variable is a collection containing the records. This variable contains what the neural network has to learn. The variable `records` will be used in the method `record`, `records`, and `trainNeuralNetwork`, described later on. 
The variable `mouseX` represents the X-component of the mouse cursor. This is useful when a human steers the racket. The `minimums` and `maximums` variables are two collections containing the minimum and maximum of the records kept in `records`. This is useful to perform the data normalization of the records.

```Smalltalk
PRacket>>initialize
	super initialize.
	isLearning := true.
	records := OrderedCollection new.
	mouseX := 0.
```
```Smalltalk
PRacket>>game: aGame
	game := aGame
```
```Smalltalk
PRacket>>game
	^ game
```

```Smalltalk
PRacket>>isLearning
	^ isLearning 
```
```Smalltalk
PRacket>>mouseX: aValue
	mouseX := aValue 
```
```Smalltalk
PRacket>>receiveMouseEvent: anEvent
	isLearning ifFalse: [ ^ self ].
	mouseX := (anEvent position x / 3) asInteger * 3
```

```Smalltalk
PRacket>>record
	| record |
	record := OrderedCollection new.
	
	record add: self element position x.
	game balls do: [ :ball | 
		record add: ball element position x.
		record add: ball element position y.
		record add: ball speedX.
		record add: ball speedY. ].

	element position x < mouseX ifTrue: [ record add: 2 ].
	element position x > mouseX ifTrue: [ record add: 1 ].
	element position x = mouseX ifTrue: [ record add: 0 ].
	records add: record
```
```Smalltalk
PRacket>>records
	^ records
```
```Smalltalk
PRacket>>step
	"If it is learning, then there is nothing to do"
	| record whatShouldDo normalizedRecord |
	isLearning ifTrue: [ self record. self translateToX: mouseX. ^ self ].
	
	"Else, we use the neural network"
	record := OrderedCollection new.
	record add: (self element position x / 3) asInteger * 3.
	self game balls do: [ :ball | 
		record add: ball element position x.
		record add: ball element position y.
		record add: ball speedX.
		record add: ball speedY. ].

	""
	minimums isNil ifTrue: [ ^ self ].
	normalizedRecord := Normalization new normalizeData: (Array with: record) min: minimums max: maximums nbColumns: record size.

	whatShouldDo := network predict: normalizedRecord first.	
	whatShouldDo = 1 ifTrue: [ element translateBy: -3 @ 0 ].
	whatShouldDo = 2 ifTrue: [ element translateBy: 3 @ 0 ].
```

```Smalltalk
PRacket>>toggleBehavior
	isLearning := isLearning not.
	isLearning ifFalse: [ self trainNeuralNetwork ]
```
```Smalltalk
PRacket>>trainNeuralNetwork
	network := NNetwork new configure: (game numberOfBalls * 4 + 1) hidden: 8 nbOfOutputs: 3.
	records ifEmpty: [ ^ self ].
	network train: (Normalization new normalizeData: records) nbEpoch: 500.
	
	"We need to remember the maximums and minimums"
	minimums := Array new: records last size - 1 withAll: SmallInteger maxVal.
	maximums := Array new: records last size - 1 withAll: SmallInteger minVal.
	records do: [ :row |
		1 to: row size - 1 do: [ :i |
			minimums at: i put: ((minimums at: i) min: (row at: i)).
			maximums at: i put: ((maximums at: i) max: (row at: i)).
		]
	]
```
```Smalltalk
PRacket>>translateToX: anX
	element position x < anX ifTrue: [ element translateBy: 3 @ 0 ].
	element position x > anX ifTrue: [ element translateBy: -3 @ 0 ].
```

## Game examples

```Smalltalk
PGame>>run1
	<script: 'PGame new run1.'>

	| g gameSize |
	g := PGame new.
	gameSize := 300.
	g addFixedBrick: (PBrick new position: gameSize/2 @ 0; width: gameSize; height: 20). 
	g addFixedBrick: (PBrick new position: 0 @ (gameSize/2); width: 20; height: gameSize).
	g addFixedBrick: (PBrick new position: gameSize @ (gameSize/2); width: 20; height: gameSize).
	g addMouseHoriBrick: (PRacket new position: gameSize/2 @ (gameSize + 10); width: 50; height: 10; color: Color red).
	g addBall: (PBall new color: Color blue; position: (gameSize/2) @ (gameSize/2); speedX: 2; speedY: 5).
	^ g open
```

```Smalltalk
PGame>>run2
	<script: 'self new run2'>

	| g gameSize |
	g := PGame new.
	gameSize := 300.
	g addFixedBrick: (PBrick new position: gameSize/2 @ 0; width: gameSize; height: 20). 
	g addFixedBrick: (PBrick new position: 0 @ (gameSize/2); width: 20; height: gameSize).
	g addFixedBrick: (PBrick new position: gameSize @ (gameSize/2); width: 20; height: gameSize).
	g addMouseHoriBrick: (PRacket new position: gameSize/2 @ gameSize; width: 50; height: 10; color: Color red).
	g addMouseHoriBrick: (PRacket new position: gameSize/2 @ (gameSize/2); width: 50; height: 10; color: Color red).
	g addBall: (PBall new color: Color blue; position: (gameSize/2) @ (gameSize/2); speedX: 2; speedY: 5).
	^ g open
```

```Smalltalk
PGame>>run4
	<script: 'PGame new run4. true'>

	| g gameSize |
	g := PGame new.
	gameSize := 300. 
	g addFixedBrick: (PBrick new position: 0 @ (gameSize/2); width: 20; height: gameSize).
	g addFixedBrick: (PBrick new position: gameSize @ (gameSize/2); width: 20; height: gameSize).
	g addMouseHoriBrick: (PRacket new position: gameSize/2 @ gameSize; width: 50; height: 10; color: Color red).
	g addMouseHoriBrick: (PRacket new position: gameSize/2 @ 0; width: 50; height: 10; color: Color red).
	g addBall: (PBall new color: Color blue; position: (gameSize/2) @ (gameSize/2); speedX: 2; speedY: 5).
	^ g open
```




