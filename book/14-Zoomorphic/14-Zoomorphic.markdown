
# Simulating Zoomorphic Creature

This chapter is about creating zoomorphic creatures. Such creature will have to solve a simple task, such as moving toward a direction or jumping.

![Example of a creature.](14-Zoomorphic/figures/CreatureExample.png){#fig:CreatureExample}

Figure @fig:CreatureExample gives the example of such a creature. It is make of join points and muscles. Each muscles has two extremities and each extremity is connected a join point. A join point may be host of several extremities. 

A muscle is a complex element in our model. Each muscle oscillates and has a strength, which makes it able to resist against external forces. Each muscle oscillate along its internal clock. 

A creature is subject to the gravity. Muscles have no weight, but a join point has a weight. 

Such a creature, at its inception, does not do much. However, we will make it evolve to solve a particular task. The task we will consider are displacing itself toward a direction and jumping.

The chapter is (unfortunately) very long as it lays out the infrastructure to simulate a complex. We will produce creature, that we qualify as _zoomorphic_ since they could be (although remotely) considered as a small digital animal.

## Modeling Join Points

Each element in our simulation has a visual representation. We will therefore define the class `CVisualElement`:

```Smalltalk
Object subclass: #CVisualElement
	instanceVariableNames: 'element'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

The class `CVisualElement` has a variable `element` which will contains a Roassal visual element. This element will have to be created by subclasses of the our class using the method `createElement`:

```Smalltalk
CVisualElement>>createElement
	"Should be overridden in subclasses. The method should
	initialize the element variable"
	self subclassResponsibility 
```

The variable `element` may be accessed using:

```Smalltalk
CVisualElement>>element
	"Return the Roassal element"
	^ element
```

Join points will be represented as instance of the class `CNode`. We define the class as follows:

```Smalltalk
CVisualElement subclass: #CNode
	instanceVariableNames: 'speedVector force isOnPlatform'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

A node has three variables:
- `force` represents the sum of the external forces. This vector will impact the speed and therefore the movement of the node;
- `speedVector` represents the speed of the node. The speed will be affected by friction of the environment;
- `isOnPlatform` indicates whether a node is on a platform. Knowing this is relevant because it may affect the speed of the node. 

The initialization of a node is defined as follows:

```Smalltalk
CNode>>initialize
	super initialize.
	isOnPlatform := false.
	self resetSpeed.
	self resetForce
```

When created, a node has no speed, it is not on a platform, and has no external force.

The external force may be reset using the method:

```Smalltalk
CNode>>resetForce
	"Reset the force exercising on the node"
	force := 0 @ 0
```

Similarly, the speed is reset using the method:

```Smalltalk
CNode>>resetSpeed
	"Make the node stop by canceling its speed"
	speedVector := 0 @ 0
```

A node is visually represented with a small gray circle. We therefore define the method `createElement`:

```Smalltalk
CNode>>createElement
	element := RTEllipse new size: 10; 
				color: Color gray trans; element.
	element @ RTDraggable	
```

A node is subject to external forces. We define the method `addForce:` that simply adds a force, expressed as a point, to the forces already exercising on the node. The method is:

```Smalltalk
CNode>>addForce: aForceAsPoint
	"Make the node subject of an external force"
	force := force + aForceAsPoint
```

Overall, our simulation is driven by a beat, which will be globally triggered. The method `beat` is defined as follows:

```Smalltalk
CNode>>beat
	"Make the node act according to the force and speed acting on the node"
	speedVector := speedVector + self gravityForce + force.
	speedVector := speedVector * 0.9.
	isOnPlatform ifTrue: [ 
		speedVector := speedVector x * 0.3 @ speedVector y ].
	self translateBy: speedVector
```

At each beat, the gravity and the external forces are summed to the speed. We arbitrary set a friction, which is the result of the air in the physical environment. 

The gravity is represented by an arbitrary point:

```Smalltalk
CNode>>gravityForce
	"A fixed force representing a gravity"
	^ 0 @ 0.3
```

Creature will live in a world made of platforms. A platform is an instance of the class `CPlatform`, which we will see later. We define the method `checkForCollision:` as follows:

```Smalltalk
CNode>>checkForCollision: platforms
	"Verify if the node is on a platform. If it is the case, 
	the variable isOnPlatform is set to true"
	isOnPlatform := false.
	platforms
		do: [ :p | 
			(p touch: self)
				ifTrue: [ 
					speedVector := speedVector x @ 0.
					p adjustNodeIfNecessary: self.
					isOnPlatform := true.
					^ self ] ]
```

First the variable `isOnPlatform` is set to `false`. If the node touches at least one platform, then the Y component of the speed is set to 0 and the variable `isOnPlatform` is set to `true`. Due to some impression of our model, we need to let the platform make some adjustment of the node. In particular, it ensures that a node is not _inside_ a platform. We will see this method later, when we will define the class `CPlatform`.

The variable `isOnPlatform` may be accessed using the accessor: 

```Smalltalk
CNode>>isOnPlatform
	"Is the node is on a platform?"
	^ isOnPlatform
```

The position of the node is given by the method `position`. It simply asks to the Roassal `element` its position. A newly created node is at a position `0@0`. The method is defined as follows:

```Smalltalk
CNode>>position
	"Return the position of the node"
	^ element position
```

A node needs to be translated to reflect the effect of the environment. We define a first method to translate the node by an incremental step using the method:

```Smalltalk
CNode>>translateBy: aPoint
	"Translate the node by an incremental point"
	element translateBy: aPoint.
```

A new position may be set to a node using the method:

```Smalltalk
CNode>>translateTo: aPoint
	"Translate the node to a new position"
	element translateTo: aPoint.
```

## Platform

In addition to the gravity that we have described above, the environment may affect the nodes (and therefore the creatures) with platforms. We define the class `CPlatform` as a subclass of `CVisualElement`:

```Smalltalk
CVisualElement subclass: #CPlatform
	instanceVariableNames: 'width'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

We initialize a platform with a width of 1000 pixels:

```Smalltalk
CPlatform>>initialize
	super initialize.
	width := 1000.
```

The creation of the element is a simple rectangle:

```Smalltalk
CPlatform>>createElement
	element := RTBox new width: width; height: 10; color: Color gray; element.
	element @ RTDraggable	
```

The primitive to handle effect of the platform is the collision detection. We define the method `touch:`:

```Smalltalk
CPlatform>>touch: node
	"Answer whether the platform touch the node provided as argument"
	| bottomNode topPlatform |
	bottomNode := node element encompassingRectangle bottomCenter y.
	topPlatform := self element encompassingRectangle topCenter y.
	^ topPlatform <= bottomNode
```

The method `touch:` returns `true` or `false` indicating whether the provided `node` is above a platform.

In case that a collision happens, it is important to adjust the position of a node if necessary:

```Smalltalk
CPlatform>>adjustNodeIfNecessary: node
	"Answer whether the platform touch the node"
	| bottomNode topPlatform |
	bottomNode := node element encompassingRectangle bottomCenter y.
	topPlatform := self element encompassingRectangle topCenter y.

	topPlatform < bottomNode 
		ifTrue: [ node translateBy: 0 @ (topPlatform - bottomNode) ]
```

Such an adjustment is necessary because the node translation is discrete and not continuous. As a consequence, it may happens that a falling node is within the platform, and should therefore be translated to be on top of it.

## Muscle

A muscle, which is at the core of our simulation, is a complex data structure. A muscle is an oscillating edge, having a strength. It connects two join points (_i.e._, nodes). We will first define the class `CConnection` to represent the connection between two nodes. We define the class `CConnection`:


```Smalltalk
CVisualElement subclass: #CConnection
	instanceVariableNames: 'node1 node2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

We define `node1` and `node2` the two extremities represented by instance of `CNode`. The first extremity is obtained using:

```Smalltalk
CConnection>>node1
	^ node1
```

The first extremity is set using the method:

```Smalltalk
CConnection>>node1: aNode
	node1 := aNode
```

The second extremity is obtained with:

```Smalltalk
CConnection>>node2
	^ node2
```

It is set using:

```Smalltalk
CConnection>>node2: aNode
	node2 := aNode
```

A muscle has an internal timer that drives the oscillation. We define the class `CMuscle` as follows:

```Smalltalk
CConnection subclass: #CMuscle
	instanceVariableNames: 'time length1 length2 time1 time2 strength color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

The class `CMuscle` has the following variables:

- the `strength` represents how much resistance a muscle has when subject to external forces, such as the weights of the connected join points;
- the length of the muscle oscillate between `length1` and `length2`;
- an internal timer, represented with the variable `time`;
- two thresholds used by the internal clock, `time1` and `time2`;
- a `color` variable representing the color of the muscle.

A muscle's length oscillates along its internal timer. The method `beat` increases the variable `time` by 1. 

```Smalltalk
CMuscle>>beat
	"Beating a muscle increases its timer"
	time := time + 1.
	time = self maxTime ifTrue: [ time := 0 ].
```

If the time reaches a maximum time, then it is reset to 0. The internal timer is therefore cyclic. When created, a muscle as 0 as a timer. We define the initialization:

```Smalltalk
CMuscle>>initialize
	super initialize.
	time := 0.
	color := Color red.
```

The visual representation of a muscle is given by the method `createElement`. A muscle is a straight line joining `node1` and `node2`. The method `createElement` is defined as:

```Smalltalk
CMuscle>>createElement
	"A muscle is a transparent line between the two nodes"
	element := RTLine new color: (color alpha: 0.3); width: 5; 
				edgeFrom: node1 element to: node2 element
```

A muscle as a variable length. The actual length of a muscle is either `length1` or `length2`. If the muscle timer is below a lower threshold (_i.e.,_ value of `self minTime`), then the muscle length is `length1`, else it is `length2`. We define the method `length` as follows:

```Smalltalk
CMuscle>>length
	"Maybe rename it to ideal length"
	^ time < self minTime
		ifTrue: [ length1 ]
		ifFalse: [ length2 ]
```

If the `time` variable has a value lower than the lower threshold, then we say we are at the beginning of a cycle. We refer to the end of the muscle cycle if `time` is greater than `self minTime`. We define some accessing methods to the variable `length1` and `length2`:

```Smalltalk
CMuscle>>length1
	"Length of a muscle at the beginning of a cycle"
	^ length1
```

The value is set using:

```Smalltalk
CMuscle>>length1: aLengthAsInteger
	"Set the muscle length at the beginning of a cycle"
	length1 := aLengthAsInteger
```

Similarly, `length2` is accessed using:

```Smalltalk
CMuscle>>length2
	"Length of a muscle at the end of a cycle"
	^ length2
```

The second length is set using:

```Smalltalk
CMuscle>>length2: aLengthAsInteger
	length2 := aLengthAsInteger
```

The cycle length is given by the value of `maxTime`, it is defined as the maximum value between `time1` and `time2`:


```Smalltalk
CMuscle>>maxTime
	"Return the cycle length"
	^ time1 max: time2
```

Similarly, the threshold is given by the method `minTime`:

```Smalltalk
CMuscle>>minTime
	"Return the timer threshold between to switch between length1 and length2"
	^ time1 min: time2
```

A muscle as a strength, accessible using:

```Smalltalk
CMuscle>>strength
	"Return the strength of the muscle"
	^ strength
```

The strength of a muscle is used to compute the forces that will be applied to the extremity nodes by the muscle. The strength is set using:

```Smalltalk
CMuscle>>strength: strengthAsFloat
	"Set the strength that is applied to the extremities"
	strength := strengthAsFloat
```

The muscle internal timer is increase at each beat, as defined above. The first timer threshold is set using:

```Smalltalk
CMuscle>>time1: anInteger
	time1 := anInteger
```

The `time1` value is obtained with:

```Smalltalk
CMuscle>>time1
	^ time1
```

As we will later see, muscle attributes have to be serialized in order to be encoded and decoded from GA individuals. We therefore need to access these values. The second time threshold is set using:

```Smalltalk
CMuscle>>time2: anInteger
	time2 := anInteger
```

It is accessed using:

```Smalltalk
CMuscle>>time2
	^ time2
```

Each creature will be randomly generated. We will have to monitor how nodes are used by the muscles. We define the method to test whether a muscle connects two nodes:

```Smalltalk
CMuscle>>usesNodes: twoNodes
	"The method accepts an array of two nodes as argument.
	Return true if the muscle connects the two nodes."
	^ (node1 == twoNodes first and: [ node2 == twoNodes second ]) or: [ 
		node1 == twoNodes second and: [ node2 == twoNodes first ] ]
```

The definition of a muscle is now complete. Our creature will be randomly generated, which means that muscle will have to be also randomly generated. The next section defines a generator of muscles.

## Muscle Generator

When a muscle is randomly generated, attributes defining the muscle (_i.e.,_ `time1`, `time2`, `length1`, `length2`, `strength`) have to be also randomly generated. The class `CMuscleGenerator` has the responsibility to generate random muscles. A muscle generator is parameterized with a range for each attribute, expressed with a minimum value and a delta value. We define the class `CMuscleGenerator`:

```Smalltalk
Object subclass: #CMuscleGenerator
	instanceVariableNames: 'random minStrength deltaStrength minLength deltaLength minTime deltaTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

The initialization of a generator assign some values that are convenient in most of the example we will later see:

```Smalltalk
CMuscleGenerator>>initialize
	super initialize.
	random := Random seed: 42.
	
	minLength := 10.
	deltaLength := 30.
	minTime := 4.
	deltaTime := 200.
	minStrength := 1.
	deltaStrength := 3
```

The delta of a value corresponds to an interval from which values will be randomly picked. The delta length is set using:

```Smalltalk
CMuscleGenerator>>deltaLength: anInteger
	deltaLength := anInteger
```

The delta strength is set using:

```Smalltalk
CMuscleGenerator>>deltaStrength: anInteger
	deltaStrength := anInteger
```

The delta time is set using:

```Smalltalk
CMuscleGenerator>>deltaTime: anInteger
	deltaTime := anInteger
```

The minimum value a length can have is set using:

```Smalltalk
CMuscleGenerator>>minLength: anInteger
	"Set the minimum value a muscle length may have"
	minLength := anInteger
```

Similarly, the minimum strength is set:

```Smalltalk
CMuscleGenerator>>minStrength: anInteger
	"Set the minimum value a muscle strength can have"
	minStrength := anInteger
```

The minimum time threshold is set using:

```Smalltalk
CMuscleGenerator>>minTime: anInteger
	"Set the minimum value a muscle time threshold can be"
	minTime := anInteger
```

A length is generated using a dedicated method:

```Smalltalk
CMuscleGenerator>>generateLength
	"Return a length within the specified range"
	^ minLength + (random nextInt: deltaLength)
```

Similarly, the strength is generated with:

```Smalltalk
CMuscleGenerator>>generateStrength
	"Return a strength within the specified range"
	^ random next * deltaStrength + minStrength
```

A time threshold is generated with:

```Smalltalk
CMuscleGenerator>>generateTime
	"Return a time within the specified range"
	^ (random nextInt: deltaTime) + minTime
```

A central method of the generator is `createMuscleFrom:to:`. This method is used to produce a muscle between two nodes:

```Smalltalk
CMuscleGenerator>>createMuscleFrom: aNode to: anotherNode
	"Return a new muscle connecting two nodes"
	| m |
	m := CMuscle new.
	m node1: aNode.
	m node2: anotherNode.
	m length1: self generateLength.
	m length2: self generateLength.
	m time1: self generateTime.
	m time2: self generateTime.
"	m time2: 200. @@TODO: CHECK THIS"
	m strength: self generateStrength.
	^ m
```

A central aspect of applying a genetic algorithm to search for optimal muscle configuration require adequately managing the mapping between a set of values and a muscle definition. A muscle can be _serialized_ into a set of values, and a set of values can be _materialized_ into a muscle. These operations are necessary to produce a creature from a given set individual from our genetic algorithm.

```Smalltalk
CMuscleGenerator>>serializeMuscle: aMuscle
	"Return an array describing the muscle provided as argument"
	^ Array
		with: aMuscle length1
		with: aMuscle length2
		with: aMuscle strength
		with: aMuscle time1
		with: aMuscle time2
```

The materialization configures a provided muscle with an array of values

```Smalltalk
CMuscleGenerator>>materialize: values inMuscle: aMuscle
	"Configure the provided muscle with some values"
	aMuscle length1: values first.
	aMuscle length2: values second.
	aMuscle strength: values third.
	aMuscle time1: values fourth.
	aMuscle time2: values fifth
```

An individual within our genetic algorithm will contain the attributes of all the muscles involved within a creature. Our algorithm will need a way to produce a particular value for a given gene position in the individual genetic information. The following method address this need:


```Smalltalk
CMuscleGenerator>>valueForIndex: anIndex
	"Produce a value for a given index. 
	This method is used to generate a gene in the GA algorithm"
	| i |
	i := (anIndex - 1) % 5.
	i = 0 ifTrue: [ ^ self generateLength ].
	i = 1 ifTrue: [ ^ self generateLength ].
	i = 2 ifTrue: [ ^ self generateStrength ].
	i = 3 ifTrue: [ ^ self generateTime ].
	i = 4 ifTrue: [ ^ self generateTime ].
	self error: 'Should not be here'
```

It is important to note that the three methods `serializeMuscle:`, `materialize:inMuscle:`, and `valueForIndex:`, heavily rely on the order of the attributes.

Our muscle generator is now complete. We will be able to assemble nodes, muscles, and muscle generators in 

<!--
```Smalltalk
CMuscleGenerator>>random: randomNumberGenerator
	"Set the random number generated used to produce muscles"
	random := randomNumberGenerator
```
-->

## Creature

We define a creature as an instance of the class `CCreature`, defined as follow:

```Smalltalk
Object subclass: #CCreature
	instanceVariableNames: 'nodes muscles random muscleGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

A creature is essentially made of a set of nodes and a set of muscle. We will run our genetic algorithm on optimizing the configuration of the muscles. So, the initial configuration of our muscle is random, which is why we need a `random` number generator. Muscles are a complex structure which require a dedicated object to be built. As we have seen, the class `CMuscleGenerator` is a convenient way to build muscles.

A creature is initialized as follow:

```Smalltalk
CCreature>>initialize
	super initialize.
	nodes := OrderedCollection new.
	muscles := OrderedCollection new.
	random := Random seed: 42.
	muscleGenerator := CMuscleGenerator new.
```

Muscle and can be generated and added in a creature using the method:

```Smalltalk
CCreature>>addMuscleFrom: aNode to: anotherNode
	"Generate and add a muscle between two nodes"
	muscles add: (muscleGenerator createMuscleFrom: aNode to: anotherNode)
```

Each beat produces a beat for each node and each muscle. After, the physic rules have to be applied between the muscles and the nodes. The method `beat` is defined as:

```Smalltalk
CCreature>>beat
	"Execute a unit of behavior"
	nodes do: #beat.
	muscles do: #beat.
	self reachStable
```

Collision between a creature and the platforms is achieved using the method `checkForCollision:`, defined as follows:

```Smalltalk
CCreature>>checkForCollision: platforms
	nodes do: [ :n | n checkForCollision: platforms ].
	self simulateNoise.
```

The physic engine we are implementing is minimal and is far from being complete. We need to add some noise in the way that the physical is simulated. For example, random noise accompanying the physic is not considered so far. However, such a noise is necessary to avoid singular situations, for example if all the nodes are exactly at the vertical level. We simply add some noise by moving a node randomly:

```Smalltalk
CCreature>>simulateNoise
	"Produce noise in our simulation"
	| direction |
	direction := ((random nextInt: 3) - 2) @ ((random nextInt: 3) - 2). 
	(nodes atRandom: random) translateBy: direction
```

All the necessary to model creature is now in place. The next section focuses on the creation of creature.

## Creating Creature

Even if we will produce creatures with simple shapes, manually creating creature is tedious. We define some dedicated methods. Adding nodes to a creature is achieved with the method `configureNodes:`:

```Smalltalk
CCreature>>configureNodes: nbNodes
	"Add a number of nodes in our creature"
	nbNodes timesRepeat: [ nodes add: CNode new createElement ]
```


A ball-like shape is created using:

```Smalltalk
CCreature>>configureBall: numberOfNodes	
	"Produce a ball-like creature"
	muscleGenerator := CMuscleGenerator new
		minStrength: 0.01;
		deltaStrength: 0.5;
		minLength: 10;
		deltaLength: 80;
		deltaTime: 200;
		minTime: 20.

	"Add some nodes nodes"
	self configureNodes: numberOfNodes.

	"Connect each node with all the other nodes"
	nodes do: [ :n1 |
		(nodes copyWithout: n1) do: [ :n2 |
			self addMuscleFrom: n1 to: n2. ] ].

	"Create the visual elements"
	self createElements.
	self randomlyLocateNodes
```

The `configureBall:` takes as argument the number of nodes that will compose the ball. All the nodes are connected with all the other nodes. As a consequence, a ball creature will contains many muscles, which means that muscles should have a low strength.

A more generic way of defining a creature is by specifying the number of nodes and the number of muscles. The method `configureNbNodes:nbMuscles:` is defined as follows:

```Smalltalk
CCreature>>configureNbNodes: nbNodes nbMuscles: nbMuscle
	"Configure a creature with a given number of nodes and muscles."
	| n1 n2 tryNewNode1 tryNewNode2 |
	self configureNodes: nbNodes.
	nbMuscle timesRepeat: [ 
			n1 := nodes atRandom: random.
			n2 := n1.
			tryNewNode1 := 0.
			tryNewNode2 := 0.
			[ tryNewNode1 < 10
				and: [ n2 == n1 or: [ muscles anySatisfy: [ :mm | mm usesNodes: { n1 . n2 } ] ] ] ]
				whileTrue: [ [ tryNewNode2 < 10
						and: [ n2 == n1
								or: [ muscles anySatisfy: [ :mm |  mm usesNodes: { n1 . n2 } ] ] ] ]
						whileTrue: [ tryNewNode2 := tryNewNode2 + 1.
							n2 := nodes atRandom: random ].
					tryNewNode2 = 10
						ifTrue: [ tryNewNode1 := tryNewNode1 + 1.
							tryNewNode2 := 0.
							n1 := nodes atRandom: random ] ].
			self addMuscleFrom: n1 to: n2 ].
		
	self createElements.
	self randomlyLocateNodes
```

The method first define some nodes. Some constraints need to be set on the nodes that are joined by a muscle. In particular, we cannot have more than one muscle between two nodes. The small algorithm used in the method avoid nodes in which there is at a muscle between the two nodes. In total, 10 tries are performed before giving up.

We also provide a way to build worm-like creature. We define the following methods

```Smalltalk
CCreature>>configureWorm: length
	"Create a work with the specified length"
	| lastTwoNodes n1 n2 index |
	"HEAD"
	nodes add: CNode new.
	nodes add: CNode new.
	lastTwoNodes := nodes last: 2.
	self addMuscleFrom: lastTwoNodes first to: lastTwoNodes second.
	
	length timesRepeat: [
		n1 := CNode new.
		n2 := CNode new.
		nodes add: n1.
		nodes add: n2.
		self addMuscleFrom: lastTwoNodes first to: n1.
		self addMuscleFrom: lastTwoNodes second to: n1.
		self addMuscleFrom: lastTwoNodes first to: n2.
		self addMuscleFrom: lastTwoNodes second to: n2.
		self addMuscleFrom: n1 to: n2.
		lastTwoNodes := { n1 . n2 } ].

	"We create the elements"
	self createElements.

	"Position the nodes to give the shape of a worm"
	index := 0.
	nodes pairsDo: [ :aNode :aSecondNode | 
		aNode translateBy: (index * 10) @ 0. 
		aSecondNode translateBy: (index * 10) @ 10.
		index := index + 1 ]
```

The graphical elements are created using the method `createElements`:

```Smalltalk
CCreature>>createElements
	"Force the creation of the all graphical elements for nodes and muscles"
	nodes do: #createElement.
	muscles do: #createElement.
```

Nodes and muscles are subject to the Newtonian physical laws, defined using the method:

```Smalltalk
CCreature>>reachStable
	"Apply the physical law on a creature"
	| n1 n2 delta actualLength unit force |
	nodes do: #resetForce.
	muscles do: [ :m |
		n1 := m node1.
		n2 := m node2.
		delta := n2 position - n1 position.
		actualLength := delta r max: 1.
		unit := delta / actualLength.
		force := 0.1 * m strength * (actualLength - m length) * unit.
		n1 addForce: force.
		n2 addForce: force negated ].
```

External forces on nodes are first canceled. We then compute the force from the strength of a muscle. Note that this force is applied to a node, and the opposite force to the second extremity node. 

### Serialization and materialization of creature

When we will hook our genetic algorithm, it is crucial to transform the individual genetic information as an array of numbers. Such numbers will therefore represent the attributes of each muscles of the creature. A creature is serialized using the method:

```Smalltalk
CCreature>>serialize
	"Serialize the creature into an array of numbers"
	^ (muscles
		flatCollect: [ :m | 
			muscleGenerator serializeMuscle: m ]) asArray
```

The opposite operation, the materialization of a creature from a set of numerical values, is carried out by the method:

```Smalltalk
CCreature>>materialize: anArrayOfValues
	"Materialize a array of numbers into a creature"
	| valuesPerMuscles |
	valuesPerMuscles := anArrayOfValues groupsOf: 5 atATimeCollect: [ :v | v ].
	muscles with: valuesPerMuscles do: [ :m :values | 
		muscleGenerator materialize: values inMuscle: m ]
```

### Accessors and utility methods

The largest part of the creature definition has been presented above. A new accessors and utility methods are necessary:


@@ CHECK IF NECESSARY
```Smalltalk
CCreature>>muscleGenerator
	^ muscleGenerator
```

```Smalltalk
CCreature>>muscleGenerator: aMuscleGenerator
	muscleGenerator := aMuscleGenerator
```

```Smalltalk
CCreature>>muscles
	^ muscles 
```

```Smalltalk
CCreature>>nodes
	^ nodes
	
```

The position of the creature is computed as the average position of the nodes. Knowing the position of the creature is necessary when we will apply the genetic algorithm. The fitness will be based on the distance of the creature. The method `position` is defined as:

```Smalltalk
CCreature>>position
	"Return the position of the creature, as the average position of the nodes"
	^ (self nodes collect: #position) sum / self nodes size
```

At the beginning of a simulation, the creature has to be located above the main platform. We move the creature at an arbitrary position `0 @ -50`:

```Smalltalk
CCreature>>resetPosition
	"Locate the creature at an arbitrary position"
	self translateTo: 0 @ -50
```

Before applying the physical rules, it is important that the nodes are not all at the same position. We randomly assign a position to each node using `randomlyLocateNodes`:

```Smalltalk
CCreature>>randomlyLocateNodes
	"Assign each node to a random position"
	nodes
		do: [ :n | n translateBy: (random nextInt: 50) @ (random nextInt: 50) ]
```

Translating the creature to a given position is achieved using the method:

```Smalltalk
CCreature>>translateTo: aPoint
	| averageCenter delta |
	averageCenter := self position.
	delta := aPoint - averageCenter.
	self nodes do: [ :n | n translateBy: delta ]
```

## Defining the World

```Smalltalk
CVisualElement subclass: #CWorld
	instanceVariableNames: 'creatures time platforms'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Creature'
```

```Smalltalk
CWorld>>addCreature: aCreature
	creatures add: aCreature.
	element addAll: (aCreature nodes collect: #element).
	element addAll: (aCreature muscles collect: #element).
```

```Smalltalk
CWorld>>addPlatform: aPlatform
	platforms add: aPlatform.
	aPlatform createElement.
	element add: aPlatform element.
```

```Smalltalk
CWorld>>beat	
	time := time + 1.
	creatures do: [ :c | c beat; checkForCollision: platforms ]
```

```Smalltalk
CWorld>>createElement
	element := RTView new.
```

```Smalltalk
CWorld>>initialize
	super initialize.
	creatures := OrderedCollection new.
	platforms := OrderedCollection new. 
	time := 0.
	self createElement
```

```Smalltalk
CWorld>>open
	| lbl |
	creatures do: #resetPosition.

	lbl := (RTLabel new elementOn: time) setAsFixed; yourself.
	element add: lbl.
	lbl translateBy: 80 @ 30.
	element
		addAnimation:
			(RTActiveAnimation new
				intervalInMilliseconds: 10;
				blockToExecute: [ 
					self beat.
					lbl trachelShape text: time asString.
					element signalUpdate ]).
	^ element open
```







```Smalltalk
GAAbstractCrossoverOperation subclass: #GAConstrainedCrossoverOperation
	instanceVariableNames: 'possibleCutpoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

```Smalltalk
GAConstrainedCrossoverOperation>>pickCutPointFor: partnerA
	"Argument is not used now. Maybe we can improve that"
	self assert: [ possibleCutpoints notNil ] description: 'Need to provide the possible cut points, using #possibleCutpoints:'.
	^ possibleCutpoints at: (random nextInt: possibleCutpoints size)
```

```Smalltalk
GAConstrainedCrossoverOperation>>possibleCutpoints
	^ possibleCutpoints
```

```Smalltalk
GAConstrainedCrossoverOperation>>possibleCutpoints: indexes
	possibleCutpoints := indexes
```


## What have we seen in this chapter?


Note that we could have added bones in the way we model creature. Once we have the notion of bone, we could have build skeletons. Although appealing, it would have significantly increased the amount of source code.


<!--
```Smalltalk
CCreature>>addMuscleFromIndex: n1 toIndex: n2

	muscles add: (muscleGenerator createMuscleFrom: (self nodes at: n1) to: (self nodes at: n2))
```
-->


<!--
~~~~~~
CNode>>printOn: str
	super printOn: str.
	str nextPut: $<.
	str nextPutAll: self hash asString.
	str nextPut: $>.
~~~~~~
-->


<!--
CCreature>>checkForCollision: platforms
	"This is an optimized version of :
	
	
	nodes do: [ :n | n checkForCollision: platforms ].
	(nodes allSatisfy: [ :n | n position y = nodes first position y ]) ifTrue: [ 
		nodes first translateBy: 0 @ -2.
		nodes last translateBy: 0 @ -2 ]
"
	| allAtTheSameLevel tmpLevel |
	allAtTheSameLevel := true.
	tmpLevel := nil.
	nodes doWithIndex: [ :n :index | 
		n checkForCollision: platforms.
		index = 1 
			ifTrue: [ tmpLevel := n position y ]
			ifFalse: [ n position y ~= tmpLevel ifTrue: [ allAtTheSameLevel ] ].
		 ].
	
"	(nodes allSatisfy: #isOnPlatform) ifFalse: [ ^ self ]. "
	allAtTheSameLevel ifFalse: [ ^ self ]. 
	nodes first translateBy: 0 @ -2.
	nodes last translateBy: 0 @ -2 
-->




<!--

	
	c := CodeToMarkdown new.
	c addClass: CCreature.
	c addClass: CMuscleGenerator.
	c addClass: CVisualElement.
	c addClass: CWorld.
	c addClass: CConnection.
	c addClass: CBone.
	c addClass: CMuscle.
	c addClass: CNode.
	c addClass: CPlatform.
	c addClass: GAConstrainedCrossoverOperation.
	c writeMarkdown: '/Users/alexandrebergel/Dropbox/Workspace/Books/AgileArtificialIntelligence.github.io/book/14-Zoomorphic/14-Zoomorphic.markdown'
	
-->