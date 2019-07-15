
# Simulating Zoomorphic Creature

The previous chapter presents the infrastructure to define and build zoomorphic creature. However, so far, they cannot do much. They are likely to not move much. This chapter will make a creature evolve to accomplish some simple tasks, such as moving toward a particular direction or passing through some obstacles.

## Process Interruption 

Making creatures evolve is a very costly operation. Most of the script given in this chapter may requires many minutes or hours to complete. We suggest you to be familiar with the way Pharo can be interrupting by pressing the `Cmd` and `.` keys on Mac OSX. On Windows or Linux, you should use the `Alt` key. 

Interrupting Pharo will bring up a Pharo debugger. When this happens, the execution has been interrupted. You can then execute any arbitrary code. 
Closing the debugger will simply end the ongoing computation. Keeping the debugger open means you can always resume the execution you interrupted by clicking on `Proceed`.

## Monitoring Execution Time

Running a genetic algorithm for our little creature is time consuming. We will extend our framework to keep track of the time

```Smalltalk
Object subclass: #GALog
	instanceVariableNames: 'generationNumber timeToProduceGeneration fittestIndividual worseFitness averageFitness time'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

```Smalltalk
GALog>>initialize
	super initialize.
	time := DateAndTime now
```

The class `DateAndTime` represents a point in time. When a log object is created, we keep the creatime time in the variable `time`. 

```Smalltalk
GAEngine>>timeTaken
	"Return the time taken to compute all the generations"
	| lastLog |
	lastLog := self logs last.
	^ lastLog time + lastLog timeToProduceGeneration - self logs first time 
```

## Dedicated Genetic Operator

So far, we have seen two crossover operations: 

- `GACrossoverOperation` to perform a simple crossover, without enforcing any characteristics,
- `GAOrderedCrossoverOperation` to avoid repetitions of particular genes.

In the case of evolving our creatures, it is important to consider a muscle as a whole while performing the crossover. For example, it could be that two creatures have a similar behavior, but each with a very different genotype. We call this situation _competing conventions_. If we combine use the unconstrained crossover operation, it is likely that the children is worse that its parents. 

One way to avoid this problem is to restrict the crossover to happen at _any_ point, but only at a muscle extremity. Combining the genetic information of two different muscle is not efficient in our situation. We define a new operator, called `GAConstrainedCrossoverOperation`:

```Smalltalk
GAAbstractCrossoverOperation subclass: #GAConstrainedCrossoverOperation
	instanceVariableNames: 'possibleCutpoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

The operator considers a set of possible cutpoints with the variable `possibleCutpoints`, which is set using:

```Smalltalk
GAConstrainedCrossoverOperation>>possibleCutpoints: indexes
	"Set the possible pointcuts considered by the operator"
	possibleCutpoints := indexes
```

We also add a utility method used to hook it into our framework:

```Smalltalk
GAConstrainedCrossoverOperation>>pickCutPointFor: partnerA
	"Argument is not used now. Maybe we can improve that"
	self assert: [ possibleCutpoints notNil ] description: 'Need to provide the possible cut points, using #possibleCutpoints:'.
	^ possibleCutpoints at: (random nextInt: possibleCutpoints size)
```


## Moving Forward 

Consider the task to move forward, to the right:

~~~~~~
numberOfNodes := 10.
numberOfMuscles := (CCreature new configureBall: numberOfNodes) numberOfMuscles.
mg := CMuscleGenerator new
		minStrength: 0.01;
		deltaStrength: 1;
		minLength: 10;
		deltaLength: 80;
		deltaTime: 200;
		minTime: 20.
g := GAEngine new.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: (1 to: numberOfMuscles*5 by: 5)).
g selection: (GATournamentSelection new).
g mutationRate: 0.02.
g endForMaxNumberOfGeneration: 128.
g populationSize: 100.
g numberOfGenes: numberOfMuscles * 5.
g createGeneBlock: [ :r :index | mg valueForIndex: index ].
g fitnessBlock: [ :genes |
	creature := CCreature new configureBall: numberOfNodes.
	creature materialize: genes.
	c := CWorld new.
	c addCreature: creature.
	3000 timesRepeat: [ c beat ].
	creature position x
].
g run. 
~~~~~~

The script considers a ball made of 10 nodes. The physic engine will locate these nodes in a circular fashion as a result of the physical rules. The number of muscles is obtained by evaluating the expression `(CCreature new configureBall: numberOfNodes) numberOfMuscles`. It simply creates a dummy creature and counts the number of muscles. A ball creature made of 10 nodes has 45 muscles. We then define a muscle generator useful to build the initial population and mutate a creature.

Each muscle is defined by five attributes. A crossover operation may happen only at the junction of a muscle definition. The fitness function simulates the behavior of the creature in a new world. We took an arbitrary number of beats, 3000, to simulate the behavior. After these 3000 beats, the x coordinate of the creature position defines the function fitness. A fit creature will move forward to the right. The evolution happens over 128 generations (an arbitrary value). 

![Evolving a 45-muscles creature.](15-ZoomorphicSimulation/figures/evolutionZoomorphic01.png){#fig:evolutionZoomorphic01}

Figure @fig:evolutionZoomorphic01 shows the fitness evolution. The fitness indicates that the creature is able to move.

We can see the result using the script:

~~~~~~
...
creature := CCreature new configureBall: 10.
creature materialize: g result.
c := CWorld new.
c addCreature: creature.
c open
~~~~~~


![A creature in its environment.](15-ZoomorphicSimulation/figures/evolutionZoomorphic02.png){#fig:evolutionZoomorphic02 width=400px}

Figure @fig:evolutionZoomorphic02 illustrates a creature in its environment. We can monitor the evolution of a creature at a particular points in time. For example, consider the script:

~~~~~~
...
c := CWorld new.
creature := CCreature new color: Color red; configureBall: 10.
creature materialize: g logs last fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color yellow darker darker; configureBall: 10.
creature materialize: (g logs at: 50) fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color blue darker darker; configureBall: 10.
creature materialize: (g logs at: 100) fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color green darker darker; configureBall: 10.
creature materialize: (g logs at: 120) fittestIndividual genes.
c addCreature: creature.

c open
~~~~~~

![Creature at different stage of its evolution (yellow = generation 50, blue = generation 100, green = generation 120, red = generation 128).](15-ZoomorphicSimulation/figures/evolutionZoomorphic03.png){#fig:evolutionZoomorphic03}

If you watch these competing creature, it is interesting to see that the red is not always in the first position. The green creature overtakes the red one at multiple times. Ultimately, the red one reaches the final pylon. 

## Serializing the Muscle Attributes

The expression `g result` returns the attributes of the muscles used in the creature. You can keep the computed result in case you do not wish to run the genetic algorithm all the time. For example, if you expand the `g result` expression, you obtain:

```Smalltalk
creature := CCreature new configureBall: 10.
creature materialize: #(24 34 0.46040109215788594 216 145 75 50 0.522318108469396 127 33 33 39 0.9105445367193523 70 93 30 88 0.5458242390378492 55 104 32 78 0.9326984656055917 36 74 20 38 0.23007194683890417 169 77 25 31 0.6407352956527543 219 147 28 14 0.5132012814205146 70 67 41 32 0.4101663086936652 116 21 30 53 0.4132064962215752 140 69 26 16 0.67239310366213 174 81 90 40 0.9493843137376868 77 82 90 24 0.9472498080773512 72 76 77 15 0.8207815849644977 51 46 63 21 0.23135899086546108 29 170 33 24 0.8508932494190025 70 94 34 32 0.85425589900662 192 99 83 84 0.8219266167338596 153 144 74 57 0.18008196523882541 38 136 76 82 0.4098378945513805 108 122 73 25 0.13200707016606214 72 102 11 24 0.525760215705149 60 33 34 53 0.47843877270279395 207 167 53 53 0.06064744597796698 47 203 90 90 0.3480303188869871 101 204 77 42 0.05166656036007524 143 155 67 89 0.5535930274164271 146 23 35 39 0.8390450097196945 136 143 78 87 0.955747404799679 153 71 15 84 0.9765097738460218 34 26 36 14 0.13894161191253998 78 51 38 41 0.1316714140594338 114 205 74 74 0.7760572821116342 191 32 67 61 0.08824125377379416 219 149 18 70 0.1469941007052521 169 175 39 43 0.2866080141424239 133 71 90 42 0.8735930218098653 90 85 53 21 0.18471918099313936 39 146 60 44 0.3135163908747567 120 38 57 43 0.32777994628892276 187 148 34 23 0.3158802803540045 35 102 75 42 0.1347404502354285 109 125 28 76 0.12238997760805766 64 23 68 70 0.9608936917180632 179 175 28 24 0.06067319378753807 116 196 ).
c := CWorld new.
c addCreature: creature.
c open
```

This long array of number constitutes the "DNA" of the creature. The objective of the genetic algorithm is to evolve the DNA to make the creature move to the right as much as possible. 


## Passing Obstacles

So far, our creature has evolved to move right. We can also train to pass obstacles. We can adapt our script to incorporate some obstacles. Consider:

~~~~~
numberOfNodes := 10.
numberOfMuscles := (CCreature new configureBall: numberOfNodes) numberOfMuscles.
mg := CMuscleGenerator new
		minStrength: 0.01;
		deltaStrength: 1;
		minLength: 10;
		deltaLength: 80;
		deltaTime: 200;
		minTime: 20.
g := GAEngine new.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: (1 to: numberOfMuscles*5 by: 5)).
g selection: (GATournamentSelection new).
g mutationRate: 0.02.
g endForMaxNumberOfGeneration: 128.
g populationSize: 100.
g numberOfGenes: numberOfMuscles * 5.
g createGeneBlock: [ :r :index | mg valueForIndex: index ].
g fitnessBlock: [ :genes |
	creature := CCreature new configureBall: numberOfNodes.
	creature materialize: genes.
	creature resetPosition.
	c := CWorld new.
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 100 @ -10).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 400 @ -10).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 700 @ -10).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 1000 @ -10).
	c addCreature: creature.
	3000 timesRepeat: [ c beat ].
	creature position x
].
g run. 
~~~~~

![Evolving a zoomorphic creature in presence of obstacles.](15-ZoomorphicSimulation/figures/evolutionZoomorphic04.png){#fig:evolutionZoomorphic04}

The result can be rendered using the script: 

~~~~~
...
c := CWorld new.
creature := CCreature new color: Color red; configureBall: 10.
creature materialize: g logs last fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color yellow darker darker; configureBall: 10.
creature materialize: (g logs at: 50) fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color blue darker darker; configureBall: 10.
creature materialize: (g logs at: 100) fittestIndividual genes.
c addCreature: creature.

creature := CCreature new color: Color green darker darker; configureBall: 10.
creature materialize: (g logs at: 90) fittestIndividual genes.
c addCreature: creature.

c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 100 @ -10).
c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 400 @ -10).
c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 700 @ -10).
c addPlatform: (CPlatform new height: 20; width: 80; translateTo: 1000 @ -10).
c open
~~~~~~

![Different stages of the evolution.](15-ZoomorphicSimulation/figures/evolutionZoomorphic05.png){#fig:evolutionZoomorphic05}

Figure @fig:evolutionZoomorphic05 illustrates different stages of an evolved creature.

## Stair Climbing

Creature can evolve to climb stairs. Consider the script:

~~~~~~~~
numberOfNodes := 10.
numberOfMuscles := (CCreature new configureBall: numberOfNodes) numberOfMuscles.
mg := CMuscleGenerator new
		minStrength: 0.01;
		deltaStrength: 1;
		minLength: 10;
		deltaLength: 80;
		deltaTime: 200;
		minTime: 20.
g := GAEngine new.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: (1 to: numberOfMuscles*5 by: 5)).
g selection: (GATournamentSelection new).
g mutationRate: 0.02.
g endForMaxNumberOfGeneration: 128.
g populationSize: 100.
g numberOfGenes: numberOfMuscles * 5.
g createGeneBlock: [ :r :index | mg valueForIndex: index ].
g fitnessBlock: [ :genes |
	creature := CCreature new configureBall: numberOfNodes.
	creature materialize: genes.
	creature resetPosition.
	c := CWorld new.
	c addCreature: creature.
	1 to: 25 by: 3 do: [ :x |
		c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 @ -10).
		c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 50 @ -30).
		c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 100 @ -50).
		c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 150 @ -70).
	].
	c addCreature: creature.
	3000 timesRepeat: [ c beat ].
	creature position x
].
g run. 
~~~~~~~~

The script is very similar to the previous ones. It simply add some well positioned platforms to form stairs. Result may be seen with the following script (Figure @fig:evolutionZoomorphic06):

~~~~~~~~
...
creature := CCreature new configureBall: 10.
creature materialize: g result.
c := CWorld new.
"We build couple of stairs"
1 to: 25 by: 3 do: [ :x |
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 @ -10).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 50 @ -30).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 100 @ -50).
	c addPlatform: (CPlatform new height: 20; width: 80; translateTo: x * 100 + 150 @ -70).
].
c addCreature: creature.
c open
~~~~~~~~

![Climbing stairs.](15-ZoomorphicSimulation/figures/evolutionZoomorphic06.png){#fig:evolutionZoomorphic06}

## What have we seen in this chapter?

This chapter illustrates how some creature, which we qualify as _zoomorphic_ due to their organic way of moving, evolve to solve some tasks. In particular, the chapter covers:

- a basic technique to interrupt long-running processes. This is central aspect of this chapter as the evolution we deal with takes several minutes;
- the evolution of a creature in three different scenarios: without any obstacles, with some simple obstacles, and with stairs.

This chapter closes the second part of the book. 
