
# Traveling Salesman Problem

The Traveling Salesman Problem is a classical algorithm problem. It consists in visiting a number of cities using the shortest possible route. 

## Illustration

![Illustration of the Traveling Salesman Problem.](12-TravelingSalesmanProblem/figures/TSPExample.png){#fig:TSPExample}

Consider the example given in Figure @fig:TSPExample. The figure shows four cities located as an horizontal diamond. Each city has a 2D coordinate and is therefore located in a two-dimensional plan. Assuming the traveler begins its journey in city A, many paths are possible to visit all the cities. 

![Illustration of the Traveling Salesman Problem.](12-TravelingSalesmanProblem/figures/TSPDifferentPaths.png){#fig:TSPDifferentPaths}

As illustrated in Figure @fig:TSPDifferentPaths, different paths are possible, including ABCD, ABDC, ADBC, ACDB, and ADCB. _What is the shortest path to visit all the cites?_ Since the distance between City B and City D is the shortest segment, the shortest path to visit all cities should necessarily contain the segment BD or DB.

## Relevance of the Traveling Salesman Problem

The Traveling Salesman Problem (TSP) is a relevant problem to focus on, both from a theoretical and practical points of view. The TSP was formulated in the early 1930 and is among the most studied algorithmic problems. Applications of the TSP are numerous, ranging from combinatorial optimization (_i.e.,_ finding an optimal object from a finite set of objects) to resource planning, DNA sequencing, and microchip manufacturing. Even though it has been studied for a long time, no general solution has been discovered yet. 

TSP is apparently a simple problem: one should simply connect some cities, in an optimal way. However, TSP is a very difficult problem and is considered as _NP-hard_. Being NP-hard means that a correct solutions are easy to verify (_e.g.,_ given two paths, it is easy to pick which one is shortest), but there is no efficient way to solve the problem itself. If someone, one day, finds an analytic solution to the TSP, the World is surely set to be impacted. Analytically solving an NP-hard problem means that NP-complete can also be analytically solved. Have you heard about P versus NP problems? This is one of the most challenging question mathematicians are facing today. Actually, a millennium prize of 1 000 000 USD will be given by the Clay Mathematics Institute if one solves this problem. 

In this chapter we do not pretend analytically solving this problem. However, genetic algorithm is a pretty solid technique to find a good path, although no guaranty is provided on whether this path is optimal or not. 

## Naive Approach

How to encode a path to make it exploitable by a genetic algorithm? For this problem, computing the fitness is trivial: it is simply the sum of the segment lengths. We can try the following script:

```Smalltalk
"We encode distances"
d := { ($A -> $B) -> 10 . ($A -> $D) -> 10 . ($B -> $C) -> 10 . ($C -> $D) -> 10 . ($A -> $C) -> 20 . ($B -> $D) -> 8 } asDictionary.

g := GAEngine new.
g endIfNoImprovementFor: 10.
g populationSize: 100.
g numberOfGenes: 4.
g createGeneBlock: [ :rand :index :ind | 'ABCD' atRandom: rand ].
g minimizeComparator.
g fitnessBlock: [ :genes |
	| currentCity length | 
	currentCity := genes first.
	length := 0.
	genes allButFirst do: [ :nextCity |
		length := length + (d at: (currentCity -> nextCity) ifAbsent: [ d at: (nextCity -> currentCity) ifAbsent: [ 0 ] ]).
		currentCity := nextCity ].
	length 
	 ].
g run
```

![Result of the naive approach.](12-TravelingSalesmanProblem/figures/TSPNaive.png){#fig:TSPNaive}

Figure @fig:TSPNaive gives the result of the run. The best fitness is zero, which is clearly not what we expect. Pressing the `Logs` tab reveals that all the individual are `($B $B $B $B)`. The genetic algorithm found that the shortest distance is to not travel at all!

How can we force the algorithm to avoid visiting the same cities? A path, in order to be valid, should pass through all the cities once. The easiest way to enforce this, is to incur a penalty when this happens. Consider this revised script:

```Smalltalk
"We encode distances"
d := { ($A -> $B) -> 10 . ($A -> $D) -> 10 . ($B -> $C) -> 10 . ($C -> $D) -> 10 . ($A -> $C) -> 20 . ($B -> $D) -> 8 } asDictionary.

g := GAEngine new.
g endIfNoImprovementFor: 10.
g populationSize: 1000.
g numberOfGenes: 4.
g createGeneBlock: [ :rand :index :ind | 'ABCD' atRandom: rand ].
g minimizeComparator.
g fitnessBlock: [ :genes |
	| currentCity length | 
	currentCity := genes first.
	length := 0.
	genes allButFirst do: [ :nextCity |
		length := length + (d at: (currentCity -> nextCity) ifAbsent: [ d at: (nextCity -> currentCity) ifAbsent: [ 0 ] ]).
		currentCity := nextCity ].
	length + ((4 - genes asSet size) * 100)
	 ].
g run.
```

![Improving the naive approach.](12-TravelingSalesmanProblem/figures/TSPImprovedNaive.png){#fig:TSPImprovedNaive}

Figure @fig:TSPImprovedNaive now presents some acceptable results. The best path has a fitness of 28. The blue curve did not evolve over time, which means that the algorithm found the solution from the very beginning. Click on the `Logs` tab reveals the solutions. For example, we see that ABDC and ABCD are solutions, which can be easily verified. At a first glance, it seems that our penalty seems to do its job. Well... not quite, as we will see.

Let's pick a larger example. The following script replaces our list of cities by a list of points:

```Smalltalk
"We encode distances"
points := {(100@160). (20@40). (60@20). (180@100). (200@40). (60@200). (80@180). (40@120). (140@180). (140@140). (20@160). (200@160). (180@60). (100@120). (120@80). (100@40). (20@20). (60@80). (180@200). (160@20)}.

g := GAEngine new.
g endIfNoImprovementFor: 60.
g populationSize: 1000.
g numberOfGenes: points size.
g createGeneBlock: [ :rand :index :ind | points atRandom: rand ].
g minimizeComparator.
g fitnessBlock: [ :genes |
        | distance d |
        distance := 0.
        2 to: genes size do: [ :pointIndex |
            d := (genes at: pointIndex) dist: (genes at: pointIndex - 1).
            distance := distance + d ].
        distance + ((points size - genes asSet size) * 1000) ].
g run.
```

![Improving the naive approach.](12-TravelingSalesmanProblem/figures/TSPLargeNaive.png){#fig:TSPLargeNaive}


The distance is computed using the `dist:` method, defined in the class `Point`. Result of the script is shown in Figure @fig:TSPLargeNaive. It seems that the algorithm found a compelling solution since it has reached a plateau.

We can see the result using the script:

```Smalltalk
...
result := g result.
v := RTView new.
elements := RTEllipse new size: 10; color: Color red trans; elementsOn: result.
elements @ RTPopup.
v addAll: elements.
elements do: [ :e | e translateTo: e model ].
2 to: result size do: [ :index |
    | l |
    l := RTArrowedLine new color: Color blue; headOffset: 0.8.
    v add: (l edgeFrom: (v elementFromModel: (result at: index - 1)) to: (v elementFromModel: (result at: index))) ].
v
```

![Visualizing the result of the naive approach.](12-TravelingSalesmanProblem/figures/TSPLargeNaive2.png){#fig:TSPLargeNaive2}

Figure @fig:TSPLargeNaive2 gives the result of the algorithm. Obviously, the blue arrowed line does not indicate the shortest path that connect all the cities. For example, there are two close cities in the top-left which are not connected. An optimal solution should surely pass through these two cities, but the result of the algorithm does not take advance of this. 


![Result of a 10K population using the naive approach.](12-TravelingSalesmanProblem/figures/TSPLargeNaive2With10000.png){#fig:TSPLargeNaive2With10000}

What if we increase the population size? Figure @fig:TSPLargeNaive2With10000 shows the result of the same algorithm with a population large of 10 000 individuals. The result is now apparently closer to the solution. 

Such a problem should be easy to solve. So, why the genetic algorithm is struggling to? The reason is that the algorithm is fighting hard from avoiding redundant cities. Instead of looking for exploring the set of possible valid candidates, the algorithm is actually struggling at identifying what are these valid candidates. This is why we label our solution as _naive_. Introducing a penalty as a way to guide the algorithm has a very negative side effect, which is looking for individual who do not suffer from this penalty, thus leaving little room for exploring valid path.

The morale of the story is that we should use the algorithm to explore valid path, and not using it to identify what these valid paths are. Remember the Murphy law? If the algorithm generates random paths, then it will surely has to deal with the randomly-generated mess. Instead of using the penalty, we should tune the algorithm in such a way that only valid path can be generated both in the initial population and as a result of the genetic operations.

## Adequate Genetic Operations

Using our four-city primary example, consider the paths ABCD and DCBA. Any genetic operation, either a crossover between these two paths or a genetic mutation on any of it will generate an invalid path. 

Can we design some genetic operations that do not produce invalid path? The answer is _yes_. The remaining of the chapter will present the _swap mutation operation_ and the _ordered crossover operation_.  


## Swap Mutation Operation

Instead of replacing any gene any another one, as implemented by the `GAMutationOperation` class, we will _swap_ two genes in an individual. For example, if we have ABCD, then a swap mutation could produce CBAD, but never AACD. 

Luckily, we prepared the ground to implement new operation. We can define the class `GASwapMutationOperation`:

```Smalltalk
GAAbstractMutationOperation subclass: #GASwapMutationOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

We can override the method `doMutate:` to swap genes:

```Smalltalk
GASwapMutationOperation>>doMutate: individual
	"Mutate genes of the argument"

	| i2 tmp |
	self checkForRandomNumber.
	1 to: individual genes size do: [ :i1 | 
		self randomNumber <= mutationRate
			ifTrue: [ 
				i2 := random nextInt: individual genes size.
				tmp := individual genes at: i1.
				individual genes at: i1 put: (individual genes at: i2).
				individual genes at: i2 put: tmp ] ]	
```

The method randomly pick to gene indexes, and swap their content. 

## Ordered Crossover Operation

The ordered crossover operation is slightly more complex. Consider the paths `iA = ABCD` and `iB = ADBC`. Our new operation will consider a swath of genes, delimited with two indexes, 2 and 4. The children `iC` will have the genes obtained from `iA` from index 2 to index 4. We have `iC = ..CD`. The two first genes will have to be obtained from `iB` from which we will remove the genes we already picked, `C` and `D`. We have `iB - CD = AB`. We can have `iC = ABCD`.

We create the class `GAOrderedCrossoverOperation`:

```Smalltalk
GAAbstractCrossoverOperation subclass: #GAOrderedCrossoverOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

The crossover will randomly choose the two extremities of the swath:

```Smalltalk
GAOrderedCrossoverOperation>>crossover: individualA with: individualB
    "Return a new child, which is the result of mixing the two individuals"
 	| i1 i2 |
	i1 := self pickCutPointFor: individualA.
	i2 := self pickCutPointFor: individualA.
	
	(i1 > i2) ifTrue: [ | t | t := i1. i1 := i2. i2 := t ].
	^ self crossover: individualA with: individualB from: i1 to: i2
```

The core of the ordered crossover operation is the method: 

```Smalltalk
GAOrderedCrossoverOperation>>crossover: individualA with: individualB from: i1 to: i2
    "Return a new child, which is the result of mixing myself the two individual"

    | child crossOverGenes runningIndex swath |
    child := GAIndividual new.
    child random: random.

	swath := individualA genes copyFrom: i1 to: i2.
	crossOverGenes := Array new: individualA genes size.
	crossOverGenes := crossOverGenes copyReplaceFrom: i1 to: i2 with: swath.

	runningIndex := 1.
	(individualB genes copyWithoutAll: swath)
		do: [ :v | (crossOverGenes includes: v) ifFalse: [ 
				[(crossOverGenes at: runningIndex) notNil] whileTrue: [ runningIndex := runningIndex + 1 ]. 
				crossOverGenes at: runningIndex put: v ] ].

    child genes: crossOverGenes.
    ^ child
```

We used then the following utility method:

```Smalltalk
GAOrderedCrossoverOperation>>pickCutPointFor: partner
    "Simply return a random number between 1 and the number of genes of the individual provided as argument"
    ^ random nextInt: partner genes size
```

And _voila_! We can now test our new operator:

```Smalltalk
TestCase subclass: #GAOrderedCrossoverOperationTest
	instanceVariableNames: 'i1 i2 op'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Tests'
```

We define a `setUp` method:

```Smalltalk
GAOrderedCrossoverOperationTest>>setUp
    super setUp.
    i1 := GAIndividual new genes: #(8 4 7 3 6 2 5 1 9 0).
    i2 := GAIndividual new genes: #(0 1 2 3 4 5 6 7 8 9).
    op := GAOrderedCrossoverOperation new.
```

A first test could be:

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover1
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 4 to: 8.
    self assert: i3 genes equals: #(0 4 7 3 6 2 5 1 8 9).
```

We take the first gene as an extremity:

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover2
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 1 to: 4.
    self assert: i3 genes equals: #(8 4 7 3 0 1 2 5 6 9).
```

We consider the last two genes as the swath:

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover3
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 9 to: 10.
    self assert: i3 genes equals: #(1 2 3 4 5 6 7 8 9 0).
```
<!--
```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover4
    | i3 |  
    i1 := GAIndividual new genes: #(1 2 3 4 5 6 7 8 9).
    i2 := GAIndividual new genes: #(9 8 7 6 5 4 3 2 1).

    i3 := op crossover: i1 with: i2 from: 6 to: 8.
    self assert: i3 genes equals: #(9 5 4 3 2 6 7 8 1).               
```
-->

## Our Large Example

At the beginning of the chapter we presented a configuration for which our naive approach could not find the shortest path. Now that we have defined our two new genetic operations, we can hook them up:

```Smalltalk
"We define the points"
points := {(100@160). (20@40). (60@20). (180@100). (200@40). (60@200). (80@180). (40@120). (140@180). (140@140). (20@160). (200@160). (180@60). (100@120). (120@80). (100@40). (20@20). (60@80). (180@200). (160@20)}.

g := GAEngine new.
g endIfNoImprovementFor: 5.
g populationSize: 1000.
g numberOfGenes: points size.
g crossoverOperator:  GAOrderedCrossoverOperation new.
g beforeCreatingInitialIndividual: 
		[ :rand | points copy shuffleBy: rand ].
g mutationOperator: GASwapMutationOperation new.
g createGeneBlock: [ :rand :index :ind | points at: index ].

g minimizeComparator.
g fitnessBlock: [ :genes |
	| distance d |
	distance := 0.
	2 to: genes size do: [ :pointIndex |
		d := (genes at: pointIndex) dist: (genes at: pointIndex - 1).
		distance := distance + d ].
	distance ].
g run.
```

![Result of using our two new genetic operators.](12-TravelingSalesmanProblem/figures/TSPLargeExample.png){#fig:TSPLargeExample}

Figure @fig:TSPLargeExample illustrates the evolution of the fitness along the generations. We configured the algorithm to stop if it does not find an improvement for 5 generations, using the message `endIfNoImprovementFor: 5`. 

We can now visualize the result using:

```Smalltalk
...
result := g result.
v := RTView new.
elements := RTEllipse new size: 10; color: Color red trans; elementsOn: result.
elements @ RTPopup.
v addAll: elements.
elements do: [ :e | e translateTo: e model ].
2 to: result size do: [ :index |
	| l |
	l := RTArrowedLine new color: Color blue; headOffset: 0.8.
	v add: (l edgeFrom: (v elementFromModel: (result at: index - 1)) to: (v elementFromModel: (result at: index))) ].
v
```

![Result of using our two new genetic operators.](12-TravelingSalesmanProblem/figures/TSPLargeExampleFinallySolved.png){#fig:TSPLargeExampleFinallySolved}

Figure @fig:TSPLargeExampleFinallySolved shows the result of the algorithm. With only a population large of 1 000 individuals, our algorithm was able to solve the TSP. Remember that with our first naive approach, we could not solve it with a population ten times larger!

## What have we seen in this chapter?

This chapter presented a compelling way to solve a complex problem by using dedicated genetic operations. In particular, the chapters presented 

- the traveler salesman problem, a classical algorithmic problem;
- an illustration of the consequence of naively applying the genetic algorithm;
- a motivation for introducing two new genetic operations, namely the _swap mutation operation_ and the _ordered crossover operation_.