
# Traveling Salesman Problem

The Traveling Salesman Problem is a classical algorithm problem. It consists in visiting a number of cities using the shortest possible route. 

## Illustration

![Illustration of the Traveling Salesman Problem.](12-TravelingSalesmanProblem/figures/TSPExample.png){#fig:TSPExample}

Consider the example given in Figure @fig:TSPExample. The figure shows four cities located as an horizontal diamond. Each city has a 2D coordinate. Assuming the traveler begins its journey in city A, many paths are possible, for example ABCD, ABDC, ADBC, ACDB, or ADCB. What is the shortest path to visit all the cites? Since the distance between City B and City D is shorter than 

## Swap mutation operation

A particular operation for mutation is necessary. 

```Smalltalk
GAAbstractMutationOperation subclass: #GASwapMutationOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

```Smalltalk
GASwapMutationOperation>>doMutate: individual
	"Mutate genes of the argument"

	| i2 tmp |
	self checkForRandomNumber.
	1 to: individual genes size do: [ :i1 | 
		self randomNumber <= mutationRate
			ifTrue: [ i2 := random nextInt: individual genes size.
				tmp := individual genes at: i1.
				individual genes at: i1 put: (individual genes at: i2).
				individual genes at: i2 put: tmp ] ]	
```

## Ordered Crossover operation

```Smalltalk
GAAbstractCrossoverOperation subclass: #GAOrderedCrossoverOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Core'
```

```Smalltalk
GAOrderedCrossoverOperation>>crossover: partnerA with: partnerB
    "Return a new child, which is the result of mixing myself with the argument"

 	| i1 i2 |
	i1 := self pickCutPointFor: partnerA.
	i2 := self pickCutPointFor: partnerA.
	
	(i1 > i2) ifTrue: [ | t | t := i1. i1 := i2. i2 := t ].
	^ self crossover: partnerA with: partnerB from: i1 to: i2
```

```Smalltalk
GAOrderedCrossoverOperation>>crossover: partnerA with: partnerB from: i1 to: i2
    "Return a new child, which is the result of mixing myself with the argument"

    | child crossOverGenes runningIndex swath |
    child := GAIndividual new.
    child random: random.

	swath := partnerA genes copyFrom: i1 to: i2.
	crossOverGenes := Array new: partnerA genes size.
	crossOverGenes := crossOverGenes copyReplaceFrom: i1 to: i2 with: swath.

	runningIndex := 1.
	"((partnerB genes copyFrom: 1 to: i1), (partnerB genes copyFrom: i2 to: partnerB genes size)) "
	(partnerB genes copyWithoutAll: swath)
		do: [ :v | (crossOverGenes includes: v) ifFalse: [ 
				[(crossOverGenes at: runningIndex) notNil] whileTrue: [ runningIndex := runningIndex + 1 ]. 
				crossOverGenes at: runningIndex put: v ] ].

    child genes: crossOverGenes.
    ^ child
```

```Smalltalk
GAOrderedCrossoverOperation>>pickCutPointFor: partnerA
    "Simply return a random number between 1 and the number of genes of the individual provided as argument"
    ^ random nextInt: partnerA genes size
```

```Smalltalk
TestCase subclass: #GAOrderedCrossoverOperationTest
	instanceVariableNames: 'i1 i2 op'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeneticAlgorithm-Tests'
```

```Smalltalk
GAOrderedCrossoverOperationTest>>setUp
    super setUp.
    i1 := GAIndividual new genes: #(8 4 7 3 6 2 5 1 9 0).
    i2 := GAIndividual new genes: #(0 1 2 3 4 5 6 7 8 9).
    op := GAOrderedCrossoverOperation new.
```

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover1
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 4 to: 8.
    self assert: i3 genes equals: #(0 4 7 3 6 2 5 1 8 9).
```

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover2
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 1 to: 4.
    self assert: i3 genes equals: #(8 4 7 3 0 1 2 5 6 9).
```

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover3
    | i3 |  
    i3 := op crossover: i1 with: i2 from: 9 to: 10.
    self assert: i3 genes equals: #(1 2 3 4 5 6 7 8 9 0).
```

```Smalltalk
GAOrderedCrossoverOperationTest>>testCrossover4
    | i3 |  
    i1 := GAIndividual new genes: #(1 2 3 4 5 6 7 8 9).
    i2 := GAIndividual new genes: #(9 8 7 6 5 4 3 2 1).

    i3 := op crossover: i1 with: i2 from: 6 to: 8.
    self assert: i3 genes equals: #(9 5 4 3 2 6 7 8 1).               
```


## Example

```Smalltalk
| points tmp g result v elements |
points := ((Array new: 20) at: 1 put: ((100@160)); at: 2 put: ((20@40)); at: 3 put: ((60@20)); at: 4 put: ((180@100)); at: 5 put: ((200@40)); at: 6 put: ((60@200)); at: 7 put: ((80@180)); at: 8 put: ((40@120)); at: 9 put: ((140@180)); at: 10 put: ((140@140)); at: 11 put: ((20@160)); at: 12 put: ((200@160)); at: 13 put: ((180@60)); at: 14 put: ((100@120)); at: 15 put: ((120@80)); at: 16 put: ((100@40)); at: 17 put: ((20@20)); at: 18 put: ((60@80)); at: 19 put: ((180@200)); at: 20 put: ((160@20)); yourself).

tmp := nil.

    g := GAEngine new.
	g endForMaxNumberOfGeneration: 100.
	g mutationRate: 0.15.
    g populationSize: 60.
    g numberOfGenes: points size.
	g crossoverOperator:  GAOrderedCrossoverOperation new.
	g beforeCreatingInitialIndividual: [ :rand | tmp := points copy shuffleBy: rand ].
		g mutationOperator: GASwapMutationOperation new.
    g createGeneBlock: [ :rand :index :ind | tmp at: index ].

	g minimizeComparator.
    g fitnessBlock: [ :genes |
		| distance d |
		distance := 0.
		2 to: genes size do: [ :pointIndex |
			d := (genes at: pointIndex) dist: (genes at: pointIndex - 1).
			distance := distance + (d ) ].
		distance ].
    
    g run.


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