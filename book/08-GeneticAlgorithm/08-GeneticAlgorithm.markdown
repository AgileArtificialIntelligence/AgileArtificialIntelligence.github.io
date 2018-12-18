
# Genetic Algorithm

The first part of the book is about neural network, a computational metaphor for how the brain operates. In this second part, we will focus on evolution in general. Genetic algorithm are a computational metaphor of how evolution naturally happens.

## Natural Evolution Inspired Algorithms

We, as humans beings, are the result of a few thousands years of evolution. Biological evolution refers to some alteration of the heritable characteristics of biological populations over successive generations. Most of the characteristics are the expressions of genes that are passed on from parent to offspring during reproduction.

The Darwinian Natural Selection_ stipulates that in oder to have a natural evolution, the evolution mechanism needs to support:
- _Heredity_: a child receives a number of properties from its parents. In particular, if parents are robust and can live long enough, then the child should too.
- _Variation_: some variation may be introduced in children. As such, children should not be identical copy of their parents.
- _Selection_: some members of a population must have the opportunity to be parents, and have offspring in order to pass their genetic information. The selection is typically referred to as "survival of the fittest".

Computer scientists have produced a number of algorithms that simulate evolution. For example:
- _ant colony optimization_ is based on the idea that ant are foraging by pheromone communication to form a path. Such algorithm is suitable for graph-related problems
- _Bees algorithm_ is based on the honey bees foraging behavior. Such algorithm is suitable for scheduling and ordering problems. 

We will focus on _genetic algorithm_ (GA). It is an evolutionary algorithm that simulate the evolution of DNA information across a population. Genetic algorithm has three important properties that we will exploit in the book:
- GA is efficient to solve optimization problem,
- GA is easy implemented and does not require a strong theoretical background,
- GA can be easily combined with neural networks. We will go into details in the third part of the book.

## Example of a Genetic Algorithm

The overall idea of genetic algorithm is pretty simple. Imagine a friend ask you to solve the following challenge: _"You must find the 3-letter word I have in mind. For each try, I tell you the number of correct letters."_. Assume that the secret word is _cat_. At first, we give any randomly generated words made of 3 letters, such as _cow_, _poz_, and _gat_.

The word _cow_ as exactly 1 letter in common with _cat_, the secret word. We therefore say that _cow_ as a score of 1. The word _poz_ as a score of 0 since it has no matching letter with the secret word. The word _gat_ has a score of 2 since _at_ are two matching letters.

Since we have not found the solution, we can produce a new generation of words by doing some combinations of the words we already have. In particular, _gat_ and _cow_ can be combined into _gaw_, _cot_, _gow_. From these three words, the word _cot_ has a score of 2 and is very close to the secret word. We say that this second generation of words is improving by being closer to the solution.

## Vocabulary

We have to introduce a few terms to describe the concepts we will use all over. We will rephrase the example given using these appropriate concepts.

We refer to an _individual_ an element that contains genetic information. Such a genetic information is described as a sequential collection of _genes_. A _gene_ represent an unit of information. A gene could be anything. In the example we previously gave, a gene is simply a letter. An individual is a 3-letter word. 

A _population_ is simply a fixed number of individuals. The population has a constant size, however its composing individuals are replaced at each generation. 

The _fitness function_ indicates how "strong" an individual is. The fitness function is a simple function that takes as argument an individual and produces a numerical value. The whole idea of genetic algorithm is to search for individuals that maximize the fitness function. 

## Individual

We will first express individuals. We will therefore model a class called `GAIndividual`. We will make `GAIndividual` of our custom class `GAObject`. We define `GAObject` as a subclass of `Object` which has a `random` variable. 

Almost all elements involved in a genetic algorithm requires generating random numbers. It is therefore convenient to have the variable defined in the root hierarchy used in our implementation:

```Smalltalk
Object subclass: #GAObject
	instanceVariableNames: 'random'
	classVariableNames: ''
	package: 'GeneticAlgorithm'
```
As usual, we define the getter:

```Smalltalk
GAObject>>random
	"Return the random number generator associated to the object"
	^ random
```	

And the setter: 

```Smalltalk
GAObject>>random: aRandomNumberGenerator
	"Set the random number generator associated to the object. The argument must be an instance of Random."
	random := aRandomNumberGenerator
```	

The method `random:` expects an instance of the class `Random` as argument. We also define a utility method that generate a number between 0.0 and and 1.0:

```Smalltalk
GAObject>>randomNumber
	"Return a number between 0.0 and 1.0"
	^ random next
```

We define the class `GAIndividual` as:

```Smalltalk
GAObject subclass: #GAIndividual
	instanceVariableNames: 'genes fitness'
```

An essential property of the individual, is to have a fitness computed for its genes:

```Smalltalk
GAIndividual>>computeFitnessUsing: fitnessBlock
	"Compute the fitness of myself"
	self assert: [ genes notNil ] description: 'Need to have some genes first'.
	fitness := fitnessBlock value: genes
```

We will use a one-argument block to compute its fitness. The block takes as argument the genes of the individual. Shortly we will see some example.

One the fitness is computed, we can get its value:

```Smalltalk
GAIndividual>>fitness
	"Return the fitness of the individual"
	^ fitness 
```

Genes may be accessed using:

```Smalltalk
GAIndividual>>genes
	"Return the collection of genes"
	^ genes
```

Later the genetic operations will set the genes of a particular individual. We therefore need a dedicated method for this:

```Smalltalk
GAIndividual>>genes: someGenes
	"Set the genes of the individual. Useful for the genetic operations."
	genes := someGenes
```

The number of genes may be obtained using a dedicated method. This will be useful for the genetic operations:

```Smalltalk
GAIndividual>>numberOfGenes
	"Return the number of genes the individual has"
	^ self genes size
```

A central ability of the class is to generate the genes from a block factory:

```Smalltalk
GAIndividual>>set: numberOfGenes genesUsing: geneBlockFactory
	"Public method"

	self checkForRandomNumber.
	genes := (1 to: numberOfGenes)
		collect: [ :index | geneBlockFactory cull: random cull: index cull: self ]
```

We can now create a useful factory method, as a class method:

```Smalltalk
GAIndividual class>>create: numberOfIndividuals individualsAndInitialize: numberOfGenes genesWith: geneBlockFactory using: randomNumberGeneration
	"Factory method to easily create a number of Individuals.
		numberOfIndividuals : is the number of individuals to return
		numberOfGenes : number of genes each individual should have
		geneBlockFactory : is a one-argument block to generate a gene. It takes a random generator as argument
		randomNumberGeneration : is a random generator"
	| someIndividuals ind |
	someIndividuals := OrderedCollection new.
	numberOfIndividuals timesRepeat: [ 
		ind := self new.
		ind random: randomNumberGeneration.
		ind set: numberOfGenes genesUsing: geneBlockFactory.
		someIndividuals add: ind ].
	^ someIndividuals
```

We can now test the class `GAIndividual`. We create the test `GAIndividualTest` for that purpose:

```Smalltalk
TestCase subclass: #GAIndividualTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgo-Tests'
```

As a simple test, we can create 100 individual, each having 10 genes, 

```Smalltalk
GAIndividualTest>>testCreationWithCharacters
	| r individuals f ind |
	r := Random seed: 42.
	f := [ :random | ($a to: $z) atRandom: random ].
	individuals := GAIndividual
		create: 100
		individualsAndInitialize: 10
		genesWith: f
		using: r.
		
	self assert: individuals size equals: 100.
	self assert: (individuals collect: #numberOfGenes) asSet asArray equals: #(10).

	ind := individuals anyOne.
	self assert: (ind genes allSatisfy: [ :c | ($a to: $z) includes: c ]).
```	

In the example of genetic algorithm we first gave, the algorithm has to guess the word _cat_. We can now use some individual to guess that word.

First, we need to create some individuals, each having 3 letters as genes:

```Smalltalk
inds := GAIndividual 
			create: 1000 
			individualsAndInitialize: 3 
			genesWith: [ :r | ($a to: $z) atRandom: r ].
```

Notice that the block to create a gene takes as argument a random number generator. This is necessary to be able to recreate the exact same individual. In other words, evaluating the expression given above will create _the exact same_ individuals. 

We can then compute the fitness of each individual. The fitness describes a score on how close to the solution the individual is. 

```Smalltalk
fitnessBlock := [ :genes | (genes with: 'cat' asArray collect: [ :a :b | (a == b) 
									ifTrue: [ 1 ] 
									ifFalse: [ 0 ] ]) sum ].

inds do: [ :i | i computeFitnessUsing: fitnessBlock ].
```

After executing these two statements, each individual has a fitness value. Overall, the fitness value ranges from 0 to 3. An individual with a fitness of 3 means it matches the solution, the genes are equal to `#($c $a $t)`.

We can now render the distribution of the result over the individuals (Figure @fig:distributionResults):

```Smalltalk
data := ((inds collect: #fitness) groupedBy: #yourself) associations collect: [ : as | as key -> as value size ] .
 
g := RTGrapher new.
d := RTData new.
d points: data.
d barChartWithBarTitle: #key.
d y: [ :as | as value ].
d yLog.
g add: d.
g axisY noDecimal.
g axisX noTick.
g
```

![Visualizing the fitness distribution of 1000 individuals.](08-GeneticAlgorithm/figures/distributionResults.png){#fig:distributionResults}

The graphic shows, using a vertical logarithmic scale, that from the 1000 initial individual, 880 individuals have a fitness of 0, 113 individuals have a fitness of 1, and only 7 individuals have a fitness of 2. None have a fitness of 3. 

If we step back a bit, looking for the word _cat_ involves a combination of $$27 * 27 * 27 = 19 683$$. So, we need a number of individuals in that order to find the secret word. Let's try with 100 000 individual this times. The complete script is (Figure @fig:distributionResults2):

```Smalltalk
inds := GAIndividual 
			create: 100000 
			individualsAndInitialize: 3 
			genesWith: [ :r | ($a to: $z) atRandom: r ].
			
fitnessBlock := [ :genes | (genes with: 'cat' asArray collect: [ :a :b | (a == b) 
									ifTrue: [ 1 ] 
									ifFalse: [ 0 ] ]) sum ].

inds do: [ :i | i computeFitnessUsing: fitnessBlock ].

data := ((inds collect: #fitness) groupedBy: #yourself) associations collect: [ : as | as key -> as value size ] .
 
g := RTGrapher new.
d := RTData new.
d points: data.
d barChartWithBarTitle: #key.
d y: [ :as | as value ].
d yLog.
g add: d.
g axisY noDecimal.
g axisX noTick.
g
```

![Visualizing the fitness distribution of 100 000 individuals.](08-GeneticAlgorithm/figures/distributionResults2.png){#fig:distributionResults2}

Locating the mouse above the bar 3 reveals that 4 individuals with the secret words have been created. 

## Crossover Genetic Operation

Genetic algorithm uses genetic operations to produce new individuals. Biology recognizes two operations: _crossover_ that combines two individual to form a new one, and _mutation_ to produce a new individual with sporadic gene variations.

It is important to have our implementation open to new genetic operations. Some particular operations may be crucial to significantly accelerate the convergence toward a solution. In this chapter, we will focus on mutation and crossover. When we will cover neuroevolution, we will need different mutations and crossover operations.

We can define the class `GAOperation` as the root class of all operations. 


```Smalltalk
GAObject subclass: #GAOperation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgo-Core'
```

We can define a hierarchy of crossover 

```Smalltalk
GAOperation subclass: #GAAbstractCrossoverOperation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgo-Core'
```

We can now implement the crossover operation with the following method:

```Smalltalk
GAAbstractCrossoverOperation>>crossover: partnerA with: partnerB
	"Return a new child, which is the result of mixing myself with the argument"
	^ self crossover: partnerA with: partnerB midpoint: (self pickCutPointFor: partnerA)
```

The method `crossover:with:` takes two individuals as argument. A new individual is produced and the genetical information of the parents are mixed. Consider the method:

```Smalltalk
GAAbstractCrossoverOperation>>crossover: partnerA with: partnerB midpoint: midpoint
	"Return a new child, which is the result of mixing myself with the argument"

	| child crossOverGenes |
	child := GAIndividual new.
	child random: random.
	crossOverGenes := (partnerA genes first: midpoint)
		, (partnerB genes allButFirst: midpoint).
	child genes: crossOverGenes.
	^ child
```

The method `crossover:with:midpoint:` accepts a cutting point as the third argument. The call `first: midpoint` returns the first `midpoint` elements, and the call `allButFirst: midpoint` returns the elements after the first `midpoint` elements. For example, we have `'abcdefghi' first: 3` returns `abc` and `'abcdefghi' allButFirst: 3` return `'defghi'`.

We define an abstract method:

```Smalltalk
GAAbstractCrossoverOperation>>pickCutPointFor: anIndividual
	"Need to be overriden in subclasses"
	self subclassResponsibility 
```

The method `pickCutPointFor:` has to be overridden in each subclass. We define the class:

```Smalltalk
GAAbstractCrossoverOperation subclass: #GACrossoverOperation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgo-Core'
```

The class `GACrossoverOperation` override the method `pickCutPointFor:`:

```Smalltalk
GACrossoverOperation>>pickCutPointFor: partnerA
	"Simply return a random number between 1 and the number of genes of the individual provided as argument"
	^ random nextInt: partnerA genes size
```

We can now test our crossover operation:

```Smalltalk
TestCase subclass: #GACrossoverOperationTest
	instanceVariableNames: 'i1 i2 op'
	classVariableNames: ''
	package: 'GeneticAlgo-Tests'
```

The test defines three variables, `i1`, `i2`, and `op`. We initialize these variables in the `setUp` method:

```Smalltalk
GACrossoverOperationTest>>setUp
	super setUp.
	i1 := GAIndividual new genes: 'abcd'.
	i2 := GAIndividual new genes: 'defg'.
	op := GACrossoverOperation new.

```

We can now test different combinations. In the first scenario, the midpoint is 2, which means that the resulting genes will have the first two letters of `i1` and the last two letter of `i2`:


```Smalltalk
GACrossoverOperationTest>>testCrossover1
	| i3 |	
	i3 := op crossover: i1 with: i2 midpoint: 2.
	self assert: i3 genes equals: 'abfg'
```

In this second scenario, the midpoint is 1:

```Smalltalk
GACrossoverOperationTest>>testCrossover2
	| i3 |
	i3 := op crossover: i1 with: i2 midpoint: 1.
	self assert: i3 genes equals: 'aefg'
```

In this third scenario, the midpoint is 0, which means that the resulting individual has all the letters of `i2`:
```Smalltalk
GACrossoverOperationTest>>testCrossover3
	| i3 |
	i3 := op crossover: i1 with: i2 midpoint: 0.
	self assert: i3 genes equals: 'defg'
```

## Mutation Genetic Operation



