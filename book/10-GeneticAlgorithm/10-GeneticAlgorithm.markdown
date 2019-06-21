
# Genetic Algorithm

The first part of the book is about neural network, a computational metaphor for how the brain operates. In this second part, we will focus on evolution in general. Genetic algorithm are a computational metaphor of how evolution naturally happens.

This chapter is self-contained: we therefore expect a reader familiar with Pharo to fully enjoy this chapter, even without reading any previous chapters.

## Natural Evolution Inspired Algorithms

We, as humans beings, are the result of a few thousands years of evolution. Biological evolution refers to some alteration of the heritable characteristics of biological populations over successive generations. Most of the characteristics are the expressions of genes that are passed on from parent to offspring during reproduction.

The _Darwinian natural selection_ stipulates that in order to have a natural evolution, the evolution mechanism needs to support:

- _Heredity_: a child receives a number of properties from its parents. In particular, if parents are robust and can live long enough, then the child should too.
- _Variation_: some variations may be introduced in children. As such, children should not be identical copy of their parents.
- _Selection_: some members of a population must have the opportunity to be parents, and have offspring in order to pass their genetic information. The selection is typically referred to as "survival of the fittest".

Computer scientists have produced a number of algorithms that simulate evolution. For example:

- _ant colony optimization_ is based on the idea that ants are foraging by pheromone communication to form a path. Such algorithm is suitable for graph-related problems;
- _Bees algorithm_ is based on the honey bees foraging behavior. This algorithm is suitable for scheduling and ordering problems. 

We will focus on _genetic algorithm_ (GA). It is an evolutionary algorithm that simulates the evolution of DNA information across a population. Genetic algorithm has three important properties that we will exploit in the book:

- GA is efficient to solve optimization problem;
- GA is easy implemented and does not require a strong theoretical background;
- GA can be easily combined with neural networks. We will go into details in the third part of the book.

## Example of a Genetic Algorithm

The overall idea of genetic algorithm is pretty simple. Imagine a friend asks you to solve the following challenge: _"You must find the 3-letter word I have in mind. For each try, I tell you the number of correct letters."_. Assume that the secret word is _cat_. At first, we give any randomly generated words made of 3 letters, such as _cow_, _poz_, and _gat_.

The word _cow_ has exactly 1 letter in common with _cat_, the secret word. We therefore say that _cow_ as a score of 1. The word _poz_ as a score of 0 since it has no matching letter with the secret word. The word _gat_ has a score of 2 since _at_ are two matching letters.

Since we have not found the solution (_i.e.,_ none of cow, poz, and gat are the secret word), we can produce a new generation of words by doing some combinations of the words we already have. In particular, _gat_ and _cow_ can be combined into _gaw_, _cot_, _gow_. From these three words, the word _cot_ has a score of 2 and is very close to the secret word. We say that this second generation of words is improving by being closer to the solution.

This small example illustrates the overall idea of genetic algorithm: each individual in a set of random individuals is evaluated to compute a score value. Individual with a high score, which are the ones close to solving the problem, are recombined to form new individuals. Before detailling the algorith, we will define the vocabulary we will thouroughly use in the chapter.

## Vocabulary

We have to introduce a few terms to describe the concepts we will use all over the chapter. We will rephrase the example given using these appropriate concepts.

We refer to an _individual_ an element that contains genetic information. Such a genetic information is described as a sequential collection of _genes_. A _gene_ represent an unit of information. A gene could be anything. In the example we previously gave, a gene is simply a letter. An individual is a 3-letter word. 

A _population_ is simply a fixed number of individuals. The population has a constant size, however its composing individuals are replaced at each generation. 

The _fitness function_ indicates how "strong" an individual is. The fitness function is a simple function that takes as argument an individual and produces a numerical value. The whole idea of genetic algorithm is to build and search for individuals that maximize the fitness function. 

## Our implementation

This chapter is long. It contains the whole implementation of a simple genetic algorithm in Pharo. All the presented code is assumed to be part of a package named `GeneticAlgorithm`. 

## Individual

We will first express individuals. We will therefore model a class called `GAIndividual`. We will create the class `GAIndividual` subclass of our custom class `GAObject`. We define `GAObject` as a subclass of `Object` which has a `random` variable. 

Almost all elements involved in a genetic algorithm requires generating random numbers. It is therefore convenient to have the variable defined in the root hierarchy used in our implementation:

```Smalltalk
Object subclass: #GAObject
	instanceVariableNames: 'random'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
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

The method `random:` expects an instance of the class `Random` as argument. We also define a utility method to generate a number between 0.0 and and 1.0:

```Smalltalk
GAObject>>randomNumber
	"Return a number between 0.0 and 1.0"
	^ random next
```

We can define a small utility method used by the subclasses to ensure a random number generator is set:

```Smalltalk
GAObject>>checkForRandomNumber
	self
		assert: [ random notNil ]
		description: 'Need to provide a random number generator'
```

We define the class `GAIndividual` as:

```Smalltalk
GAObject subclass: #GAIndividual
	instanceVariableNames: 'genes fitness'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

Computing the fitness of an individual is an essential piece of the genetic algorithm:

```Smalltalk
GAIndividual>>computeFitnessUsing: fitnessBlock
	"Compute the fitness of myself if not already computed"
	self assert: [ genes notNil ] description: 'Need to have some genes first'.

	"Simply exit if already computed"
	fitness ifNotNil: [ ^ self ].

	"Compute the fitness score"
	fitness := fitnessBlock value: genes
```

We will use a one-argument block to compute its fitness. The block takes as argument the genes of the individual. Shortly we will see some examples.

Once the fitness value is computed, we can get it via an accessor:

```Smalltalk
GAIndividual>>fitness
	"Return the fitness value of the individual"
	^ fitness 
```

Genes from an individual may be accessed using:

```Smalltalk
GAIndividual>>genes
	"Return the collection of genes"
	^ genes
```

Later the genetic operations will set the genes of a particular individual. We therefore need a dedicated method for this:

```Smalltalk
GAIndividual>>genes: someGenes
	"Set the genes of the individual. Used by the genetic operations."
	genes := someGenes
```

The number of genes may be obtained using a dedicated method. This will be useful for the genetic operations:

```Smalltalk
GAIndividual>>numberOfGenes
	"Return the number of genes the individual has"
	^ self genes size
```

A central ability of the class `GAIndividual` is to generate the genes from a block factory:

```Smalltalk
GAIndividual>>set: numberOfGenes genesUsing: geneBlockFactory
	"Public method - Generate the genes of the individual"

	self checkForRandomNumber.
	genes := (1 to: numberOfGenes)
		collect: [ :index | geneBlockFactory cull: random cull: index cull: self ]
```

We can now create a useful factory method, as a class method:

```Smalltalk
GAIndividual class>>create: numberOfIndividuals individualsAndInitialize: numberOfGenes genesWith: geneBlockFactory using: randomNumberGeneration
	"Factory method to easily create a population of Individuals.
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

The random number generator may be omitted using the factory method:

```Smalltalk
GAIndividual class>>create: numberOfIndividuals individualsAndInitialize: numberOfGenes genesWith: geneBlockFactory
 	"Factory method to easily create a number of Individuals.
        numberOfIndividuals : is the number of individuals to return
        numberOfGenes : number of genes each individual should have
        geneBlockFactory : is a one-argument block to generate a gene. It takes a random generator as argument"
	^ self create: numberOfIndividuals individualsAndInitialize: numberOfGenes genesWith: geneBlockFactory using: (Random new seed: 42)
```

We can now test the class `GAIndividual`. We create the test `GAIndividualTest` for that purpose:

```Smalltalk
TestCase subclass: #GAIndividualTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgorithm-Tests'
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

Consider the following script:

```Smalltalk
inds := GAIndividual 
			create: 1000 
			individualsAndInitialize: 3 
			genesWith: [ :r | ($a to: $z) atRandom: r ].
fitnessBlock := [ :genes | (genes with: 'cat' asArray collect: [ :a :b | (a == b) 
									ifTrue: [ 1 ] 
									ifFalse: [ 0 ] ]) sum ].
inds do: [ :i | i computeFitnessUsing: fitnessBlock ].		
```

The script first creates some individuals, each having 3 letters as genes. Notice that the block to create a gene takes as argument a random number generator. This is necessary to be able to recreate the exact same individual. In other words, evaluating the expression given above will create _the exact same_ individuals. The fitness of each individual is then computed. The fitness describes a score on how close to the solution the individual is. 

After executing this short script, each individual has a fitness value. Overall, the fitness value ranges from 0 to 3. An individual with a fitness of 3 means it matches the solution, the genes are equal to `#($c $a $t)`.

We can now render the distribution of the result over the individuals (Figure @fig:distributionResults):

```Smalltalk
...
data := ((inds collect: #fitness) groupedBy: #yourself) associations 
			collect: [ : as | as key -> as value size ] .
 
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

![Visualizing the fitness distribution of 1000 individuals.](10-GeneticAlgorithm/figures/distributionResults.png){#fig:distributionResults}

The graphic shows, using a vertical logarithmic scale, that from the 1000 initial individual, 880 individuals have a fitness of 0, 113 individuals have a fitness of 1, and only 7 individuals have a fitness of 2. None have a fitness of 3. 

If we step back a bit, looking for the word _cat_ involves a combination of $27 * 27 * 27 = 19 683$. So, we need a number of individuals in that order to find the secret word. Let's try with 100 000 individual this times. The complete script is (Figure @fig:distributionResults2):

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

![Visualizing the fitness distribution of 100 000 individuals.](10-GeneticAlgorithm/figures/distributionResults2.png){#fig:distributionResults2}

Locating the mouse above the bar 3 reveals that 4 individuals with the secret words have been created. 

## Crossover Genetic Operation

Genetic algorithm uses genetic operations to produce new individuals. Biology recognizes two operations: _crossover_ that combines two individual to form a new one, and _mutation_ to produce a new individual with sporadic gene variations.

We will provide an implementation of these two operators, but it is important to have our implementation open to new genetic operations. Some particular operations may be crucial to significantly accelerate the convergence toward a solution. In this chapter, we will focus on mutation and crossover. When we will cover neuroevolution, we will need different mutations and crossover operations.

We can define the class `GAOperation` as the root class of all operations. 

```Smalltalk
GAObject subclass: #GAOperation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

We can define a hierarchy of crossover 

```Smalltalk
GAOperation subclass: #GAAbstractCrossoverOperation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

We can now implement the crossover operation with the following method:

```Smalltalk
GAAbstractCrossoverOperation>>crossover: partnerA with: partnerB
	"Return a new child, which is the result of mixing myself with the argument"
	^ self crossover: partnerA with: partnerB midpoint: (self pickCutPointFor: partnerA)
```

The method `crossover:with:` takes two individuals as argument. A new individual is produced and the genetic information of the parents are mixed. Consider the method:

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

The method `crossover:with:midpoint:` accepts a cutting point as the third argument. The call `first: midpoint` returns the first `midpoint` elements, and the call `allButFirst: midpoint` returns the elements after the first `midpoint` elements. For example, we have `'abcdefghi' first: 3` returns `'abc'` and `'abcdefghi' allButFirst: 3` return `'defghi'`.

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
	package: 'GeneticAlgorithm-Core'
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
	package: 'GeneticAlgorithm-Tests'
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

We can also test the method `crossover:with:` using the following test:

```Smalltalk
GACrossoverOperationTest>>testCrossover4
	| i3 |
	op random: (Random seed: 42).
	i3 := op crossover: i1 with: i2.
	self assert: i3 genes equals: 'aefg'
```

## Mutation Genetic Operation

Numerous mutations operations may be defined. Having a dedicated hierarchy in which each subclass express variation in the mutation operations is a natural strategy using an object-oriented programming language.

Consider the abstract class:

```Smalltalk
GAOperation subclass: #GAAbstractMutationOperation
	instanceVariableNames: 'mutationRate'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

The commonalities among all future mutation operation is to have a mutation rate. Typically, the value of that variable is a small positive number, close to 0.0 and significantly less than 1.0. We set it per default:

```Smalltalk
GAAbstractMutationOperation>>initialize
	super initialize.
	self mutationRate: 0.01
```

The variable `mutationRate` may be accessed using:

```Smalltalk
GAAbstractMutationOperation>>mutationRate
	"Return the used mutation rate. Typically, a small positive number, close to 0.0 and significantly less than 1.0"
	^ mutationRate
```

The variable `mutationRate` may be set using:

```Smalltalk
GAAbstractMutationOperation>>mutationRate: aFloat
	"Set the mutation rate. Typically, a small positive number, close to 0.0 and significantly less than 1.0"
	mutationRate := aFloat
```

The key method of the mutation operation class is `mutate:`, which takes as argument an individual and _produce a new individual_, result of mutating the argument:

```Smalltalk
GAAbstractMutationOperation>>mutate: individual
	"Return a new individual (different object than the argument), result of a mutation from the individual provided as argument."
	| newIndividual |
	newIndividual := GAIndividual new.
	newIndividual random: random.
	newIndividual genes: individual genes copy.
	self doMutate: newIndividual.
	^ newIndividual
```

The method `doMutate:` is abstract, as defined as:

```Smalltalk
GAAbstractMutationOperation>>doMutate: individual
	"To be overridden"
	self subclassResponsibility 
```

Most of the mutation operations require a way to create an individual gene. We add the empty method:

```Smalltalk
GAAbstractMutationOperation>>geneFactoryBlock: oneArgBlock
	"Do nothing. May be overridden if necessary"
```

The method will be overridden in subclasses. This method is called by `GAEngine`

We can now define the standard mutation operation. Consider the class:

```Smalltalk
GAAbstractMutationOperation subclass: #GAMutationOperation
	instanceVariableNames: 'geneFactoryBlock'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

The mutation operator object requires a way to define a new gene. We will use the same requirement expressed using the class `GAIndividual`. The variable `geneFactoryBlock` will refers to a one-argument block to create a gene. The block receives a random number as argument. The method `geneFactoryBlock:` sets the block to the operation:

```Smalltalk
GAMutationOperation>>geneFactoryBlock: oneArgBlock
	"The block receive a random number as argument"
	geneFactoryBlock := oneArgBlock
```

The block may be accessed using:

```Smalltalk
GAMutationOperation>>geneFactoryBlock
	"Return the one-arg block used to create a gene"
	^ geneFactoryBlock
```

As a help when using the mutation operation, we can provide some indication in case of improper configuration:

```Smalltalk
GAMutationOperation>>checkForGeneFactory
	self
		assert: [ geneFactoryBlock notNil ]
		description: 'Need to provide a block to create gene'
```

The core method of `GAMutationOperation` is `doMutate:`. Consider the method: 

```Smalltalk
GAMutationOperation>>doMutate: individual
	"Mutate genes of the argument"
	self checkForRandomNumber.
	self checkForGeneFactory.
	1 to: individual genes size do: [ :index | 
		self randomNumber <= mutationRate
			ifTrue: [ individual genes at: index put: (geneFactoryBlock cull: random cull: index cull: individual) ] ]	
```

The class `GAMutationOperation` can be properly tested. Consider the class:

```Smalltalk
TestCase subclass: #GAMutationOperationTest
	instanceVariableNames: 'i op'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Tests'
```

The `setUp` method:

```Smalltalk
GAMutationOperationTest>>setUp
	super setUp.
	i := GAIndividual new genes: 'abcd'.
	op := GAMutationOperation new.
```

We can test the mutation with:

```Smalltalk
GAMutationOperationTest>>testMutation
	| i2 |	
	op random: (Random seed: 7).
	op geneFactoryBlock: [ :r | ($a to: $z) atRandom: r ].
	op mutationRate: 0.5.
	
	i2 := op mutate: i.
	self assert: i2 genes equals: 'xfcd'.
	
	i2 := op mutate: i2.
	self assert: i2 genes equals: 'tfcd'.
	
	i2 := op mutate: i2.
	self assert: i2 genes equals: 'tfjd'.
```

The erroneous case can be tested using:

```Smalltalk
GAMutationOperationTest>>testRandomAndGeneFactoryMustBeSet
	self should: [ op mutate: i ] raise: AssertionFailure.
	
	op random: Random new.
	self should: [ op mutate: i ] raise: AssertionFailure.
	
	op geneFactoryBlock: [ :r | 42 ].
	self shouldnt: [ op mutate: i ] raise: AssertionFailure.
```

## Parent Selection

We will define a hierarchy of selection mechanism. The class `GASelection` is a relatively large and complex class. This class is closely tied to the class `GAEngine` that we will present later in this chapter.

The class `GASelection` may be defined as follow:

```Smalltalk
Object subclass: #GASelection
	instanceVariableNames: 'population fittest initialPopulation fitnessBlock populationSize compareFitness engine'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

`GASelection` references a `population` of `GAIndividual` instances. The purpose of `GASelection` is to pick the `fittest` individual based on a strategy, implemented by a subclass of `GASelection`. The selection also is aware of the `initialPopulation`, necessary to deduce a new `population`, of a size `populationSize`. The `fitnessBlock` gives to the selection the way the fitness of each individual is computed. The variable `compareFitness` references a two-arguments block that is useful to indicates which of two fitness values is the best. In some situations, a high fitness indicates a good individual, in some other situations, a high fitness value may indicates a bad individual. At last, the variable `engine` references the genetic algorithm engine. 

First, we provide a simple constructor for `GASelection`:

```Smalltalk
GASelection>>initialize
	super initialize.
	population := OrderedCollection new.
```
We provide some accessors and mutator methods. Consider the method `engine`:

```Smalltalk
GASelection>>engine
	"Return the GAEngine to which I am associated to"
	^ engine
```

The mutator of `engine` may be:

```Smalltalk
GASelection>>engine: theEngine
	"Set the GAEntine to which I have to be associated with"
	engine := theEngine.
	self checkIfEngineSet
```

We provide a simple guard, defined as:

```Smalltalk
GASelection>>checkIfEngineSet
	self assert: [ engine notNil ] description: 'Should set the engine'.
```

The population may be accessed using:

```Smalltalk
GASelection>>population
	"Return the new population"
	^ population
```

The fitness block may be accessed using `fitnessBlock:`:

```Smalltalk
GASelection>>fitnessBlock: aOneArgBlock
	"The argument is evaluated on the genes of each individual.
	The block argument has to compute the fitness."
	fitnessBlock := aOneArgBlock
```

The fitness block may be accessed using `fitnessBlock`:

```Smalltalk
GASelection>>fitnessBlock
	^ fitnessBlock
```

The fittest element is accessible using the method `fittest`:


```Smalltalk
GASelection>>fittest
	"Return the fittest individual from the new population"
	^ fittest
```

The initial population may be set using a dedicated method:

```Smalltalk
GASelection>>initialPopulation: aPopulationAsIndividuals
	"Set the initial population. This is used to create the new population"
	initialPopulation := aPopulationAsIndividuals.
	self checkIfInitialPopulationSet
```

We provide a new guard that prevent some predictable errors to happen:

```Smalltalk
GASelection>>checkIfInitialPopulationSet
	self assert: [ initialPopulation notNil ] description: 'Should set the initial population'.
	self assert: [ initialPopulation isCollection ] description: 'Has to be a collection'.
	self assert: [ initialPopulation notEmpty ] description: 'Cannot be empty'
```

The way fitness values are compared may be set:

```Smalltalk
GASelection>>compareFitness: aTwoArgBlock
	"Take as argument a two argument block that compare the fitness of two individuals"
	compareFitness := aTwoArgBlock
```

The population size may be read using:
```Smalltalk
GASelection>>populationSize
	"Return the population size"
	^ initialPopulation size
```

And set using:

```Smalltalk
GASelection>>populationSize: anInteger
	"Set the population size"
	populationSize := anInteger
```

Subsequently, we will define a number of essential methods that describes the logic of the selection. The abstract method `createNewPopulation` has to be overridden in subclasses. Its purpose is to create a new population:

```Smalltalk
GASelection>>createNewPopulation
	"Create a new population"
	self subclassResponsibility 
```

An essential method of the class `GASelection` is to be able to actually perform the selection. This is what the method `doSelection` does:

```Smalltalk
GASelection>>doSelection
    self checkIfEngineSet.
    self checkIfInitialPopulationSet.
    populationSize := initialPopulation size.
    fittest := initialPopulation first.
    initialPopulation
        do: [ :ind | 
            ind computeFitnessUsing: fitnessBlock.
            (self isIndividual: ind betterThan: fittest)
                ifTrue: [ fittest := ind ] ].
    self createNewPopulation.
    initialPopulation := population.
```

The method first begins by performing some sanity checks. These checks are intended to help a user to not make incorrect usage of the code. 

Some utility method may be written to use the use of a selection. For example, the crossover operation may be delegated using:

```Smalltalk
GASelection>>crossover: partnerA with: partnerB
	"Return one child"
	^ engine crossover: partnerA with: partnerB
```

Comparison between individual may be defined as:

```Smalltalk
GASelection>>isIndividual: ind betterThan: fittestIndividual
	^ engine isIndividual: ind betterThan: fittestIndividual
```

The mutation operation may be invoked using:

```Smalltalk
GASelection>>mutate: child
	^ engine mutate: child
```

We need the necessary to produce a random number within a particular interval:

```Smalltalk
GASelection>>randomNumber: value
	"Return a number between 1 and value"
	^ engine randomNumber: value
```

Several selections strategies are available. We will focus here on the tournament selection, one of the most efficient selection mechanism. We define the class `GATournamentSelection`:

```Smalltalk
GASelection subclass: #GATournamentSelection
	instanceVariableNames: 'tournamentSize'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

The tournament selection algorithm operates as follow: it randomly picks a number of individual from a population, and make a competition between pair of individuals. The winner individual is returned from the algorithm. In our case, the variable `tournamentSize` indicates how large the tournament should be. Per default, the value may be set to 5:

```Smalltalk
GATournamentSelection>>initialize
	super initialize.
	tournamentSize := 5
```

We implement the algorithm:

```Smalltalk
GATournamentSelection>>getGoodIndividual
	"Return the best individual from tournamentSize individual randomly chosen from the population"
	| best ind |
	best := nil.
	tournamentSize timesRepeat: [ 
		ind := initialPopulation at: (self randomNumber: initialPopulation size).
		(best isNil or: [ compareFitness value: ind fitness value: best fitness ]) 
			ifTrue: [ best := ind ]
	].
	^ best
```

Finally, a new population may be created using: 

```Smalltalk
GATournamentSelection>>createNewPopulation
   "Return a new population made of newly breed individual"
    | partnerA partnerB child |
    population := (1 to: self populationSize) collect: [ :seed |
      engine random: (Random seed: seed).
      partnerA := self getGoodIndividual.
      partnerB := self getGoodIndividual.
      child := self mutate: (self crossover: partnerA with: partnerB).
      child computeFitnessUsing: engine fitnessBlock.
      child.
    ]
```

## Monitoring the Evolution

Being able to monitor the execution of the algorithm is essential. For example, this is important to have a termination condition to indicate when the algorithm has to stop. We will produce a dedicated class to monitor the progresses. Consider the class `GALog`:

```Smalltalk
Object subclass: #GALog
	instanceVariableNames: 'generationNumber timeToProduceGeneration fittestIndividual'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

An instance of `GALog` is associated to a generation and contains some relevant information to indicate progresses of the genetic algorithm.

First, it is relevant to have identified the best individual from a population:

```Smalltalk
GALog>>fittestIndividual
	"Return the best individual of the generation I represent"
	^ fittestIndividual
```

The best individual will be set by the genetic algorithm engine we will soon see:

```Smalltalk
GALog>>fittestIndividual: anIndividual
	"Set the best individual of the generation I represent"
	fittestIndividual := anIndividual
```


The method `fitness` returns the fitness value of the best individual of the population:

```Smalltalk
GALog>>fitness
	"Return the best fitness value of a generation I am representing"
	
	^ fittestIndividual fitness
```	

The number of generation has also to be tracked. The `generationNumber` indicates the number of the generation the log object is referring to:

```Smalltalk
GALog>>generationNumber
	"Return the generation number I represent"
	^ generationNumber
```

Similarly than for the fittest individual, the generation number is set by the engine, as we will soon see:

```Smalltalk
GALog>>generationNumber: generationNumberAsInteger
	"Set the generation number I represent"
	generationNumber := generationNumberAsInteger
```

Monitoring the consumed resources may be employed in some cases. The time taken to produce a new generation is important to track:

```Smalltalk
GALog>>timeToProduceGeneration
	"Time to produce the generation I represent"
	^ timeToProduceGeneration
```

Again, the engine will set it:

```Smalltalk
GALog>>timeToProduceGeneration: anInteger
	"Set the time to produce the generation I represent"
	timeToProduceGeneration := anInteger
```

A simple way to print the result is often useful. We override the method `printOn:` for that purpose:

```Smalltalk
GALog>>printOn: str
	"Provide a descent printing of the log"
	super printOn: str.
	str nextPut: $<.
	str nextPutAll: fittestIndividual genes asString.
	str nextPut: $>.
```


## Engine

The engine is a central class to use the genetic algorithm. It offers methods to configure and run a genetic algorithm. We can define the class:

```Smalltalk
GAObject subclass: #GAEngine
	instanceVariableNames: 'fitnessBlock createGeneBlock numberOfGenes populationSize logs population terminationBlock compareFitness mutationOperator crossoverOperator selection beforeCreatingInitialIndividual'
	classVariableNames: ''
	package: 'GeneticAlgorithm-Core'
```

`GAEngine` is a complex and relatively long class. It has a number of variables:

- `fitnessBlock` is a one-argument block. It takes the genes of each individual as argument and returns the fitness of the individual.
- `createGeneBlock` takes a gene block factory.
- `numberOfGenes` indicates the number of genes each individual has
- `populationSize` indicates the size of the population.
- `logs` refers to a collection of instances of `GALog`. This variables keeps the evolution history of the algorithm
- `population` refers to the actual individual population.
- `terminationBlock` is a block that indicates when the algorithm has to stop. The block represents the termination condition and does not take any argument.
- `compareFitness` is a two-argument block, taking two fitness value. The block indicates which fitness is better than the other.
- `mutationOperator` represents the mutation operator.
- `crossoverOperator` represents the crossover operator.
- `selection` refers to a selection algorithm.
- `beforeCreatingInitialIndividual` contains a one-argument block that is evaluated before an individual of the initial population is created. The block takes a random number generator as argument.

Some accessors are necessary to let the user configure the algorithm. Note that an example of using the algorithm is provided at the end of the chapter. The method `createGeneBlock:` is used to indicates how a gene has to be created:

```Smalltalk
GAEngine>>createGeneBlock: threeArgBlock
	"Three arguments must be provided rand, index, and the individual being filled"
	createGeneBlock := threeArgBlock.
	mutationOperator geneFactoryBlock: threeArgBlock
```

The method `fitnessBlock:` is used to indicate how the fitness has to be computed:

```Smalltalk
GAEngine>>fitnessBlock: aOneArgBlock
	"The argument is evaluated on the genes of each individual.
	The block argument has to compute the fitness. 
	Higher fitness means to be closer to the solution"
	fitnessBlock := aOneArgBlock
```

The `fitnessBlock` may be obtained using a method (the selection algorithm uses it):

```Smalltalk
GAEngine>>fitnessBlock
	"Return the fitness block used by the engine"
	^ fitnessBlock
```

The mutation rate may be as using:

```Smalltalk
GAEngine>>mutationRate: aFloat
	"Set the mutation rate used by the engine. The default value is 0.01"
	mutationOperator mutationRate: aFloat.
```

The number of genes per individual is set:

```Smalltalk
GAEngine>>numberOfGenes: anInteger
	"Set the number of genes each individual will have"
	numberOfGenes := anInteger
```

The crossover operation may be set using the method `crossoverOperator:`:

```Smalltalk
GAEngine>>crossoverOperator: aCrossoverOperator
	"Set the crossover operator used in the algorithm"
	crossoverOperator := aCrossoverOperator.
	crossoverOperator random: random
```

The mutation operation may be set:

```Smalltalk
GAEngine>>mutationOperator: aMutationOperator
	mutationOperator := aMutationOperator.
	aMutationOperator random: random
```


```Smalltalk
GAEngine>>populationSize: anInteger
	"Set the population size"
	populationSize := anInteger
```

The selection operator may be set using a dedicated method:

```Smalltalk
GAEngine>>selection: aSelection
	"Set the selection method to be used to create a new population"
	selection := aSelection.
	aSelection engine: self.
```

Typically, a tournament object is used as argument of `selection:`. The variable `selection` may be accessed using:

```Smalltalk
GAEngine>>selection
	"Return the selection operator"
	^ selection
```

In many situations, a better individual is the one with the highest fitness value:

```Smalltalk
GAEngine>>maximizeComparator
	"A better individual is the one with the highest fitness value"
	compareFitness := [ :f1 :f2 | f1 > f2 ]
```

However, it may happen that a better individual is the one with the lowest value:

```Smalltalk
GAEngine>>minimizeComparator
	"A better individual is the one with the lowest fitness value"
	compareFitness := [ :f1 :f2 | f1 < f2 ]
```

The constructor of the engine is:

```Smalltalk
GAEngine>>initialize
	super initialize.
	logs := OrderedCollection new.
	random := Random seed: 42.
	self endForMaxNumberOfGeneration: 10.
	populationSize := 10.
	self maximizeComparator.
	mutationOperator := GAMutationOperation new.
	mutationOperator mutationRate: 0.01.
	mutationOperator random: random.
	
	crossoverOperator := GACrossoverOperation new.
	crossoverOperator random: random.
	
	self selection: GATournamentSelection new.

	beforeCreatingInitialIndividual := [ :rand | "do nothing" ] 
```

As you can see, several parameters have a default value. Prior to actually run the algorithm, a few evaluation has to be made. The `fitnessBlock` is passed from the engine to the `selection`:

```Smalltalk
GAEngine>>beforeRun
    "Method executed before creating the initial population"
    self checkIfReadyToRun.  
    selection fitnessBlock: fitnessBlock.
    selection populationSize: populationSize 
```

The method `checkIfReadyToRun` raises an exception in case the algorithm is not properly configured:

```Smalltalk
GAEngine>>checkIfReadyToRun
	"Raise an exception if the configuration is not ready to be run"
	self assert: [ fitnessBlock notNil ] description: 'Need to set a fitnessBlock'.
	self assert: [ createGeneBlock notNil ] description: 'Need to set a createGeneBlock'.
	self assert: [ numberOfGenes notNil ] description: 'Need to set how many genes you wish to have, using numberOfGenes:'.
	self assert: [ logs isEmpty ] description: 'Already been run'.
```

In particular, the algorithm can be run if it has a `fitnessBlock`, a `createGeneBlock`, and a `numberOfGenes`. Moreover, it should not have been previously run (_i.e.,_ the variable `logs` is empty).

When the engine is asked to perform a crossover operation, it simply delegates it to the operation object:

```Smalltalk
GAEngine>>crossover: partnerA with: partnerB
	"Perform a crossover operation between the two arguments"
	^ crossoverOperator crossover: partnerA with: partnerB
```

Similarly, when the engine is asked to mutate an individual, it simply delegates it to the corresponding operator:

```Smalltalk
GAEngine>>mutate: individual
	"Mutate the child provided as argument"
	^ mutationOperator mutate: individual
```

The initial population is defined using:

```Smalltalk
GAEngine>>initializePopulation
	self checkForRandomNumber.
	population := OrderedCollection new.
	populationSize
		timesRepeat: [ 
			| ind |
			beforeCreatingInitialIndividual value: random.
			ind := GAIndividual new.
			population
				add:
					(ind
						random: random;
						set: numberOfGenes genesUsing: createGeneBlock) ]
```

It is essential to determine which of two individuals is better. We use the following method:

```Smalltalk
GAEngine>>isIndividual: anIndividual betterThan: aFittestIndividual
	"Compare an individual against the fittest individual of the population"
	^ compareFitness value: anIndividual fitness value: aFittestIndividual fitness
```

The logs may be obtained using a simple variable accessors:

```Smalltalk
GAEngine>>logs
	"Return the logs of the run"
	^ logs copy
```

Note that the `copy` message ensure that the collection `logs` is not modified afterwards. 

Here is the central method of the algorithm. The method `GAEngine>>run` is entry point of the algorithm:

```Smalltalk
GAEngine>>run
    "Public method -- Run the genetic algorithm"

    | t log |
    self beforeRun.
    self initializePopulation.
    selection initialPopulation: population.
    selection compareFitness: compareFitness.
    UIManager default
        informUserDuring: [ :bar | 
            | gen |
            gen := 0.
            [ self shouldTerminate ]
                whileFalse: [ gen := gen + 1.
                    bar label: gen asString.
                    self microPause.
                    t := Time now asSeconds.
                    self produceNewPopulation.
                    
                    log := GALog new.
                    log generationNumber: gen.
                    log fittestIndividual: selection fittest.
                    log timeToProduceGeneration: Time now asSeconds - t.
                    logs add: log ] ]
```

When the algorithm is run, it is essential to let the system notify about its progresses. We therefore add the method `microPause` that makes the current running thread the possibility to let other thread do some work:

```Smalltalk
GAEngine>>microPause
	"Useful when you wish to log in the Transcript and see progresses"
	(Delay forMilliseconds: 1) wait.
	World doOneCycleNow.
```

The method `produceNewPopulation` is central to the engine:

```Smalltalk
GAEngine>>produceNewPopulation
	"This method  
		- produces a new population, put in the variable 'population'
		- select the fittest element of the population"
	selection doSelection.
	population := selection population.
```

We also employ a small utility method to produce random numbers:

```Smalltalk
GAEngine>>randomNumber: maxNumber
	"Return a number between 1 and maxNumber"
	^ random nextInt: maxNumber
```

Result of the algorithm is accessed using the method `result`:

```Smalltalk
GAEngine>>result
	"Return the genes of the fittest individual. This method is expected to be executed after #run has completed"
	^ self logs last fittestIndividual genes
```


## Terminating the Genetic Algorithm

Terminating a genetic algorithm is a sensitive aspect that should be carefully considered. The condition that should be met in order to stop the algorithm may depend on a number of different factors (_e.g.,_ if the exact solution exist and may be found).

The method `shouldTerminate` indicates whether the algorithm has to terminate. If no log has been registered, it means that the algorithm was not run, and in that case, we evaluate the `terminationBlock` variable:

```Smalltalk
GAEngine>>shouldTerminate
	logs ifEmpty: [ ^ false ].
	^ terminationBlock value
```

The following method defines the variable `terminationBlock` according to a particular strategy. The method `endForMaxNumberOfGeneration:` defines a termination condition based on the number of generations. The algorithm stops after a particular number of created generations:

```Smalltalk
GAEngine>>endForMaxNumberOfGeneration: nbOfGenerations
	"End the algorithm after a fixed number of generations"
	terminationBlock := [ logs last generationNumber >= nbOfGenerations ]
```

It may happen that if the fitness is above a particular value, the fittest individual may be considered as an acceptable solution. In such a case, there is no reason to pursue the execution of the algorithm:

```Smalltalk
GAEngine>>endIfFitnessIsAbove: aFitnessValueThreshold
	"End the algorithm if the best fitness value is above a particular threshold"
	terminationBlock := [ logs last fittestIndividual fitness >= aFitnessValueThreshold ]
```

Another strategy is to stop the algorithm if the if no better solution is found for a given number of generation:

```Smalltalk
GAEngine>>endIfNoImprovementFor: nbOfGenerations
	"End if no improvement occurred within a given number of generations"
	^ self endIfNoImprovementFor: nbOfGenerations withinRangeOf: 0
```

Complex strategy may be formulated. For example, `endIfNoImprovementFor:withinRangeOf:` defines a condition based on the number of generations and a range of delta values:

```Smalltalk
GAEngine>>endIfNoImprovementFor: nbOfGenerations withinRangeOf: delta
	"End if no improvement occurred (within a delta value) within a given number of generations"
	terminationBlock := [ 
		(logs last generationNumber >= nbOfGenerations) and: [ 
			| fs |
			fs := (logs last: nbOfGenerations) collect: [ :aLog | aLog fittestIndividual fitness ].
			(fs max - fs min) <= delta
			 ] ]
```


## Testing our Algorithm

We can now test our engine. Consider the class `GAEngineTest`:

```Smalltalk
TestCase subclass: #GAEngineTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'GeneticAlgorithm-Tests'
```

We can now implement the introducing example we use about searching a secret words:

```Smalltalk
GAEngineTest>>testExamples01
	| g |
	g := GAEngine new.
	g populationSize: 1000.
	g numberOfGenes: 4.
	g createGeneBlock: [ :rand :index :ind | ($a to: $z) atRandom: rand ].
	g fitnessBlock: [ :genes | (#($g $a $t $o) with: genes collect: [ :a :b | a = b 
											ifTrue: [ 1 ] ifFalse: [ 0 ] ]) sum ].
	
	g run.
	self assert: g logs first fittestIndividual fitness equals: 2.
	self assert: g logs first fittestIndividual genes equals: #($g $l $t $s).
	self assert: g logs fourth fittestIndividual fitness equals: 4.
	self assert: g logs fourth fittestIndividual genes equals: #($g $a $t $o).
```

## What have we seen in this chapter
This chapter covers the following topics:

- It presents the complete implementation of a genetic algorithm.
- It presents a very simple, but representative, example of finding a word.

The following chapter will build on top of this chapter by showing some more interesting problem to solve using genetic algorithm

