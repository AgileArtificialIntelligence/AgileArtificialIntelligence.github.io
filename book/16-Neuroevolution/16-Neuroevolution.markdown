
# Neuroevolution

This chapter opens the third and last part of the book. The first part covers the topic of neural network, a metaphor for a biological brain. The second part covers genetic algorithm, a metaphor for mechanism that enables evolution of species. Neuroevolution is a form of artificial intelligence that combines neural network and genetic algorithm. 

## Supervised, Unsupervised Learning, Reinformcement learning

In the first part, we have seen that neural network requires example to operate. For example, in order for a neural network to learn a behavior a labeled dataset has to be provided. This way to `learning` is called _supervised learning_: the algorithm learns from labeled data. 

In many situations, obtaining such a labeled dataset is not problematic. For example, Facebook has a large dataset of labeled pictures. Each time you label a friend in a picture, you provide an example with which Facebook can improve its models. 

Whereas supervised learning finds pattern in a dataset for which we have the right answer, _unsupervised learning_ is about finding patterns without having these right anwers. Algorithms including k-means and autoencoder ar commonly used in unsupervised learning. 

There are some case where having a labeled dataset is difficult, too costly, or even impossible to produce with a satisfactory quality. Autonomous vehicle and gaming may be the most two prominent domains for which having examples of a good quality is difficult. _Reinforcement learning_ a third form of learning in which software agents learns from the environment and make proper decision. Neuroevolution is a technique that is associated to reinforcement learning.

## Core Idea of Neuroevolution

Neuroevolution is a technique that consists in making a network evolve. Along generations, the network become better at recognizing patterns. 


## Extending Our Network

```Smalltalk
NNetwork>>neurons
	"Return the list of neurons contains in the network"
	^ layers flatCollect: #neurons
```

```Smalltalk
NNetwork>>numberOfValues
	"Return the number of weights and biases contained in the network"
	^ (self neurons collect: #numberOfWeights) sum + self neurons size
```

```Smalltalk
NNetwork>>getPossibleCutpoints
	"Return the indexes of each neurons values
	This method is useful when applying genetic algorithm to neural network"

	| result index |
	result := OrderedCollection new.
	index := 1.
	self neurons
		do: [ :n | 
			result add: index.
			index := index + n weights size + 1.
			 ].
	^ result asArray
```

```Smalltalk
NNetwork>>setWeightsAndBias: weightsAndBias
	"Set the weights and bias of each neuron.
	This method is useful when applying genetic algorithm to neural network"

	| index |
	self assert:
		[ self numberOfValues = weightsAndBias size ].
	self assert: [ weightsAndBias allSatisfy: #isNumber ].
	index := 1.
	
	self neurons
		do: [ :n | 
			n weights: (weightsAndBias copyFrom: index to: n numberOfWeights + index - 1).
			index := index + n numberOfWeights.
			n bias: (weightsAndBias at: index).
			index := index + 1 ]
```

```Smalltalk
Neuron>>numberOfWeights
	"Return the number of weights contained in the neuron"
	^ weights size
```


## A First Example Of Neuroevolution

```Smalltalk
data := {
	{0 . 0 . 0} . 
	{0 . 1 . 1} . 
	{1 . 0 . 1} . 
	{1 . 1 . 0} }.

n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 2.	
	
g := GAEngine new.
g populationSize: 500.
g mutationRate: 0.01.
g selection: (GATournamentSelection new).
g endForMaxNumberOfGeneration: 30.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: n getPossibleCutpoints).

g numberOfGenes: n numberOfValues.
g createGeneBlock: [ :rand :index :ind | rand next * 5 - 1 ].
g fitnessBlock: [ :genes | 
	| r |
	n setWeightsAndBias: genes.
	r := (data collect: [ :row | 
				(n predict: row allButLast) = row last ]) select: #yourself.
	(r size / 4) round: 4.
	
	].
g run.
g
```

```Smalltalk
data := {
		{ 0 . 0 . 0 . 0 } .
		{ 0 . 0 . 1 . 1 } .
		{ 0 . 1 . 0 . 2 } .
		{ 0 . 1 . 1 . 3 } .
		{ 1 . 0 . 0 . 4 } .
		{ 1 . 0 . 1 . 5 } .
		{ 1 . 1 . 0 . 6 } .
		{ 1 . 1 . 1 . 7 } 
		}.

n := NNetwork new.
n configure: 3 hidden: 5 nbOfOutputs: 8.

g := GAEngine new.
g populationSize: 500.
g mutationRate: 0.03.
g selection: (GATournamentSelection new).
g endForMaxNumberOfGeneration: 60.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: n getPossibleCutpoints).

g mutationOperator: 
	(GAMutationOperation new).

g numberOfGenes: n numberOfValues.
g createGeneBlock: [ :rand :index :ind | rand next * 10 - 5 ].
g fitnessBlock: [ :genes | 
	| r |
	n setWeightsAndBias: genes.
	r := (data collect: [ :row | 
				(n predict: row allButLast) = row last ]) select: #yourself.
	(r size / data size) round: 4.
	
	].
g run.
g
```

## Iris Dataset

```Smalltalk
"The execution of this script initializes the variable irisData.
This variable is used in the subsequent scripts of this chapter"
irisCSV := (ZnEasy get: 'https://agileartificialintelligence.github.io/Datasets/iris.csv') contents.
lines := irisCSV lines. 
lines := lines allButFirst.
tLines := lines collect: [ :l | 
        | ss |
        ss := l substrings: ','.
        (ss allButLast collect: [ :w | w asNumber ]), (Array with: ss last) ].

irisData := tLines collect: [ :row | 
        | l |
        row last = 'setosa' ifTrue: [ l := #( 0 ) ].
        row last = 'versicolor' ifTrue: [ l := #( 1 ) ].
        row last = 'virginica' ifTrue: [ l := #( 2 ) ].
        row allButLast, l ].
    
irisData.
	
n := NNetwork new.
n configure: 4 hidden: 6 nbOfOutputs: 3.

g := GAEngine new.
g populationSize: 500.
g mutationRate: 0.1.
g selection: (GATournamentSelection new).
g endForMaxNumberOfGeneration: 30.
g crossoverOperator: (GAConstrainedCrossoverOperation new possibleCutpoints: n getPossibleCutpoints).

g mutationOperator: 
	(GAMutationOperation new).

g numberOfGenes: n numberOfValues.
g createGeneBlock: [ :rand :index :ind | rand next * 10 - 5 ].
g fitnessBlock: [ :genes | 
	| r |
	n setWeightsAndBias: genes.
	r := (irisData collect: [ :row | 
				(n predict: row allButLast) = row last ]) select: #yourself.
	(r size / irisData size) round: 4.
	
	].
g run.
g
```


