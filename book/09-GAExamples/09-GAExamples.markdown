
# Genetic Algorithm in Action

This chapter uses genetic algorithm to find a solution to a number of difficult algorithmic problems. 

## Fundamental Theorem of Arithmetic

A prime number is a whole number greater than 1 whose only factors are 1 and itself. For example, 7 is prime because it can only be divided by 7 or by 1. The number 10 is not prime because it can be divided by 2 or by 5, which are both prime numbers. 

In the theory of number, the is a theorem called _the fundamental theorem of arithmetic_, which states _"any integer greater than 1 is either a prime number itself, or can be written as a unique product of prime numbers."_ Note that this representation is unique, except for the order of the factors. For example, we have $345 = 3 * 5 * 23$. 

We will use genetic algorithm to identify the prime factors for any given number. A gene will represents the prime factors. Note that since the genes has a fixed size, we will also consider the value 1 as a possible factor. 

The fitness function is simply the absolute difference between the multiplication of the prime factors and the number we are interested in looking for the factors. If the fitness is equal to 0, then we found the solution.

Consider the following script:

```Smalltalk
	numberOfIdentifyFactors := 345.
	primeNumbers := #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199).
	candidateFactors := #(1), primeNumbers.
	g := GAEngine new.
	g endIfNoImprovementFor: 10.
	g populationSize: 10000.
	g numberOfGenes: 10.
	g createGeneBlock: [ :rand :index :ind | candidateFactors atRandom: rand ].
	g minimizeComparator.
	g
		fitnessBlock: [ :genes | 
			((genes inject: 1 into: [ :r :v | r * v ]) - numberOfIdentifyFactors) abs ].
	g run.
```	

The fitness function contains the expression `genes inject: 1 into: [ :r :v | r * v ]` which returns the multiplication of the numbers contained in the `genes` temporary variable. For example, `#(3 5 23) inject: 1 into: [ :r :v | r * v ]` evaluates to `345`. 

After the execution of the script, we can verify how it did go with the expression:
```Smalltalk
g logs last fitness.
```

If the value is `0`, then we found the exact prime factors. If we did not found it, then we could increase the population size and / or increase the argument of `endIfNoImprovementFor:`. 

The prime factors may be obtained using the expression:

```Smalltalk
	g result copyWithout: 1.
```

![Identification prime factors of `345`.](09-GAExamples/figures/primeNumbers.png){#fig:primeNumbersOf345}

Figure @fig:primeNumbersOf345 gives the results of executing the whole script.

For any non-prime number, the sequence of prime factor is unique. Which means that the number `345` can only be decomposed into the group of prime factors `3`, `5`, and `23`. There is no other combination of prime factors that produce `345`. The prime factors therefore constitute an "identity" of the `345` composite number. The fundamental theorem of arithmetic has a well chosen name. This theorem has many applications, and one of them is cryptography. If cryptography is as important as it is today, it is essentially due to this theorem. In cryptography, a prime factor represents a private key, and if the composite number is large enough, then it takes an incredible amount of time to actually find the prime factors.

## Knapsack Problem

The knapsack problem is a well-known problem in combinatorial optimization. It can be summarized as follows: given a set of items, each having a value and a weight, determine the number of each item to include in a collection, such that (i) the total weight is less or equal a given limit and (ii) the total value is as large as possible. 

We will consider two variants of the problem, _unbounded knapsack problem_ and _0-1 knapsack problem_.

![Knapsack problem (obtained from Wikipedia, authored by Dake, under Creative Commons Attribution-Share Alike 2.5 Generic).](09-GAExamples/figures/knapsack.png){#fig:knapsackExample width=250px}

Figure @fig:knapsackExample illustrates the knapsack problem. Five boxes are available, each having a particular value and weight. The bag cannot held contains more than 15 kilograms of boxes. If we consider the unbounded variant of the problem, then the solution is three boxes of 10\$ and three boxes of 2\$. If we consider the 0-1 variant, then all the boxes exact the 4\$ one. 


### The _unbounded knapsack problem_ variant

In this variant, a box may be used multiple times. We use the genetic algorithm to search for the optimal solution. The fitness function reflect the value of a given set of boxes (the sum of the value) minus a penalty. This penalty is the difference between the total weight with the knapsack capacity. Consider the script:

```Smalltalk
	knapsackMaxWeight := 15.
	"a box = (value, weight)"
	boxes := #(#(4 12) #(2 1) #(2 2) #(1 1) #(10 4) #(0 0)).
	
	g := GAEngine new.
	g endIfNoImprovementFor: 10.
	g populationSize: 20000.
	g numberOfGenes: 15.
	g createGeneBlock: [ :rand :index :ind | boxes atRandom: rand ].
	g maximizeComparator.
	g
		fitnessBlock: [ :genes | 
			| totalWeight totalValue penalty |
			totalValue := (genes collect: #first) sum.
			totalWeight := (genes collect: #second) sum.
			knapsackMaxWeight < totalWeight 
				ifTrue: [ penalty := (knapsackMaxWeight - totalWeight) abs * 50 ]
				ifFalse: [ penalty := 0 ].
			totalValue - penalty
			 ].
	g run.
	g result copyWithout: #(0 0)
```

The variable `knapsackMaxWeight` refers to the knapsack capacity. The variable `boxes` contains all the available boxes. Each box is represented as a tuple `(value, weight)`. Note that we added an empty box `#(0 0)`. This empty box is important because of the number of considered genes is fixed. We have arbitrarily chosen 15 genes. A greater number of genes would not be meaningful since the the sum of 16 or more boxes will be strictly greater than 15. 

The fitness function contains three variables. The variable `totalValue` sums up the value of the set of boxes contained in the `genes` variable. The variable `totalWeight` is the boxes' weight. We defined a variable `penalty` which is the absolute difference between the bag capacity and the `totalWeight`. We use a factor 50 to actually make sure that the value does not take over the penalty. 

![Genetic algorithm applied to the Knapsack problem.](09-GAExamples/figures/knapsackAndGA.png){#fig:knapsackAndGA}

Figure @fig:knapsackExample illustrates the execution of the script. 

### The _0-1 knapsack problem_ variant

In this variant, each available box appears at most once. We treat this problem in a similar way than previously, however, the encoding and decoding of the genes has to reflect the fact that each box can appear at most once in the genome.

The key aspect to consider when solving this variant, is to realize that this problem is actually similar to searching for a sequence of $0$ and $1$. Assuming that the set of boxes is fixed and ordered, as we have specified so far, then we can assign the value $0$ to a box to indicate that the box is absent from a solution. Similarly, the value $1$ indicates that the box is present.

Consider the following script:

```Smalltalk
	knapsackMaxWeight := 15.
	"a box = (value, weight)"
	boxes := #(#(4 12) #(2 1) #(2 2) #(1 1) #(10 4) ).
	
	g := GAEngine new.
	g endIfNoImprovementFor: 10.
	g populationSize: 20000.
	g numberOfGenes: boxes size.
	g createGeneBlock: [ :rand :index :ind | #(0 1) atRandom: rand ].
	g maximizeComparator.
	g
		fitnessBlock: [ :genes | 
			| totalWeight totalValue penalty |
			decodeToBoxes := OrderedCollection new.
			genes doWithIndex: [ :b :ind | b = 1 ifTrue: [ decodeToBoxes add: (boxes at: ind) ] ].
			decodeToBoxes 
				ifEmpty: [ totalValue := 0. totalWeight := 0 ]
				ifNotEmpty: [ 
					totalValue := (decodeToBoxes collect: #first) sum.
					totalWeight := (decodeToBoxes collect: #second) sum ].
			knapsackMaxWeight < totalWeight 
				ifTrue: [ penalty := (knapsackMaxWeight - totalWeight) abs * 50 ]
				ifFalse: [ penalty := 0 ].
			
			totalValue - penalty
			 ].
	g run.

decodeToBoxes := OrderedCollection new.
g result doWithIndex: [ :b :ind | b = 1 ifTrue: [ decodeToBoxes add: (boxes at: ind) ] ].
decodeToBoxes 
```

Boxes selected by the algorithm are `#(2 1) #(2 2) #(1 1) #(10 4)`. We will now detail the script. 
A gene is either a value `0` or `1`. The fitness function first select the boxes indicated by the set of `0` and `1` contained in the variable `genes`. Boxes that are part of the current solution are kept in the variable `decodeToBoxes`. We need to verify whether `decodeToBoxes` is empty or not. It may be empty if the `genes` variable is only made up of `0`. Once we have the total value and the total weight indicated by the `genes` variable, we need to set the penalty to the difference between `knapsackMaxWeight` and `totalWeight`, as we previsouly did. 

### Coding and encoding

Conceptually, the two variants of the knapsack problem differs in the range of occurences each box may appear. As we have seen, this may have an impact on how to encode a possible solution. In the unbounded variant we have a set of boxes while in the 0-1 variant we have a set of $0$ and $1$ as a solution. 


## Meeting Room Scheduling Problem

The meeting room scheduling is a classical problem that consists in assigning meetings to different rooms. Meetings should not overlap while using the minimum number of different rooms. To illustrate the problem, we consider a meeting as a tupple $(start time, end time)$. The two meetings `#(#(1 3) (2 3))` do overlap, as a consequence, we need to have each meeting in a different room. Oppositely, the two meetings `#(#(1 3) (4 5))` can be held in the same room.

Consider the following meetings: `#(#(1 3) #(2 3) #(5 6) #(7 9) #(4 7))`. Two rooms are necessary since the meetings `#(2 3)` and `#(4 7)` can be held in a room, and `#(1 3)`,`#(5 6)`, and `#(7 9)` in another room.

We can use genetic algorithm to identify a number of minimum room necessary to held a set of provided meetings. Consider the script:

```Smalltalk
	"We assume that each meeting is correctly defined"	
	"a meeting = (start time, end time)"
	meetings := #(#(1 3) #(2 3) #(5 6) #(7 9) #(4 7)).
	numberOfMeetings := meetings size.
	
	g := GAEngine new.
	g endIfNoImprovementFor: 10.
	g populationSize: 20000.
	g numberOfGenes: numberOfMeetings.
	g createGeneBlock: [ :rand :index :ind | (1 to: numberOfMeetings) atRandom: rand ].
	g minimizeComparator.
	g
		fitnessBlock: [ :genes | 
			| distribution |
			distribution := OrderedCollection new.
			numberOfMeetings timesRepeat: [ distribution add: OrderedCollection new ].
			genes doWithIndex: [ :roomNumber :index | (distribution at: roomNumber) add: (meetings at: index) ].
			
			numberOfOverlap := 0.
			distribution do: [ :aSetOfMeetings |
				table := OrderedCollection new: 10 withAll: 0.
				aSetOfMeetings do: [ :meet |
					meet first to: meet second do: [ :v | table at: v put: (table at: v) + 1 ]
				].
				numberOfOverlap := numberOfOverlap + (table select: [ :v | v >= 2 ]) size.
			].

			(distribution select: #notEmpty) size + numberOfOverlap.
			 ].
	g run.
g result asSet size
```

The variable `meetings` contains the list of meetings. We are here assuming that each meeting is correctly defined (_e.g.,_ the end time is strictly greater than the start time). The variable `numberOfMeetings` contains the number of meetings we have. 

We consider a gene as a room assignation for a particular meeting. If we consider the set of meeting `#(#(1 3) #(2 3) #(5 6) #(7 9) #(4 7))`, then a possible solution is `#(1 5 1 1 5)`, which means that the meetings `#(1 3)`, `#(5 6)`, `#(7 9)` are held in room `1` while `#(2 3)`, `#(4 7)` in room `5`. The solution to these meetings is two rooms. 

Since we wish to minimize the number of rooms and the number of overlaps, the genetic algorithm will look for room assignments that minimize the fitness function.

The fitness function computes the number of different room and the number of overlaps. Finally, the number of different room is given by the expression `g result asSet size`. 

## What have we seen in this chapter?

The chapter presents three examples on how genetic algorithm can be efficiently employed to find a solution to some optimization problems. As we have said, the genetic algorithm does not guarantee that the result is actually the optimal solution. 