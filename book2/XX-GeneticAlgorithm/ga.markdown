
# Genetic Algorithm

Evolution of life on Earth is one of the most spectacular evolutionary system that we know. The way life has evolved on Earth has triggered numerous attempts to identify necessary ingredients for a successful evolution of complex systems. Numerous algorithms have been produced as a result to capture evolution mechanisms. 

## Evolutionary algorithms

''Evolutionary algorithms'' uses mechanisms inspired by biological evolution, which typically include reproduction, mutation, recombination, and selection. The flow chart of an evolutionary algorithm is relatively simple ([evolutionaryAlgorithm](#evolutionaryAlgorithm)). A population of individual is first produced. Subsequently, a selection is carried out to select relevant individual from the initial population. From these selected individual, that we call "parents", offspring are generated to finally replace the initial population. Assuming an adequate configuration, a succession of generations may lead to a convergence to a set of relevant individuals.

[evolutionaryAlgorithm]: XX-GeneticAlgorithm/figures/evolutionaryAlgorithm.png "Image Title" 
![evolutionaryAlgorithm][evolutionaryAlgorithm] 

[comment]: <> (_Artificial immune system_, _Cultural algorithms_)

Several evolutionary algorithms have been proposed:
- _Ant colony optimization_: technique to find a solution to problems that can be reduced to finding good path trough a graph.
- _Genetic algorithm_: Technique to optimize and search for a solution in a large space by relying on the mutation and crossover genetic operations.
- _Genetic programming_: Similarly to genetic algorithm, genetic programming is about finding an executable program that achieve a good fitness value for a given objective function.
- _Grammatical evolution_: This technique is similar to genetic programming, expect it has a particular set of genetic operators.

Evolutionary algorithms have numerous advantages:
- An evolutionary algorithm is not tied to a particular problem to solve. The exact same algorithm may be applied to solve very different problems. 
- An algorithm may be combined with other optimization techniques. A classical example is to use an evolutionary algorithm to find weights of a neural networks.
- An evolutionary algorithm can solve some problem for which no solution can be easily determined by a human expert. In particular, it may solve a problem without specificying the form or structure of the solution in advance. 


## Genetic Algorithm

A genetic algorithm is a simulation of evolution that uses biological genetic operations. The notion of genetic algorithm appeared in 1967, in J. D. Bagley’s thesis "The Behavior of Adaptive Systems Which Employ Genetic and Correlative Algorithms".

A genetic algorithm is often considered as an optimization method, _i.e.,_ assuming a function $f: X \rightarrow R$, $X$ being a multi-dimensional space, the genetic algorithm finds $x$ such as $f(x)$ is maximal. 



## More to read
- "Genetic Systems Programming: Theory and Experiences", 2006, edited by Ajith Abraham

Example of GA applied to the traveling salesman problem:

http://www.theprojectspot.com/tutorial-post/applying-a-genetic-algorithm-to-the-travelling-salesman-problem/5



## Difference with other techniques
For example, Simulated Annealing (http://www.theprojectspot.com/tutorial-post/simulated-annealing-algorithm-for-beginners/6)