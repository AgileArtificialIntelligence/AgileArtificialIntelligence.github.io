
# Pong

Pong game is one of the earliest video game ever produced. It describe a tennis-like game in which a ball bounces upon two adervaries rackets. Each player move the racket horizontally. 

Overall, this chapter is about training a neural network to play the pong game. In particular this chapter presents:
- a discussion on how to model the pong game
- a propose an implementation in Pharo of the whole game

## Why are we talking about Pong?

Why having a chapter about the Pong game in the first place? After all, implementing a pong game may looks like to be the perfect programming assignment for new students in computer science. 

So, what is so special about pong? What is the connection between pong and neural networks? Before answering to these question, let's deviate a bit and phrase a more general question: How can we measure the progresses made in the field of artificial intelligence? A _benchmark_ is a standard point of reference in which some techniques may be compared. A common benchmark in the field of artificial intelligence is the _Atari Game Benchmark_ (https://blog.deepsense.ai/solving-atari-games-with-distributed-reinforcement-learning/). This benchmark consists in a compilation of a number of original Atari video games for which top libraries and research institutions are competing for the highest scores.

Pong is part of the Atari Game Benchmark. Modeling this game is therefore relevant in our context.

## Modeling Pong

Figure @fig:pong represents what we will have implemented at the end of the chapter. The game is made of two red rackets, two gray walls, and one small blue ball. 

![Example of a pong game.](08-Pong/figures/pong.png){#fig:pong width=400px}

How can we make a neural network able to process a game configuration, such as the one shows in the figure? What are the important features that have to be considered by a network? Said in other words, what are the variables that one needs to consider in order to adequately play the game? It actually depends on what exactly is the dynamic of the game.

![Modeling pong.](08-Pong/figures/pongModeling.png){#fig:pong width=400px}

We consider a very simple very of the game in which: 
- The ball bounces on the racket without affecting the angle. In other words: you cannot "attack" by favoring a particular angle. As a consequence, you need not need to know the position of the opponent in order play. For the network, it should not make a difference between playing against an opponent or playing against a wall
- A racket can only move horizontally, on the very same line. This means that the only thing that matters is the X position of the racket.
- A racket horizontally moves with a constant increment. This is equivalent of steering the racket with two keys on a keyword. The racket can move left, right, or stay still. The fact there are three possible different actions one can take to handle racket, the neural network provides three outputs values.
- The ball has a position and has a constant speed. The speed is adjusted only when bouncing. As a consequence, the ball entity is described with four values, the X and Y positions, and the X and Y components of its speed. 

Figure @fig:pong illustrates the five variables we need to consider. The neural network we will therefore need for our game has five inputs and three outputs. 



## Learning Pong

## Implementing Pong

