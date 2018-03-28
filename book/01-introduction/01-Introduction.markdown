---
title: "Agile Artificial Intelligence"
author: [Alexandre Bergel]
date: 2017-12-13
subject: "Agile Artificial Intelligence"
tags: [AI, Neural networks, Pharo]
titlepage: true
...

# Introduction


## The book series

The field of artificial intelligence is experiencing a massive adoption and growth. _Agile Artificial Intelligence_ is a book series that covers expressive and popular algorithms that are central to the field. The main contribution made by this book series is to bring agility in the way techniques related to artificial intelligence are designed, implemented, and evaluated. 

The first book, the one in your hand, covers neural networks and expose some applications of it. We voluntarily excluded image and natural language processing as their deserve to be treated individually.



## Why this book?

There exist many sophisticated libraries to build, train, and run neural networks. So, why yet another book on neural network?

Most of artificial intelligence techniques are often perceived as a black box that operates in an almost magical way. This black box may appear either as sophisticated mathematical notations or as closed-world libraries.

The purpose of the _Agile Artificial Intelligence_ series is to reveals some of the most arcane algorithms. This book details how neural networks may be written from scratch, without any supporting libraries. Understanding the machinery behind fantastic algorithms and techniques is a natural goal that one should pursue. This allows one (i) to easily hook into these algorithms when necessary and (ii) to understand the scope of these algorithms. 

This book is meant to detail some easy-to-use recipes to solve punctual problems and highlights some technical details using the Pharo programming language. 

## Who should read this book?

This book provides relevant material to practitioners having an interest in learning, implementing, and applying neural networks to solve classification or regression problems. 

This book is designed to be read by a large audience. In particular, there no need to have knowledge in neural networks. There is even no need to have a strong mathematical background. We made sure that there is no such prerequisite in reading most of the chapters.
Some chapters requires a mild mathematical background. However, these chapters are optional and skipping them will not hamper the overall understanding.

## What if I am not a Pharo programmer?

Code provided in this book use the Pharo programming language. Using Pharo is very convenient to apply agile programming. Historically, agile programming is born in Smalltalk, the ancestor of Pharo.

Pharo has a very simple syntax, which means that code should be understandable as soon as you have some programming knowledge. Chapter 2 briefly introduces the Pharo programming language and environment. 

Why Pharo? The cornerstone of Pharo is its programming environment. Pharo syntax is concise and simple. However, the strength of Pharo is not its syntax, but the environment it offers. If you do not know Pharo, we do strongly encourage to get to get the basis of its programming environment. The debugger, inspector, and playground have no matches in other programming languages and environment. Using these tools really bring an unmatchable feeling when programming.

## Additional Readings

If you want to know more about some of the techniques presented here, we recommend the following books:

- http://www.gp-field-guide.org.uk by Riccardo Poli, Bill Langdon, and Nic McPhee.
- http://natureofcode.com by Daniel Shiffman.
- http://neuralnetworksanddeeplearning.com by Michael Nielsen.

Some good readings about Pharo:

- http://pharo.org is the official website about Pharo.
- http://books.pharo.org contains many books and booklets about Pharo.
- http://agilevisualization.com describes the Roassal visualization engine, which also provide a gentle introduction to Pharo.
