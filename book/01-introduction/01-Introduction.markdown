---
title: "Agile Artificial Intelligence: Programming Neural Networks in Pharo"
author: [Alexandre Bergel]
date: 2018-04-17
subject: "Agile Artificial Intelligence"
tags: [AI, Neural networks, Pharo]
titlepage: true
...

# Introduction


## The book series

The field of artificial intelligence is experiencing a massive adoption and growth. _Agile Artificial Intelligence_ is a book series that covers expressive common algorithms, often considered as central to the field. The first contribution made by this book series is to bring agility in the way techniques related to artificial intelligence are designed, implemented, and evaluated. 

The second contribution is making these techniques accessible by detailing their implementation without overwhelming the reader with mathematical material. Although there is nothing wrong with giving such formal material, there is often a significant gap between mathematical formulas and producing executable source code, unfortunately. Reducing the gap between mathematical knowledge and producing runnable models is a cornerstone of the book series.


## Why this book?

This first book covers neural networks and exposes some applications of them. We voluntarily excluded image and natural language processing as they deserve to be treated individually.

There exist many sophisticated libraries to build, train, and run neural networks. So, why yet another book on neural networks?

Most artificial intelligence techniques are often perceived as a black box that operates in an almost magical way. This black box may appear either as sophisticated mathematical notations or as closed-world libraries.

The purpose of the _Agile Artificial Intelligence_ series is to reveal some of the most arcane algorithms. This book details how neural networks may be written from scratch, without any dedicated libraries. Understanding the machinery behind fantastic algorithms and techniques is a natural goal that one should pursue. This allows one (i) to easily hook into these algorithms when necessary (for example, if one wish to hook an evolutionary algorithm into a neural network or wish to manipulate any arbitrary non-fully connected network) and (ii) to understand the scope of these algorithms. 

This book is meant to detail some easy-to-use recipes to solve actual problems and highlights some technical details using the Pharo programming language. 

## Who should read this book?

This book provides relevant material to practitioners having an interest in learning, implementing, and applying neural networks to solve classification and regression problems. 

This book is designed to be read by a large audience. In particular, there is no need to have prior knowledge on neural networks. There is even no need to have a strong mathematical background. We made sure that there is no such prerequisite in reading most of the chapters.
Some chapters require a mild mathematical background. However, these chapters are self-contain and skipping them will not hamper the overall understanding.

## What if I am not a Pharo programmer?

Code provided in this book uses the Pharo programming environment. Programming in Pharo brings a fantastic experiment. Literally, Pharo gives a meaning to agile programming that cannot be experienced in another programming language. We will try to convey this wonderful experience to the reader.

Pharo has a very simple syntax, which means that code should be understandable as soon as you have some programming knowledge. Chapter 2 briefly introduces the Pharo programming language and its environment. 

Why Pharo? The cornerstone of Pharo is its programming environment. Pharo syntax is concise and simple. However, the strength of Pharo is not its syntax, but the environment it offers. If you do not know Pharo, we strongly encourage you to become familiar with the basics of its programming environment. The debugger, inspector, and playground are unrivaled compared to other programming languages and environments. Using these tools really brings an unmatchable feeling when programming.

## Additional Readings

Here are some good readings about Pharo:

- http://pharo.org is the official website about Pharo.
- http://books.pharo.org contains many books and booklets about Pharo.
- http://agilevisualization.com describes the Roassal visualization engine, which also provides a gentle introduction to Pharo.
