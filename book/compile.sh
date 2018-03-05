#!/bin/bash

pandoc -o book.pdf --from markdown  --toc --toc-depth 1 \
	--template mybook \
	--number-sections \
	--top-level-division=chapter \
	--filter pandoc-fignos \
	--listings \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown \
	03-Neuron/*.markdown \
	04-NeuralNetwork/*.markdown \
	XX-GeneticAlgorithm/*.markdown

