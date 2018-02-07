#!/bin/bash

pandoc -o book.pdf --from markdown --top-level-division=section --toc --toc-depth 1 --template eisvogel --listings --number-sections --listings --top-level-division=chapter \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown \
	03-Neuron/*.markdown \
	04-NeuralNetwork/*.markdown \
	XX-GeneticAlgorithm/*.markdown

