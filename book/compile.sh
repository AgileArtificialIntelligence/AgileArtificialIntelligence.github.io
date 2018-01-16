#!/bin/bash

pandoc -o book.pdf --from markdown --top-level-division=chapter --toc --toc-depth 2 --template eisvogel --listings --number-sections --listings --top-level-division=chapter \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown \
	03-Neuron/*.markdown \
	04-NeuralNetwork/*.markdown 

