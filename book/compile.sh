#!/bin/bash


FILES=`ls 01-introduction/*.markdown \
	02-Perceptron/*.markdown \
	03-Neuron/*.markdown \
	04-NeuralNetwork/*.markdown \
	05-Learning/*.markdown \
	06-PredictingData/*.markdown`

if [ ! -d "build" ]; then
	mkdir build
fi

pandoc -o build/book.pdf --from markdown  --toc --toc-depth 1 \
	--template mybook \
	--number-sections \
	--top-level-division=chapter \
	--filter pandoc-fignos \
	--listings \
	$FILES
