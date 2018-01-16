#!/bin/bash

#	--mathml \
# 	
# 	--mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML \


pandoc -o book.html --number-sections --top-level-division=chapter \
	--self-contained --toc --toc-depth 2 \
	--mathml \
	--template template.html --css template.css \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown \
	03-Neuron/*.markdown

