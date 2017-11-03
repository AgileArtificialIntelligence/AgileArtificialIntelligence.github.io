#!/bin/bash

pandoc -o book.pdf \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown
