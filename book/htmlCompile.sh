#!/bin/bash

pandoc -o book.html \
	01-introduction/*.markdown \
	02-Perceptron/*.markdown
