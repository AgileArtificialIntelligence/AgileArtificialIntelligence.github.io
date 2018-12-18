#!/bin/bash

#	--mathml \
# 	
# 	--mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML \

# 	--self-contained \




FILES=`ls \
	08-GeneticAlgorithm/*.markdown \
	09-GATuning/*.markdown`


#FILES=`ls 06-Data/*.markdown`


if [ ! -d "build" ]; then
	mkdir build
fi

for originalFile in $FILES
do
	HTMLFILENAME=build/$(echo $originalFile | tr "/" " " | cut -f1 -d " ").html
	echo "Generating " $HTMLFILENAME

	pandoc -o $HTMLFILENAME --self-contained \
		--number-sections --top-level-division=chapter \
		--toc --toc-depth 2 \
		--filter pandoc-fignos \
		--mathml \
		--template template.html --css Template/template.css \
		$originalFile
done




