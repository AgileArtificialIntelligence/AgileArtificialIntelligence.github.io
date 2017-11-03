# Neuron

## Limit of the Perceptron

~~~~~~~
((1 to: 1000) collect: [ :i |
	f := [ :x | x * -2 - 40 ].
	r := Random new seed: 42.
	
	p := MPPerceptron  new.
	p weights: { -1 . -1 }.
	p bias: -0.5.

	i timesRepeat: [ 
			x := (r nextInt: 100) - 50.
			y := f value: x.
			o := (y >= 0) ifTrue: [ 1 ] ifFalse: [ 0 ].
			p train: { x . y } desiredOutput: o.
			].
		
	error := 0.
	100 timesRepeat: [ 
			x := (r nextInt: 100) - 50.
			y := f value: x.
			o := (y >= 0) ifTrue: [ 1 ] ifFalse: [ 0 ].
			error := error + (o - (p feed: { x . y })) abs.
	].
	error := error / 100.
	error := error asFloat round: 2.
	error ]) plot
~~~~~~~

## The Sigmoid Neuron



~~~~~~~
MPNeuron>>feed: inputs
	| z |
	self assert: [ inputs isCollection ] description: 'Should be a collection'.
	self assert: [ inputs size = weights size ] description: 'Input should have the same size then the weights’.

	z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias .
	output := 1 / (1 + z negated exp).
	^ output
~~~~~~~
