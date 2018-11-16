Class {
	#name : #Neuron,
	#superclass : #Object,
	#instVars : [
		'weights',
		'bias',
		'learningRate'
	],
	#category : #NeuralNetwork
}

{ #category : #accessing }
Neuron >> bias [
    ^ bias
]

{ #category : #accessing }
Neuron >> bias: aNumber [
    bias := aNumber
]

{ #category : #accessing }
Neuron >> feed: inputs [
    | z |
    z := (inputs with: weights collect: [ :x :w | x * w ]) sum + bias.
    ^ z > 0 ifTrue: [ 1 ] ifFalse: [ 0 ].
]

{ #category : #initialization }
Neuron >> initialize [
	super initialize.
	learningRate := 0.1
]

{ #category : #accessing }
Neuron >> learningRate [
	"Return the learning rate of the neuron"
	^ learningRate
]

{ #category : #accessing }
Neuron >> learningRate: aNumber [
	"Set the learning rate of the neuron"
	learningRate := aNumber
]

{ #category : #learning }
Neuron >> train: inputs desiredOutput: desiredOutput [
    | theError output newWeight |
    output := self feed: inputs.
    theError := desiredOutput - output.
    inputs
        withIndexDo: [ :anInput :index | 
            newWeight := (weights at: index) + (learningRate * theError * anInput).
            weights at: index put: newWeight ].
    bias := bias + (learningRate * theError)
]

{ #category : #accessing }
Neuron >> weights [
    ^ weights
]

{ #category : #accessing }
Neuron >> weights: someWeightsAsNumbers [
    weights := someWeightsAsNumbers
]
