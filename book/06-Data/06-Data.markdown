
# Data classification and regression

This chapter covers the classification and regression of data, which are the most prominent applications of neural networks.

## Support to easily train network

In the previous chapter, we have seen that we can obtain a trained neural network to express the XOR logical gate with:

```Smalltalk
n := NNetwork new.
n configure: 2 hidden: 3 nbOfOutputs: 1.

10000 timesRepeat: [ 
	n train: { 0 . 0 } desiredOutputs: { 0 }.	
	n train: { 0 . 1 } desiredOutputs: { 1 }.
	n train: { 1 . 0 } desiredOutputs: { 1 }.
	n train: { 1 . 1 } desiredOutputs: { 0 }.
].
```

After evaluating this script, the expression `n feed: {1 . 0}` evaluates to `#(0.9735546630024936)`, an array having a float close to 1.

We will 


## Neural network as a Hashmap



## Normalization

Before 

## Iris Dataset

## Classifying


