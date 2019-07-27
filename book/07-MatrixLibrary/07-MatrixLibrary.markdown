
# A Matrix Library

In the previous chapters we presented an implementation of a neural network made of layers and neurons (_i.e.,_ instances of `NeuronLayer` and `Neuron`). Although instructive, our implementation does not reflects classical ways of implementing a neural network. A layer can be expressed as a matrix of weights and a vector of biases. This is actually how most libraries to build neural networks (_e.g.,_ TensorFlow and PyTorch) actually operate.

This chapter lays out a small library to build and manipulate matrices. This chapter is important for the subsequent chapter which is about how networks can be implemented using matrices. Matrix are particular data structure for which operations cannot efficiently be implemented in Pharo. We will write such costly operations in C but make them accessible within Pharo.

In addition to defining a matrix library, this chapters highlights one particular aspect of Pharo, which is the use of Foreign Function Interface (FFI). This is a relevant mechanism whenever one wishes to make Pharo use external libraries written using the C or C++ programming languages. For example, TensorFlow is written in C++, which may be accessed from Pharo using the very same technique presented in this chapter.

This chapter is long and contains many inter-dependent methods. The chapter needs to be fully implemented before being functional.

## Matrix operations in C

Pharo does not provide built-in features to manipulate matrices. Although we could implement them in Pharo, it would suffer from very poor performances. Instead, we will code a small library in C to support the elementary C operations. Create a file named `matrix.c` with the following C code:

```C
void dot(double *m1, int m1_nb_rows, int m1_nb_columns, double *m2,
			int m2_nb_rows, int m2_nb_columns, double *res) { 
              
    int col, row, k; 
    for (col = 0; col < m2_nb_columns; col++) { 
        for (row = 0; row < m1_nb_rows; row++) { 
			double tmp = 0;
            for (k = 0; k < m2_nb_rows; k++) 
				tmp += m1[row * m1_nb_columns + k] * m2[k * m2_nb_columns + col];
				res[row * m2_nb_columns + col] = tmp;
} } } 
void sub(double *m1, int nb_rows, int nb_columns, 
			double *m2, double *res) {
   int col, row; 
   for (col = 0; col < nb_columns; col++) { 
        for (row = 0; row < nb_rows; row++) { 
			res[row * nb_columns + col] = 
				  m1[row * nb_columns + col] - m2[row * nb_columns + col];
} } } 
void add(double *m1, int nb_rows, int nb_columns, 
			double *m2, double *res) {
   int col, row; 
    for (col = 0; col < nb_columns; col++) { 
        for (row = 0; row < nb_rows; row++) { 
			   res[row * nb_columns + col] = 
				  m1[row * nb_columns + col] + m2[row * nb_columns + col];
} } } 
```

This small libraries is composed of three C functions:

- `dot` to perform the multiplication of matrices;
- `sub` to subtract one matrix from another;
- `add` to sum two matrices.

We will not go into details about this C file. It simply applies some basic matrix operations. Each function takes as argument a pointer to somes matrices along with their shape. The library has to be compiled. We can use the gcc standard compiler for this. Assuming you have `gcc` installed, in a terminal you should type:

```bash
gcc -dynamiclib -o matrix.dylib matrix.c
```

Our matrix file is compiled as a dynamic library, loadable within Pharo. The compilation produces a dynamic library. On OSX, the generated file is named `matrix.dylib`. The `matrix.dylib` file _has to be_ located next to the `.image` file, within the same folder.

## The Matrix class

We can now write the Pharo class `MMatrix` which will use our C-library. Note that Pharo 7 contains a deprecated class `Matrix` not really useful for our purpose, which is why our class is prefixed with an additional `M` character. In a new package called `Matrix`, we define the class:

```Smalltalk
Object subclass: #MMatrix
	instanceVariableNames: 'nbRows nbColumns array'
	classVariableNames: ''
	package: 'Matrix'
```

The two first variables describe the shape of the matrix while the variable `array` will refer to an array containing the actual values of the matrix, in a linear fashion. This array will have to be accessible both from Pharo and from our C library.

On the class side of the class `MMatrix` we define a number of useful methods to create matrices. You need to switch the class browser to the class mode to define class methods. The method `newFromArrays:` creates a matrix from a collection of arrays:

```Smalltalk
MMatrix class>>newFromArrays: arrays
	"Create a matrix from an array containing the structured 
	values of the matrix. Example of matrix creations:
	MMatrix newFromArrays: #(#(1 2 3) #(4 5 6))
	MMatrix newFromArrays: #(#(1 2 3))
	MMatrix newFromArrays: #(#(1) #(2) #(3))
	"
	^ self basicNew
		initializeRows: arrays size columns: arrays first size;
		fromContents: (arrays flatCollect: #yourself);
		yourself
```

We also need a lower level to create a matrix, simply by providing the shape of the matrix. This assumes that the matrix content is set later. Consider this new class method:

```Smalltalk
MMatrix class>>newRows: numRows columns: numColumns
	"Create a matrix with a given shape"
	^ self basicNew
		initializeRows: numRows columns: numColumns;
		yourself
```

We then define a method to initialize a matrix:

```Smalltalk
MMatrix>>initializeRows: numRows columns: numColumns
	self initialize.
	nbRows := numRows.
	nbColumns := numColumns.
	array := self newArray
```

The array is useful to keep the matrix content is defined using `newArray`:

```Smalltalk
MMatrix>>newArray
	"Create an array used to contains the store the matrix content"
	^ FFIExternalArray 
		newType: 'double' 
		size: nbColumns * nbRows
```

The class `FFIExternalArray` represents an array for which its elements are values of some external type. In our case, we will encode matrix values as a `double`, which is a float value encoded on 64 bits. The array has to be accessed from other objects:

```Smalltalk
MMatrix>>array
	"The array containing matrix' values"
	^ array
```

Foreign objects, living within the Pharo memory space, need to be accessible from our external library. A handle represents the memory address that is used by the C library. The class `FFIExternalArray` offers the method `getHandle` to access the memory location:

```Smalltalk
MMatrix>>getHandle
	"Return the handle of the foreign object. 
	This allows the array to be accessed from C"
	^ array getHandle
```

A handy method useful in the test is `asArray`. We will use it when verifying that a matrix is properly created:

```Smalltalk
MMatrix>>asArray
	"Return a linear array of the matrix values"
	^ array asArray
```

In some situations, a handle has to be provided when a matrix is created. The following method address this:

```Smalltalk
MMatrix class>>newHandle: aHandle rows: numRows columns: numColumns
	"Create a matrix with a provided content. Useful when creating a matrix after an FFI operation"
	^ self basicNew
		initializeHandle: aHandle rows: numRows columns: numColumns;
		yourself
```

The method `initializeHandle:rows:columns:` initializes a matrix with a handle and a particular shape:

```Smalltalk
MMatrix>>initializeHandle: aHandle rows: numRows columns: numColumns
	"Initialize the matrix"
	self initialize.
	nbRows := numRows.
	nbColumns := numColumns.
	array := self newArrayFromHandle: aHandle
```

The following factory method creates an external array using a given handle:

```Smalltalk
MMatrix>>newArrayFromHandle: aHandle
	"Create an external array using a handle"
	^ FFIExternalArray 
		fromHandle: aHandle 
		type: 'double' 
		size: nbColumns * nbRows
```

We need a few utility methods to access the shape of the matrix:

```Smalltalk
MMatrix>>nbRows
	"Number of rows defined in the matrix"
	^ nbRows
```

and 

```Smalltalk
MMatrix>>nbColumns
	"Number of columns defined in the matrix"
	^ nbColumns
```

The number of values of the matrix is accessed using `size`:

```Smalltalk
MMatrix>>size
	"The number of values contained in the matrix"
	^ nbColumns * nbRows 
```

A matrix may be _filled_ with a linear set of values:

```Smalltalk
MMatrix>>fromContents: content
	"Initialize the matrix with a linear content"
	self assert: [ content size = (nbColumns * nbRows) ] description: 'size mismatch'.
	content doWithIndex: [ :v :i | array at: i put: v ]
```

These methods will be properly tested in the following subsections.

## Creating the unit test

We can now write a unit test. The class `MMatrixTest` will contains all out tests about `MMatrix`. Consider the class:

```Smalltalk
TestCase subclass: #MMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Matrix'
```

As a first test, we can very the proper behavior of the creation method, defined on `MMatrix`:

```Smalltalk
MMatrixTest>>testCreation
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	self assert: m asArray equals: #(1.0 2.0 3.0 4.0)
```

In the remaining of the chapter we will expand the `MMatrixTest` class.

## Accessing and Modifying the Content of a Matrix

Being able to easily update the matrix content is the first initial step we should consider.
The content of a matrix may be accessed using the `at:` message. This method takes as argument a point:

```Smalltalk
MMatrix>>at: aPoint
	"Access an element of the matrix"
	^ array at: ((aPoint x - 1) * nbColumns + (aPoint y - 1)) + 1
```

We can test the `at:` method:

```Smalltalk
MMatrixTest>>testAt
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	self assert: (m at: 1 @ 1) equals: 1.
	self assert: (m at: 1 @ 2) equals: 2.
	self assert: (m at: 2 @ 1) equals: 3.
	self assert: (m at: 2 @ 2) equals: 4.
```

Similarly, we need to provide a way to modify the content of a matrix. The method `at:put:` inserts a value at a given position:


```Smalltalk
MMatrix>>at: aPoint put: aNumber
	"Modify an element of the matrix"
	array at: ((aPoint x - 1) * nbColumns + (aPoint y - 1)) + 1 put: aNumber asFloat
```
To ease the testing, we add a convenient conversion method:

```Smalltalk
MMatrix>>asStructuredArray
	"Return a structured array that describe the matrix"
	^ (1 to: nbRows) collect: [ :i | self atRow: i ] as: Array
```
The method `atRow:` returns the horizontal values for a given index.

```Smalltalk
MMatrix>>atRow: rowNumber
	"Return a particular row"
	(rowNumber between: 1 and: rowNumber)
		ifFalse: [ self error: 'index out of range' ].
	^ (1 to: nbColumns) collect: [ :x | self at: rowNumber @ x ] 
```

A simple test illustrates the use of `at:put:`:

```Smalltalk
MMatrixTest>>testAtPut
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	m at: 2 @ 1 put: 10.0.
	self assert: (m at: 2 @ 1) equals: 10.0.
	self assert: m asStructuredArray equals: #(#(1 2) #(10 4))
```

Note that we refer to an element using a coordinate `row @ column`. This way to access a matrix element is close to the mathematical notation traditionally used in linear algebra.

When we will do the prediction in a network, we will need to obtain the maximum value of a matrix. We can simply define:

```Smalltalk
MMatrix>>max
	"Return the maximum value of the matrix"
	^ self asArray max
```

The corresponding test is:

```Smalltalk
MMatrixTest>>testMax
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	self assert: m max equals: 4.
```

We have laid out the necessary infrastructure to define some operations. The following sections will covers the operations we will employ in our neural network.

## Summing matrices

Two matrices may be summed up. The operation assumes that the two matrices have exactly the same dimensions. We can define the sum with the `+` method. This method accepts another matrix of the same size than the receiver, or a vertical vector (_i.e.,_ matrix with only one column):

```Smalltalk
MMatrix>>+ matrixOrVector
	"Add either a matrix or a vector to the receiver. 
	The argument could either be a matrix of the same size or a vector
	A new matrix is returned as result"
	| m |
	((nbRows = matrixOrVector nbRows) and: [ nbColumns = matrixOrVector nbColumns ])
		ifTrue: [ ^ self add: matrixOrVector ].
	matrixOrVector nbColumns ~= 1 ifTrue: [ self error: 'not a n * 1 vector' ].
	m := matrixOrVector stretchToColumns: nbColumns.
	^ self + m
```

The addition involves several steps due to the complexity of the operation. We define the `add:` method:

```Smalltalk
MMatrix>>add: aMatrix
	"Add two matrices, the receiver and the argument, and produces a new matrix"
	| result resultArray |
	nbColumns = aMatrix nbColumns ifFalse: [self error: 'dimensions do not conform'].
	nbRows = aMatrix nbRows ifFalse: [self error: 'dimensions do not conform'].
	
	resultArray := ByteArray new: (nbRows * aMatrix nbColumns * 8).
	self assert: [ nbRows * nbColumns = array size ].
	self assert: [ aMatrix nbRows * aMatrix nbColumns = aMatrix size ].
	self assert: [ nbRows * aMatrix nbColumns * 8 = resultArray size ].
	self 
		add: self getHandle with: nbRows with: nbColumns with: aMatrix getHandle
		in: resultArray.
	result := MMatrix newHandle: resultArray rows: nbRows columns: nbColumns.
	^ result
```

The method `add:` creates a new matrix, and invokes the `add` function from our C library:

```Smalltalk
MMatrix>>add: m1 with: nb_rows with: nb_columns with: m2 in: res
	^ self 
		ffiCall: #(void add(double *m1, int nb_rows, int nb_columns, 
              	double *m2, 
              	double *res)) 
		module: 'matrix.dylib'
```

We can test summing two matrices:

```Smalltalk
MMatrixTest>>testAddition1
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	m2 := MMatrix newFromArrays: #(#(4 5 6) #(1 2 3)).
	self assert: (m1 + m2) asStructuredArray equals: #(#(5.0 7.0 9.0) #(5.0 7.0 9.0))
```

We can also try summing a matrix with itself:

```Smalltalk
MMatrixTest>>testAddition2
	| m |
	m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	self assert: (m + m) asStructuredArray equals: #(#(2.0 4.0 6.0) #(8.0 10.0 12.0))
```

Elements of a matrix may be horizontally summed up. As we will see in the next chapter, this operation is important when we will implement the backpropagation algorithm. Consider the method `sumHorizontal`:

```Smalltalk
MMatrix>>sumHorizontal
	"Horizontal summing"
	| result sum |
	result := MMatrix newRows: nbRows columns: 1.
	1 to: nbRows do: [ :y |
		sum := 0.
		1 to: nbColumns do: [ :x |
			sum := sum + (self at: y @ x) ].
		result at: y @ 1 put: sum ].
	^ result
```

An example of `sumHorizontal` is provided in the following test method:

```Smalltalk
MMatrixTest>>testSumHorizontal
	| m expectedResult |
	m := MMatrix newFromArrays: #(#(1.0 2.0) #(3.0 4.0) #(5.0 6.0)).
	expectedResult := MMatrix newFromArrays: #(#(3.0) #(7.0) #(11.0)).
	self assert: m sumHorizontal asStructuredArray equals: expectedResult asStructuredArray
```

## Printing a matrix

Being able to print a matrix is essential to see how the matrix is made of. The method `printOn:` returns a textual representation of the object that received the corresponding message. We will therefore redefine it in our class `MMatrix`:

```Smalltalk
MMatrix>>printOn: aStream
	"Print the matrix in the stream, with 4 decimal for each value"
	self printOn: aStream round: 4
```

We will handle matrices with 64 bit float values. To make the printing effective, we need to limit the number of decimals:

```Smalltalk
MMatrix>>printOn: aStream round: nbDecimals
	"Print the receiver matrix into a stream. All numerical value are truncated to a fixed number of decimals"
	aStream nextPutAll: '('.
	(1 to: nbRows) 
		do: [ :r | 
				(self atRow: r) 
					do: [ :each | aStream nextPutAll: (each round: nbDecimals) printString ] 
					separatedBy: [ aStream space ]]
		separatedBy: [ aStream cr ].
	aStream nextPutAll: ' )'.
```

We can now test our code in a playground. Consider the following code snippet: 

```Smalltalk
m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
m + m
```

Printing the code above should produce:
```
(2.0 4.0 6.0
8.0 10.0 12.0)
```

## Vector

A vector is a matrix with only one column. For example, the expression `MMatrix newFromArrays: #(#(1) #(2) #(3))` creates a vector of three elements. We provide a utility method to define vector:

```Smalltalk
MMatrix class>>newFromVector: array
	"Create a Nx1 matrix from an array of numbers (N = array size)"
	^ self basicNew
		initializeRows: array size columns: 1;
		fromContents: array;
		yourself
```

The method `newFromVector:` expects a flat Pharo array. Here is an example:

```Smalltalk
MMatrixTest>>testVectorCreation
	| v |
	v := MMatrix newFromVector: #(1 2 3).
	self assert: v nbColumns equals: 1.
	self assert: v nbRows equals: 3.
	self assert: v asStructuredArray equals: #(#(1) #(2) #(3))
```

The backpropagation algorithm requires to stretch a vector into a matrix. It converts a vector into a matrix by juxtaposing several times the vector. We define the following method:

```Smalltalk
MMatrix>>stretchToColumns: nbOfColumns
	"Stretch a vertical vector in a column."
	| content result |
	content := OrderedCollection new.
	1 to: nbRows do: [ :row |
		1 to: nbOfColumns do: [ :columns |
			content add: (self at: row @ 1) ] ].
	result := MMatrix newRows: nbRows columns: nbOfColumns.
	result fromContents: content.
	^ result
```

Printing the expression `(MMatrix newFromVector: #(1 2 3 4)) stretchToColumns: 5` results in: 

```
(1.0 1.0 1.0 1.0 1.0
2.0 2.0 2.0 2.0 2.0
3.0 3.0 3.0 3.0 3.0
4.0 4.0 4.0 4.0 4.0 )
```

A test can be defined as:

```Smalltalk
MMatrixTest>>testStretching
	| m |
	m := (MMatrix newFromVector: #(1 2 3 4)) stretchToColumns: 5. 
	self assert: m nbRows equals: 4.
	self assert: m nbColumns equals: 5.
	self assert: (m atRow: 1) equals: #(1 1 1 1 1).
	self assert: (m atRow: 3) equals: #(3 3 3 3 3).
```

## Factors

Being able to transform a matrix and multiply matrices is essential in several parts of the backpropagation algorithm. We will first define a generic way to transform a matrix:

```Smalltalk
MMatrix>>collect: aOneArgBlock
	"Return a new matrix, for which each matrix element is transformed using the provided block"
	| result |
	result := MMatrix newRows: nbRows columns: nbColumns.
	1 to: nbRows do: [ :y |
		1 to: nbColumns do: [ :x |
			result at: y @ x put: (aOneArgBlock value: (self at: y @ x))
		] ].
	^ result
```

A simple test that add a value to each matrix element:

```Smalltalk
MMatrixTest>>testCollect
	| m expectedMatrix |
	m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)).
	expectedMatrix := MMatrix newFromArrays: #(#(2 3 4) #(5 6 7)).
	self assert: (m collect: [ :v | v + 1]) asStructuredArray equals: expectedMatrix asStructuredArray
```

Elements of a matrix may be multiplied by a numerical factor. For that purpose, we define the method `*`:


```Smalltalk
MMatrix>>* aFactor
	"Multiply each element of the matrix by a factor"
	^ self collect: [ :v | v * aFactor ]
```

We can test this method when applied to a vector:

```Smalltalk
MMatrixTest>>testMultiplicationOnVector
	| x |
	x := MMatrix newFromVector: #(1 2 3 4).
	self assert: (x * 5) asStructuredArray equals: #(#(5.0) #(10.0) #(15.0) #(20.0))
```

Similarly, we can test the multiplication on a matrix:

```Smalltalk
MMatrixTest>>testMultiplicationOnMatrix
	| x |
	x := MMatrix newFromArrays: #(#(1 2 3 4) #(10 20 30 40)).
	self assert: (x * 5) asStructuredArray 
			equals: #(#(5.0 10.0 15.0 20.0) #(50.0 100.0 150.0 200.0))
```

Another relevant operation is to multiply two matrices element-wise:

```Smalltalk
MMatrix>>multiplyPerElement: mat
	"Multiply two matrices element-wise"
	| r |
	self assert: [ nbRows = mat nbRows ].
	self assert: [ nbColumns = mat nbColumns ].
	r := MMatrix newRows: nbRows columns: nbColumns.
	r fromContents: (self asArray with: mat array asArray collect: [ :a :b | a * b ]).
	^ r
```

The method could be tested as follows:

```Smalltalk
MMatrixTest>>testMultiplicationPerElement
	| v1 v2 expectedVector |
	v1 := MMatrix newFromVector: #(1 2 3).
	v2 := MMatrix newFromVector: #(10 20 30).
	expectedVector := MMatrix newFromVector: #(10 40 90).
	self assert: (v1 multiplyPerElement: v2) asArray 
			equals: expectedVector asArray
```

## Dividing a matrix by a factor

In the same fashion than seen in the previous section, we can divide a matrix by a factor:

```Smalltalk
MMatrix>>/ value
	"Divide each element of the matrix by a value"
	^ self collect: [ :v | v / value ]
```

This method can be tested using:

```Smalltalk
MMatrixTest>>testDivision
	| m |
	m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	self assert: (m / 2) asStructuredArray equals: #(#(0.5 1.0 1.5) #(2.0 2.5 3.0))
```

## Matrix product

We defined the matrix product using two methods `+*` and `dot:`. The first being a shortcut to the latter:

```Smalltalk
MMatrix>>+* anotherMatrix
	"Shortcut for the dot operator between matrices"
	^ self dot: anotherMatrix 
```

The method `dot:` is defined as:

```Smalltalk
MMatrix>>dot: anotherMatrix
	"Compute the dot product between the receiving matrix and the argument"
	| result resultArray |
	nbColumns = anotherMatrix nbRows ifFalse: [self error: 'dimensions do not conform'].
	self assert: [ nbRows * nbColumns = array size ].
	self assert: [ anotherMatrix nbRows * anotherMatrix nbColumns = anotherMatrix  size ].
	resultArray := ByteArray new: (nbRows * anotherMatrix nbColumns * 8).
	
	self 
		dot: self getHandle with: nbRows with: nbColumns 
		with: anotherMatrix getHandle
		with: anotherMatrix nbRows with: anotherMatrix nbColumns in: resultArray.

	result := MMatrix 
		newHandle: resultArray 
		rows: nbRows 
		columns: anotherMatrix nbColumns.
	^ result
```

The connection between the Pharo code and C library is defined in the following method:

```Smalltalk
MMatrix>>dot: array1 with: m1_nb_rows with: m1_nb_columns with: array2 with: m2_nb_rows with: m2_nb_columns in: res
	"Invoke the C library to perform the dot operator"
	^ self 
		ffiCall: #(void dot(
			void *array1, int m1_nb_rows, int m1_nb_columns, 
         	void *array2, int m2_nb_rows, int m2_nb_columns, void *res) ) 
		module: 'matrix.dylib'

```

We can test our code using the following test method:

```Smalltalk
MMatrixTest>>testMatrixProduct
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	m2 := MMatrix newFromArrays: #(#(1 2) #(3 4) #(5 6) #(7 8)).
	self assert: (m1 +* m2) asStructuredArray equals: #(#(50.0 60.0) #(114.0 140.0))
```

## Matrix substraction

Substracting matrices is another relevant operation in machine learning in general. We define the following shortcut:

```Smalltalk
MMatrix>>- anotherMatrix
	"Substract a matrix from the receiver matrix"
	^ self sub: anotherMatrix
```

This shortcut calls the `sub:` method:

```Smalltalk
MMatrix>>sub: anotherMatrix
	| result resultArray |
	nbColumns = anotherMatrix nbColumns ifFalse: [self error: 'dimensions do not conform'].
	nbRows = anotherMatrix nbRows ifFalse: [self error: 'dimensions do not conform'].
	
	resultArray := ByteArray new: (nbRows * anotherMatrix nbColumns * 8).
	
	self assert: [ nbRows * nbColumns = array size ].
	self assert: [ anotherMatrix nbRows * anotherMatrix nbColumns = anotherMatrix  size ].
	self assert: [ nbRows * anotherMatrix nbColumns * 8 = resultArray size ].
	
	self 
		sub: self getHandle with: nbRows with: nbColumns with: anotherMatrix getHandle
		in: resultArray.
	result := MMatrix newHandle: resultArray rows: nbRows columns: nbColumns.
	^ result
```

The use of our C library is made using the following method:

```Smalltalk
MMatrix>>sub: m1 with: nb_rows with: nb_columns with: m2 in: res
	^ self 
		ffiCall: #(void sub(double *m1, int nb_rows, int nb_columns, 
              	double *m2, double *res)) 
		module: 'matrix.dylib'
```

A simple test illustrates the behavior of matrix substraction:

```Smalltalk
MMatrixTest>>testSub
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	m2 := MMatrix newFromArrays: #(#(4 2 1 3) #(7 6 8 5)).
	self assert: (m1 - m2) asStructuredArray equals: #(#(-3 0 2 1) #(-2 0 -1 3))
```

## Filling the matrix with random numbers

The initial state of a neural network is mostly random. We therefore need a way to randomly initialize a matrix. Consider the method:

```Smalltalk
MMatrix>>random
	"Fill the matrix with random numbers"
	^ self random: Random new
```

It could be convenient to provide a random generator for the initialization:

```Smalltalk
MMatrix>>random: randomNumberGenerator
	"Fill the matrix with random numbers. Takes a random number generator as argument"
	self fromContents: ((1 to: nbRows * nbColumns) collect: [ :vv | randomNumberGenerator next ])

```

Executing the expression `(MMatrix newRows: 4 columns: 5) random` illustrates its usage:

```
(0.2073 0.7154 0.3008 0.06 0.0865
0.3493 0.6396 0.7285 0.4873 0.1947
0.7951 0.3034 0.6066 0.8358 0.1445
0.5454 0.2504 0.2012 0.9086 0.5719 )
```


## Summing the matrix values

Values contained in a matrix may be summed up. This will be useful to evaluate the cost function when training a neural network:

```Smalltalk
MMatrix>>sum
	"Return the sum of the matrix values"
	| sum |
	sum := 0.
	1 to: nbRows do: [ :y |
		1 to: nbColumns do: [ :x |
			sum := sum + (self at: y @ x)
		] 
	].
	^ sum
```

The use of `sum` is illustrates in the test:

```Smalltalk
MMatrixTest>>testSum
	| m |
	m := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	self assert: m sum equals: (1 to: 8) sum
```

## Transpose

The transpose of a matrix is an operation that consists in flipping a matrix along its diagonal. We can define the operation as follows:

```Smalltalk
MMatrix>>transposed
	"Transpose the matrix"
	| result |
	result := MMatrix newRows: nbColumns columns: nbRows.
	1 to: nbRows do: [ :row |
		1 to: nbColumns do: [ :column |
			result at: column @ row put: (self at: row @ column)
		]
	].
	^ result
```

The following test illustrates the behavior of the `transposed` method:

```Smalltalk
MMatrixTest>>testTransposedOnMatrix
	| m expectedResult |
	m := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	expectedResult := MMatrix newFromArrays: #(#(1 5) #(2 6) #(3 7) #(4 8)).
	self assert: m transposed asStructuredArray equals: expectedResult asStructuredArray
```

Transposing a vector produce a matrix of one row, as illustrates with the following test method:


```Smalltalk
MMatrixTest>>testTransposedOnVector
	| m |
	m := MMatrix newFromVector: #(1 2 3).
	self assert: m transposed asStructuredArray equals: #(#(1 2 3))
```


## Example

We can illustrate the use of matrices in a simple backpropagation implementation. The following script creates two random set of values and train a neural network to maps the input values to the output values. It illustrates the "essence" of forward and backward propagation:

```Smalltalk
n := 8.         "Number of examples"
din := 10.      "Number of input values"
h := 20.        "Size of the hidden layer"
dout := 5.      "Number of output values"

r := Random seed: 42.
x := (MMatrix newRows: n columns: din) random: r.
y := (MMatrix newRows: n columns: dout) random: r.
w1 := (MMatrix newRows: din columns: h) random: r.
w2 := (MMatrix newRows: h columns: dout) random: r.

learningRate := 1e-6.
losses := OrderedCollection new.
1500 timesRepeat: [ 
    hh := x +* w1.
    hrelu := hh collect: [ :v | v max: 0 ].
    ypred := hrelu +* w2.
    
    "Compute and print loss"
    loss := ((ypred - y) collect: [:vv | vv * vv ]) sum.
    losses add: loss.
    
    "Backprop to compute gradients of w2 and w2 with respect to loss"
    gradYPred := (ypred - y) * 2.0.
    gradW2 := hrelu transposed +* gradYPred.
    gradHRelu := gradYPred +* w2 transposed.
    gradH := gradHRelu collect: [ :v | v max: 0 ].
    gradW1 := x transposed +* gradH.
    
    w1 := w1 - (gradW1 * learningRate).
    w2 := w2 - (gradW2 * learningRate) 
].

g := RTGrapher new.
d := RTData new.
d noDot; connectColor: Color blue.
d points: losses.
d y: #yourself.
g add: d.
g axisX title: 'Epoch'.
g axisY title: 'Error'.
g
```

![A simple implementation of backpropagation.](07-MatrixLibrary/figures/simpleBackpropagation.png){#fig:simpleBackpropagation}

The last part of the script uses `RTGrapher` to show a the evolution of the loss value along epochs (Figure @fig:simpleBackpropagation).

## What have we seen?

The chapter covers the following topics:

- _Definition of a minimal C library._ Neural networks, and deep learning in general, employ matrices to performs its computation.
- _Definition of the class `MMatrix`._ This class models the mathematical notion of matrix. Note that we designed our class to offer relevant operations for neural networks. Note that it is by no means a definitive generic matrix implementation.

This was a long dense chapter. However, the matrix library we have seen will greatly simply the revised version of our neural network in the next chapter. 

Modern libraries to build neural networks employ matrices to carry out the numerical computation. However, the GPU is traditionally used instead of the CPU, as we are doing here. We could have used CUDA or OpenCL to perform the matrix operations on the GPU. However, it would have considerably lengthen the amount of code. This is the reason why we simply restrict ourselves to computation carry out by the CPU.

The next chapter will rewrite our neural network implementation to use the matrix.

