
# A Matrix Library

In the previous chapters we presented an implementation of a neural network made of layers and neurons (_i.e.,_ instances of `NeuronLayer` and `Neuron`). Although instructive, our implementation does not reflects classical ways of implementing a neural network. A layer can be expressed as a matrix of weights and a vector of biases. This is actually how most libraries to build neural network (_e.g.,_ TensorFlow and PyTorch) actually operate.

This chapter lays out a small library to build and manipulate matrices. This chapter is important for the subsequent chapter which is about how networks can be implemented using matrices. Matrix are particular data structure for which operations cannot efficiently be implemented in Pharo. We will write such costly operations in C but make them accessible within Pharo.

In addition to defining a matrix library, this chapters highlights one particular aspect of Pharo, which is the use of Foreign Function Interface (FFI). This is a relevant mechanism whenever one wishes to make Pharo use external libraries written using the C or C++ languages. For example, TensorFlow is written in C++, which may be accessed from Pharo using the very same technique presented in this chapter.

This chapter is long and contains many inter-dependent methods. The chapter needs to be fully implemented before being functional.

## Matrix operations in C

Pharo does not provide built-in features to manipulate matrices. Although we could implement them in Pharo, it would suffer from very poor performances. Instead, we will code a small library in C to support the elementary C operations. 

Create a file named `matrix.c` with the following C code:

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

We will not go into details about this C file. It simply applies some basic matrix operations. 

The library has to be compiled. We can use the gcc standard compiler for this:

```bash
gcc -dynamiclib -o matrix.dylib matrix.c

```

Our matrix file is compiled as a dynamic library, loadable within Pharo. The compilation produces the `matrix.dylib` file. Note that the `matrix.dylib` file should be located next to the `.image` file.

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
	values of the matrix.
	Example of matrix creations:
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

Foreign objects, living within the Pharo memory space, need to be accessible from our external library. A handle represents the memory address that is used by the C library. The class `FFIExternalArray` offers the method `getHandle` for accessing the memory location:

```Smalltalk
MMatrix>>getHandle
	"Return the handle of the foreign object. 
	This allows the array to be accessed from C"
	^ array getHandle
```

An handy method useful in the testing is `asArray`. We will use it when verifying that matrix are properly created:

```Smalltalk
MMatrix>>asArray
	"Return a linear array of the matrix values"
	^ array asArray
```

In some situations, an handle has to be provided when a matrix is created. The following method address this:

```Smalltalk
MMatrix class>>newHandle: aHandle rows: numRows columns: numColumns
	"Create a matrix with a provided content. Useful when creating a matrix after an FFI operation"
	^ self basicNew
		initializeHandle: aHandle rows: numRows columns: numColumns;
		yourself
```

The method `initializeHandle:rows:columns:` initializes a matrix with an handle and a particular shape:

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
	"Create an external array using an handle"
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

## Unit test

We can now write a unit test. The class `MMatrixTest` will contains all out tests of `MMatrix`:

```Smalltalk
TestCase subclass: #MMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Matrix'
```

We can tests some creation and initialization methods.

```Smalltalk
MMatrixTest>>testCreation
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	self assert: m asArray equals: #(1.0 2.0 3.0 4.0)
```

## Accessing and Modifying the Content of a Matrix

The content of a matrix may be accessed using the `at:` message. It takes as argument a point.

```Smalltalk
MMatrix>>at: aPoint
	"Access an element of the matrix"
	^ array at: ((aPoint y - 1) * nbColumns + (aPoint x - 1)) + 1
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
	array at: ((aPoint y - 1) * nbColumns + (aPoint x - 1)) + 1 put: aNumber asFloat
```

A simple test illustrates its usage:

```Smalltalk
MMatrixTest>>testAtPut
	| m |
	m := MMatrix newFromArrays: #(#(1 2) #(3 4)).
	m at: 2 @ 1 put: 10.0.
	self assert: (m at: 2 @ 1) equals: 10.0.
```

We have laid out the necessary infrastructure to define some operations. The following sections will covers the operations we will employ in our neural network.

## Summing matrices

Two matrices may be summed up. The operation assumes that the two matrices have exactly the same dimensions. We can define the sum with the `+` method. This method accepts another matrix of the same size than the receiver, or a vertical vector (_i.e.,_ matrix with only one column):

```Smalltalk
MMatrix>>+ matrixOrVector
	"Add either a matrix or a vector to the receiver. The argument could either be a matrix of the same size than me, or a vector
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

Elements of a matrix may be horizontally summed up. As we will see in the next chapter, this operation is important when we will implement the backpropagation algorithm. Consider the method `sumHorizontal`:

```Smalltalk
MMatrix>>sumHorizontal
	"Vertical summing"
	| result sum |
	result := MMatrix newRows: nbRows columns: 1.
	1 to: nbRows do: [ :y |
		sum := 0.
		1 to: nbColumns do: [ :x |
			sum := sum + (self at: x @ y)
		].
		result at: 1 @ y put: sum
	].
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

The method `atRow:` returns 

```Smalltalk
MMatrix>>atRow: rowNumber
	"Return a particular row"
	(rowNumber between: 1 and: rowNumber)
		ifFalse: [ self error: 'index out of range' ].
	^ (1 to: nbColumns) collect: [ :x | self at: x @ rowNumber ] 
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

To ease the testing, we add a convenient conversion method:

```Smalltalk
MMatrix>>asStructuredArray
	"Return a structured array that describe the matrix"
	^ (1 to: nbRows) collect: [ :i | self atRow: i ] as: Array
```



```Smalltalk
MMatrixTest>>testAddition
	| m |
	m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	self assert: (m + m) asStructuredArray equals: #(#(2.0 4.0 6.0) #(8.0 10.0 12.0))
```
```Smalltalk
MMatrixTest>>testAddition2
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	m2 := MMatrix newFromArrays: #(#(4 5 6) #(1 2 3)).
	self assert: (m1 + m2) asStructuredArray equals: #(#(5.0 7.0 9.0) #(5.0 7.0 9.0))
```

## Vector

Vector may be created using:

```Smalltalk
MMatrix class>>newFromVector: array
	"Create a Nx1 matrix from an array of numbers"
	^ self basicNew
		initializeRows: array size columns: 1;
		fromContents: array;
		yourself
```

```Smalltalk
MMatrixTest>>testVectorCreation
	| v |
	v := MMatrix newFromVector: #(1 2 3).
	self assert: v nbColumns equals: 1.
	self assert: v nbRows equals: 3.
```

During the back-propagation algorithm, a column vector has to be stretched into a matrix. we therefore define the method:

```Smalltalk
MMatrix>>stretchToColumns: nbOfColumns
	"Stretch a vertical vector in a column."
	| content result |
	content := OrderedCollection new.
	1 to: nbRows do: [ :row |
		1 to: nbOfColumns do: [ :columns |
			content add: (self at: 1 @ row)
		]
	].
	result := MMatrix newRows: nbRows columns: nbOfColumns.
	result fromContents: content.
	^ result
```

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

Elements of a matrix may be multiplied by a factor. 

```Smalltalk
MMatrix>>collect: aOneArgBlock
	"Return a new matrix, for which each matrix element is transformed."
	| result |
	result := MMatrix newRows: nbRows columns: nbColumns.
	1 to: nbRows do: [ :y |
		1 to: nbColumns do: [ :x |
			result at: x @ y put: (aOneArgBlock value: (self at: x @ y))
		] 
	].
	^ result
```
```Smalltalk
MMatrix>>* aFactor
	"Multiply each element of the matrix by a factor"
	^ self collect: [ :v | v * aFactor ]
```

```Smalltalk
MMatrix>>multiplyPerElement: mat
	"
	| v1 v2 |
	v1 := MMatrix newFromVector: #(1 2 3).
	v2 := MMatrix newFromVector: #(10 20 30).
	v1 multiplyPerElement: v2	
	"
	| r |
	self assert: [ nbRows = mat nbRows ].
	self assert: [ nbColumns = mat nbColumns ].
	r := MMatrix newRows: nbRows columns: nbColumns.
	r fromContents: (self asArray with: mat array asArray collect: [ :a :b | a * b ]).
	^ r
```

```Smalltalk
MMatrixTest>>testMultiplication
	| x |
	x := MMatrix newFromVector: #(1 2 3 4).
	self assert: (x * 5) asStructuredArray equals: #(#(5.0) #(10.0) #(15.0) #(20.0))
```
```Smalltalk
MMatrixTest>>testMultiplication2
	| x |
	x := MMatrix newFromArrays: #(#(1 2 3 4) #(10 20 30 40)).
	self assert: (x * 5) asStructuredArray 
			equals: #(#(5.0 10.0 15.0 20.0) #(50.0 100.0 150.0 200.0))
```

## Matrix product

```Smalltalk
MMatrix>>+* anotherMatrix
	"Shortcut for the dot operator between matrices"
	^ self dot: anotherMatrix 
```

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

The use of the C library is done using the following method:
```Smalltalk
MMatrix>>dot: array1 with: m1_nb_rows with: m1_nb_columns with: array2 with: m2_nb_rows with: m2_nb_columns in: res

	^ self 
		ffiCall: #(void dot(
			void *array1, 
			int m1_nb_rows, 
			int m1_nb_columns, 
         void *array2, 
			int m2_nb_rows, 
			int m2_nb_columns, 
			void *res) ) 
		module: 'matrix.dylib'

```

```Smalltalk
MMatrixTest>>testDotProduct
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	m2 := MMatrix newFromArrays: #(#(1 2) #(3 4) #(5 6) #(7 8)).
	self assert: (m1 +* m2) asStructuredArray equals: #(#(50.0 60.0) #(114.0 140.0))
```

## Matrix substraction

```Smalltalk
MMatrix>>- anotherMatrix
	"Substract a matrix from the receiver matrix"
	^ self sub: anotherMatrix
```

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
		sub: self getHandle 
		with: nbRows 
		with: nbColumns 
		with: anotherMatrix getHandle
		in: resultArray.
	
	result := MMatrix 
		newHandle: resultArray 
		rows: nbRows 
		columns: nbColumns.
	
	^ result
```
```Smalltalk
MMatrix>>sub: m1 with: nb_rows with: nb_columns with: m2 in: res
	^ self 
		ffiCall: #(void sub(double *m1, int nb_rows, int nb_columns, 
              	double *m2, 
              	double *res)) 
		module: 'matrix.dylib'
```

## Divide a matrix by a factor

```Smalltalk
MMatrix>>/ value
	"Divide each element of the matrix by a value"
	^ self collect: [ :v | v / value ]
```


```Smalltalk
MMatrixTest>>testDivision
	| m |
	m := MMatrix newFromArrays: #(#(1 2 3) #(4 5 6)). 
	self assert: (m / 2) asStructuredArray equals: #(#(0.5 1.0 1.5) #(2.0 2.5 3.0))
```
## Filling the matrix with random numbers

```Smalltalk
MMatrix>>random: randomNumberGenerator
	"Fill the matrix with random numbers. Takes a random number generator as argument"
	self fromContents: ((1 to: nbRows * nbColumns) collect: [ :vv | randomNumberGenerator next ])

```

```Smalltalk
MMatrix>>random
	"Fill the matrix with random numbers"
	^ self random: Random new
```

## Summing 
```Smalltalk
MMatrix>>sum
	"Return the sum of the matrix values"
	| sum |
	sum := 0.
	1 to: nbRows do: [ :y |
		1 to: nbColumns do: [ :x |
			sum := sum + (self at: x @ y)
		] 
	].
	^ sum
```

## Transpose

```Smalltalk
MMatrix>>transposed
	"Transpose the matrix"
	| result |
	result := MMatrix newRows: nbColumns columns: nbRows.
	1 to: nbRows do: [ :row |
		1 to: nbColumns do: [ :column |
			result at: row @ column put: (self at: column @ row)
		]
	].
	^ result
```
