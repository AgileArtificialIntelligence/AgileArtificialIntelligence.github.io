
# A Matrix Library

We have presented an implementation of a neural network that is is made of instances of `NeuronLayer` and `Neuron`. Although instructive, our implementation does not reflects traditional ways of implementing a network.  

Neural networks are implemented using matrices. This chapter lays out a small library to build and manipulate matrices. Costly matrix operations are implemented in C.

In addition to defining a matrix library, this chapters highlights one particular aspect of Pharo, which is the use of Foreign Function Interface (FFI). This is a relevant whenever one wish to make Pharo use external libraries written using the C or C++ languages. For example, TensorFlow is written in C++, which may be accessed from Pharo using the very same technique presented in this chapter.

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
- `dot` to perform the multiplication of matrices
- `sub` to subtract one matrix from another
- `add` to sum two matrices.

The C code does not allocate memory for its own use. This will have to be provided from Pharo. The library has to be compiled. We can use the gcc standard compiler for this:

```bash
gcc -dynamiclib -o matrix.dylib matrix.c

```

Our matrix file is compiled as a dynamic library, loadable within Pharo. The compilation produces the `matrix.dylib` file. 


## The Matrix class

We can now write the Pharo class `MMatrix` which will use our C-library. Note that Pharo 7 contains a deprecated class `Matrix`, which is why our class is prefixed with an additional `M`.

In a new package called `Matrix`, we define the class:

```Smalltalk
Object subclass: #MMatrix
	instanceVariableNames: 'nbRows nbColumns array'
	classVariableNames: ''
	package: 'Matrix'
```

The two first variables describe the shape of the matrix while the variable `array` will refer to an array containing the actual values of the matrix. This array will have to be accessible from Pharo.

On the class side of the class `MMatrix` we define a number of useful methods to create matrices. You need to switch the class browser to the class mode to define class methods. The method `newFromArrays:` creates a matrix from a collection of arrays:

```Smalltalk
MMatrix class>>newFromArrays: arrays
	"
	MMatrix newFromArrays: #(#(1 2 3) #(4 5 6))
	MMatrix newFromArrays: #(#(1 2 3))
	"
	^ self basicNew
		initializeRows: arrays size columns: arrays first size;
		fromContents: (arrays flatCollect: #yourself);
		yourself
```


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

```Smalltalk
MMatrix>>array
	^ array
```

A matrix may be _filled_ with a linear set of values:

```Smalltalk
MMatrix>>fromContents: content
	"Initialize the matrix with a linear content"
	self assert: [ content size = (nbColumns * nbRows) ] description: 'size mismatch'.
	content doWithIndex: [ :v :i | array at: i put: v ]
```


```Smalltalk
MMatrix class>>newHandle: aHandle rows: numRows columns: numColumns
	"Create a matrix with a provided content. Useful when creating a matrix after an FFI operation"
	^ self basicNew
		initializeHandle: aHandle rows: numRows columns: numColumns;
		yourself
```

```Smalltalk
MMatrix>>initializeHandle: aHandle rows: numRows columns: numColumns
	self initialize.
	nbRows := numRows.
	nbColumns := numColumns.
	array := self newArrayFromHandle: aHandle
```


```Smalltalk
MMatrix>>newArrayFromHandle: aHandle

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

## Summing matrices

We can define the sum with the `+` method. This method accepts another matrix of the same size than the receiver, or a vertical vector (_i.e.,_ matrix with only one column):

```Smalltalk
MMatrix>>+ matrixOrVector
	"Add either a matrix or a vector. The argument could either be a matrix of the same size than me, or a vector
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
Matrix>>add: aMatrix
	"Add two matrices, the receiver and the argument, and produces a new matrix"
	| result resultArray |
	nbColumns = aMatrix nbColumns ifFalse: [self error: 'dimensions do not conform'].
	nbRows = aMatrix nbRows ifFalse: [self error: 'dimensions do not conform'].
	
	resultArray := ByteArray new: (nbRows * aMatrix nbColumns * 8).
	
	self assert: [ nbRows * nbColumns = array size ].
	self assert: [ aMatrix nbRows * aMatrix nbColumns = aMatrix array size ].
	self assert: [ nbRows * aMatrix nbColumns * 8 = resultArray size ].
	
	self 
		add: self array getHandle 
		with: nbRows 
		with: nbColumns 
		with: aMatrix array getHandle
		in: resultArray.
	
	result := MMatrix 
		newHandle: resultArray 
		rows: nbRows 
		columns: nbColumns.
	
	^ result
```	

```Smalltalk
Matrix>>add: m1 with: nb_rows with: nb_columns with: m2 in: res
	^ self 
		ffiCall: #(void add(double *m1, int nb_rows, int nb_columns, 
              	double *m2, 
              	double *res)) 
		module: 'matrix.dylib'
```
## Accessing and Modifying the content of a matrix

```Smalltalk
Matrix>>at: aPoint
	"Access an element of the matrix"
	^ array at: ((aPoint y - 1) * nbColumns + (aPoint x - 1)) + 1
```

```Smalltalk
Matrix>>at: aPoint put: aNumber
	"Modify an element of the matrix"
	array at: ((aPoint y - 1) * nbColumns + (aPoint x - 1)) + 1 put: aNumber asFloat
```






## Printing a matrix

```Smalltalk
Matrix>>atRow: rowNumber
	"Return a particular row"
	(rowNumber between: 1 and: rowNumber)
		ifFalse: [ self error: '1st subscript out of range' ].
	^ (1 to: nbColumns) collect: [ :x | self at: x @ rowNumber ] 
```

```Smalltalk
Matrix>>printOn: aStream
	"Print the matrix in the stream, with 4 decimal for each value"
	self printOn: aStream round: 4
```

```Smalltalk
Matrix>>printOn: aStream round: nbDecimals
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
8.0 10.0 12.0 )
```

We add a conversion method:

```Smalltalk
Matrix>>asStructuredArray
	"Return a structured array that describe the matrix"
	^ (1 to: nbRows) collect: [ :i | self atRow: i ] as: Array
```

We can now write a unit test. 

```Smalltalk
TestCase subclass: #MMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Matrix'
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
Matrix class>>newFromVector: array
	"Create a Nx1 matrix from an array of numbers
	"
	^ self basicNew
		initializeRows: array size columns: 1;
		fromContents: array;
		yourself
```

```Smalltalk
MatrixTest>>testVectorCreation
	| v |
	v := MMatrix newFromVector: #(1 2 3).
	self assert: v nbColumns equals: 1.
	self assert: v nbRows equals: 3.
```

During the back-propagation algorithm, a column vector has to be stretched into a matrix. we therefore define the method:

```Smalltalk
Matrix>>stretchToColumns: nbOfColumns
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
MatrixTest>>testStretching
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
Matrix>>collect: aOneArgBlock
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
Matrix>>* aFactor
	"Multiply each element of the matrix by a factor"
	^ self collect: [ :v | v * aFactor ]
```
```Smalltalk
MatrixTest>>testMultiplication
	| x |
	x := MMatrix newFromVector: #(1 2 3 4).
	self assert: (x * 5) asStructuredArray equals: #(#(5.0) #(10.0) #(15.0) #(20.0))
```
```Smalltalk
MatrixTest>>testMultiplication2
	| x |
	x := MMatrix newFromArrays: #(#(1 2 3 4) #(10 20 30 40)).
	self assert: (x * 5) asStructuredArray 
			equals: #(#(5.0 10.0 15.0 20.0) #(50.0 100.0 150.0 200.0))
```

## Matrix product

```Smalltalk
Matrix>>+* anotherMatrix
	"Shortcut for the dot operator between matrices"
	^ self dot: anotherMatrix 
```

```Smalltalk
Matrix>>dot: anotherMatrix
	"Compute the dot product between the receiving matrix and the argument"
	| result resultArray |
	nbColumns = anotherMatrix nbRows ifFalse: [self error: 'dimensions do not conform'].
	self assert: [ nbRows * nbColumns = array size ].
	self assert: [ anotherMatrix nbRows * anotherMatrix nbColumns = anotherMatrix array size ].
	resultArray := ByteArray new: (nbRows * anotherMatrix nbColumns * 8).
	
	self 
		dot: self array getHandle with: nbRows with: nbColumns 
		with: anotherMatrix array getHandle
		with: anotherMatrix nbRows with: anotherMatrix nbColumns in: resultArray.

	result := MMatrix 
		newHandle: resultArray 
		rows: nbRows 
		columns: anotherMatrix nbColumns.
	^ result
```

The use of the C library is done using the following method:
```Smalltalk
Matrix>>dot: array1 with: m1_nb_rows with: m1_nb_columns with: array2 with: m2_nb_rows with: m2_nb_columns in: res

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
MatrixTest>>testDotProduct
	| m1 m2 |
	m1 := MMatrix newFromArrays: #(#(1 2 3 4) #(5 6 7 8)).
	m2 := MMatrix newFromArrays: #(#(1 2) #(3 4) #(5 6) #(7 8)).
	self assert: (m1 +* m2) asStructuredArray equals: #(#(50.0 60.0) #(114.0 140.0))
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
```Smalltalk
Matrix>>
```
