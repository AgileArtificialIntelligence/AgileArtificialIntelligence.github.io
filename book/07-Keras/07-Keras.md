
# Bridging Keras and Pharo

The previous chapter provide the complete code of a neural network. Although interesting from a pedagogical point of view, the code we provided is not suitable for building resource-consuming models. Using dedicated hardware, such as a GPU, is often essential. This chapter will guide the reader to build and train a Keras model from Python.

Keras is one of the most used Python library for building and training neural network. It is built on top of the numerical library TensorFlow. Keras is expressive and a complex model may be built in a few lines of code. 

Keras is built in Python. This chapter makes some short deviation outside the Pharo world by being  tainted with Pythonic scripts.

## Installing Keras

Here is a crucial step, which unfortunately, cannot be avoided. Keras needs to be installer prior using it from Pharo.

### Pipenv

Pipenv is a tool that ease the installation of complex Python applications and libraries, such as Keras. Pipenv makes the installation less error-prone. 
[https://pipenv.readthedocs.io/en/latest/](https://pipenv.readthedocs.io/en/latest/)