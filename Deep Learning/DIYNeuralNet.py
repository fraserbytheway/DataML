import numpy as np


def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def relu(x):
   return np.maximum(0, x)

class Layer():
    def __init__(self, inputs, outputs, activation=sigmoid):
        self.W = np.random.uniform(-1, 1, (inputs, outputs))
        self.activation = activation
        self.input = None
        self.output = None
        self.dell = np.zeros(outputs)

        self.b = np.zeros(outputs)

    def forward(self, x):
        self.input = x
        self.output = self.activation(x @ self.W + self.b)
        return self.output

class NeuralNet():
    def __init__(self, layers):
        # layer initialisation
        self.layers = []
        for i in range(len(layers) - 1):
            self.layers.append(Layer(layers[i], layers[i + 1]))

    def forward(self, X):
        for layer in self.layers:
            X = layer.forward(X)
        return X

    def train(self, X, y, rate, epochs):
        for _ in range(epochs):
            rows, cols = X.shape
            for r in range(rows):
                x = X[r, :]
                output = self.forward(x)
                # store output activation layers
                self.layers[-1].dell = np.array(output * (1 - output) * (y[r, :] - output))

                # compute hidden layer deltas
                for i in range(len(self.layers) - 2, -1, -1):
                    curr = self.layers[i]
                    forw = self.layers[i + 1]
                    dell = curr.output * (1 - curr.output) * np.dot(forw.W, forw.dell)
                    curr.dell = dell

                # compute SGD weight updates
                for i in range(len(self.layers) - 1, -1, -1):
                    curr = self.layers[i]
                    curr.W += rate * np.dot(curr.input.reshape(-1, 1), curr.dell.reshape(-1, 1).T)
                    curr.b += rate * curr.dell


if __name__ == '__main__':
    layers = [2, 2, 1]

    # XOR
    X = np.array([[0, 0], [0, 1], [1, 0], [1, 1]])
    y = np.array([[0], [1], [1], [0]])

    nn = NeuralNet(layers)
    print(nn.forward(X))

    nn.train(X, y, 0.1, 10000)
    print(nn.forward([0, 0]), nn.forward([1, 0]), nn.forward([0, 1]), nn.forward([1, 1]))