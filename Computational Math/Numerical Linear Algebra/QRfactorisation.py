import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def QRfactor(A):
    rows, cols = A.shape
    R = A.astype(float).copy()
    Q = np.eye(rows)

    for c in range(cols):
        curr = R[:, c]
        target = np.zeros_like(curr)

        target[:c] = curr[:c]
        target[c] = np.linalg.norm(curr[c:])
        sign = 1 if curr[c] > 0 else -1
        target[c] *= -sign

        lambd = sign/(np.abs(curr[c]) + np.abs(target[c]))
        v = lambd * (curr - target)
        tau = 2/(np.linalg.norm(v)**2)

        H = np.eye(rows) - tau * np.outer(v, v)

        Q = H @ Q
        R = H @ R

    return np.linalg.inv(Q), R

def subQR(Q, R):
    rows, cols = R.shape

    r = 0
    while np.abs(R[r][cols - 1]) > 0.000001:
        r += 1

    Q1 = Q[:, :r]
    R1 = R[:r, :]
    Q2 = Q[:, r:]
    R2 = R[r:, :]

    return Q1, R1, Q2, R2

def question1():
    A = np.array([[-4, 3, 1], [2, 9, 5], [4, -1, 6], [2, 5, 8], [0, -5, 7], [1, 8, 2]])
    b = np.array([-7, 30, 51, 62, 48, 11]).T

    Q, R = QRfactor(A)
    np.set_printoptions(precision=3, suppress=True)
    print("Q\n", Q)
    print("R\n", R)

    print(A - Q @ R)
    print(Q.T @ Q - np.eye(Q.shape[0]))

    # find the QR sub matrices
    Q1, R1, Q2, R2 = subQR(Q, R)

    x = np.linalg.inv(R1) @ Q1.T @ b

    print("Least Squares Solution", x)
    print("Residual", np.linalg.norm(Q2.T @ b))

def question2():
    data = np.loadtxt("xy_data.txt")
    x = data[:, 0]
    y = data[:, 1]
    A = np.zeros((26, 10))

    for i in range(26):
        for j in range(10):
            A[i][j] = x[i]**(j)

    # polynomials of degree 0
    fig, axs = plt.subplots(5, 2, constrained_layout = True)
    axs = axs.flatten()
    for i in range(1, 11):
        Q, R = QRfactor(A[:, :i])
        Q1, R1, Q2, R2 = subQR(Q, R)

        x = np.linalg.inv(R1) @ Q1.T @ y
        print("Least Squares Solution\n", x)
        print("Residual:", np.linalg.norm(Q2.T @ y))

        grid_x = np.linspace(0, 5, 100)
        grid_y = np.zeros_like(grid_x)
        for l in range(x.shape[0]):
            grid_y += grid_x**(l+1) * x[l]

        axs[i-1].plot(grid_x, grid_y)
        axs[i-1].plot(data[:, 0], data[:, 1])
        axs[i-1].set_title(f"Fitted Polynomial Degree {i-1}. Residual Size = {round(np.linalg.norm(Q2.T @ y), 2)}")

    plt.show()


if __name__ == "__main__":

    #question1()
    question2()




