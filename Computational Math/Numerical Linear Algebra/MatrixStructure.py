import numpy as np
from scipy.linalg import circulant

def cholesky_factorisation(A):
    """
    Computes the cholesky factorisation of A, in place. Returns R, where A = R.T @ R
    :param A:
    :return:
    """

    rows, cols = np.shape(A)

    for r in range(rows):

        # diagonal element update
        chunk = np.sum(A[:r, r] ** 2)
        A[r][r] = np.sqrt(A[r][r] - chunk)

        # off diagonal elements for the row
        for c in range(r+1, cols):

            slice1 = A[:r, r]
            slice2 = A[:r, c]

            A[r][c] = 1/A[r][r]*(A[r][c] - np.dot(slice1, slice2))
            A[c][r] = 0

    return A

def circulant_mult(A, B):
    """
    Computing using the convolution operator
    """

    rows, cols = A.shape
    a = A[0, :]
    b = B[0, :]
    c = np.zeros(cols)

    for i in range(cols):
        c[i] = np.dot(np.roll(a[::-1], i+1), b)

    return np.array([np.roll(c, i) for i in range(rows)]).squeeze()

def fft_mul(A, B):
    target_len = len(A) + len(B) - 1
    A_hat = np.fft.fft(A, n = target_len)
    B_hat = np.fft.fft(B, n = target_len)
    C = A_hat * B_hat
    return np.fft.ifft(C)



if __name__ == '__main__':
    run_config = 3

    if run_config == 1:
        n = 4
        A = np.random.randint(1, 10, size=(n,n)).astype('float64')
        A = A @ A.T # A is now a random positive semidefinite matrix
        A2 = A.copy()

        R = cholesky_factorisation(A.copy())
        print("\n\nA:\n\n", A, "\n")
        print("R:\n\n", R, "\n")
        print("R.TR:\n\n", R.T @ R, "\n")
        print("R.TR - A\n\n", R.T @ R - A)
    elif run_config == 2:
        n = 5
        a = np.random.randint(1, 10, size=(n, 1))
        A = np.array([np.roll(a, i) for i in range(n)]).squeeze()

        b = np.random.randint(1, 10, size=(n, 1))
        B = np.array([np.roll(b, i) for i in range(n)]).squeeze()

        C = circulant_mult(A, B)
        print("\n\nC - AB:\n\n", C - A@B, "\n")
    elif run_config == 3:
        n = 5
        a = np.random.randint(1, 10, size=(n, 1))
        A = np.array([np.roll(a, i) for i in range(n)]).squeeze()

        b = np.random.randint(1, 10, size=(n, 1))
        B = np.array([np.roll(b, i) for i in range(n)]).squeeze()

        C = fft_mul(A, B)
        print("\n\nC - AB:\n\n", C - A @ B, "\n")
