import numpy as np
import time
from numba import njit

@njit
def uppertri_solve(A, b):

    rows, cols = A.shape

    # find solution to Ax = b in place
    for i in range(rows-1, -1, -1):

        targ_row = A[i, :].copy()
        targ_row[i] = 0

        b[i] = 1/A[i][i] * (b[i] - np.dot(targ_row, b))

    return b

@njit
def lowertri_solve(A, b):

    rows, cols = A.shape

    for i in range(rows):

        targ_row = A[i, :].copy()
        targ_row[i] = 0
        b[i] = 1 / A[i][i] * (b[i] - np.dot(targ_row, b))

    return b

#@njit
def LU_permutation(A):
    """
    Given a matrix A, permute rows, to get row major format
    """

    rows, cols = A.shape
    perm = np.eye(rows)

    for c in range(cols):

        target = A[c:, c]
        coords = int(np.abs(target).argmax())

        temp = np.eye(rows)
        temp[[c, coords + c]] = temp[[coords + c, c]]

        perm = temp @ perm
        A = temp @ A

    return perm

#@njit
def LU_factor(A):
    """
    Given a matrix A, compute the LU factorisation of A
    """

    rows, cols = A.shape

    L = np.eye(rows)
    U = A.copy()

    for c in range(cols):
        # want to create a transform that produces a LU matrix

        M = np.eye(rows)
        target = U[:, c].copy()
        ratio = target[c]
        target[:c] = np.zeros_like(target[:c])
        target[c+1:] *= -1
        target = target / ratio

        M[:, c] = target
        L = M @ L
        U = M @ U
        #print(f"M:\n\n{M}\n\n")
        #print(f"{M@A}")

    return np.linalg.inv(L), U


if __name__ == '__main__':

    run_config = 4
    arr_size = 1000

    if run_config == 1:
        A = np.random.randint(1, 10, size=(4,4)).astype('float64')
        A = np.triu(A)
        b = np.random.randint(1, 10, size=4).astype('float64')
        print(f"\nTrying to solve Ax = b\n")
        print(f"A:\n {A}\n")
        print(f"b:\n {b}\n")

        x = uppertri_solve(A, b)
        print(f"Solution: x = {x}")
        print(f"Check: Ax = {A@x}")
    elif run_config == 2:
        A = np.random.randint(1, 10, size=(4, 4)).astype('float64')
        print(f"A:\n\n{A}\n\n")
        P = LU_permutation(A)
        print(f"Permuatation Matrix:\n\n{P}\n\n")
        print(f"A Permuted:\n\n {P@A}")

    elif run_config == 3:

        A = np.random.randint(1, 10, size=(4, 4)).astype('float64')
        A = LU_permutation(A) @ A

        print(f"A:\n\n{A}\n\n")
        L, U = LU_factor(A)
        print(f"L:\n\n{L}\n\n")
        print(f"U:\n\n{U}")

        print(f"L @ U:\n\n{L@U}")

    elif run_config == 4:

        A = np.random.randint(1, 10, size=(arr_size, arr_size)).astype('float64')
        ref = A.copy()
        b = np.random.randint(1, 10, size=arr_size).astype('float64')
        refb = b.copy()

        start_LU = time.perf_counter()

        P = LU_permutation(A)

        A = P @ A
        b = P @ b
        L, U = LU_factor(A)

        y = lowertri_solve(L, b)
        x_LU = uppertri_solve(U, y)

        end_LU = time.perf_counter()
        time_custom = end_LU - start_LU

        start_numpy = time.perf_counter()

        x_numpy = np.linalg.solve(ref, refb)

        end_numpy = time.perf_counter()
        time_numpy = end_numpy - start_numpy

        print(f"Custom x:\n{x_LU}\n")
        print(f"NumPy x:\n{x_numpy}\n")
        print(f"Check Custom A@x (Should match original b):\n{ref @ x_LU}\n")

        print("--- Time Comparison ---")
        print(f"Custom LU Time: {time_custom:.6f} seconds")
        print(f"NumPy Time:     {time_numpy:.6f} seconds")
        print(f"NumPy is roughly {time_custom / time_numpy:.1f}x faster")

