"""Exact lattice arithmetic on vectors of rationals (`fractions.Fraction`).

Everything here is exact: the coefficient tables depend on how ties are broken, so
no floating point enters these routines. `round` is Python's rounding of a
fraction to the nearest integer, ties to even.
"""

from fractions import Fraction


def dot(u, v):
    return sum(a * b for a, b in zip(u, v))


def gram_schmidt(basis):
    """Gram-Schmidt orthogonalisation without normalisation.

    Returns the orthogonal vectors and the coefficients `mu`:
    `mu[i][j] = <basis[i], ortho[j]> / <ortho[j], ortho[j]>` for `j < i`.
    """
    n = len(basis)
    ortho = []
    mu = [[Fraction(0)] * n for _ in range(n)]
    for i in range(n):
        v = list(basis[i])
        for j in range(i):
            mu[i][j] = dot(basis[i], ortho[j]) / dot(ortho[j], ortho[j])
            v = [a - mu[i][j] * b for a, b in zip(v, ortho[j])]
        ortho.append(v)
    return ortho, mu


def lll(basis, delta):
    """LLL reduction (Lenstra, Lenstra, Lovasz) with Lovasz parameter `delta`.

    The textbook algorithm: size-reduce vector `k` against the vectors before
    it, from `k - 1` down to 0, then either move on or swap it with vector
    `k - 1`. The result depends on the order of the vectors in `basis`.
    """
    basis = [list(b) for b in basis]
    n = len(basis)
    ortho, mu = gram_schmidt(basis)
    k = 1
    while k < n:
        for j in range(k - 1, -1, -1):
            q = round(mu[k][j])
            if q:
                basis[k] = [a - q * b for a, b in zip(basis[k], basis[j])]
                # The orthogonal vectors stay the same, and so do the other rows of `mu`
                for i in range(j):
                    mu[k][i] -= q * mu[j][i]
                mu[k][j] -= q
        if dot(ortho[k], ortho[k]) >= (delta - mu[k][k - 1] ** 2) * dot(ortho[k - 1], ortho[k - 1]):
            k += 1
        else:
            basis[k], basis[k - 1] = basis[k - 1], basis[k]
            ortho, mu = gram_schmidt(basis)
            k = max(k - 1, 1)
    return basis


def babai(reduced, target, ortho=None):
    """Babai's nearest plane algorithm: a lattice vector close to `target`.

    `reduced` is an (LLL-reduced) basis of the lattice and `ortho` its
    Gram-Schmidt orthogonalisation (computed here if not given).
    """
    if ortho is None:
        ortho, _ = gram_schmidt(reduced)
    rest = list(target)
    for i in range(len(reduced) - 1, -1, -1):
        c = round(dot(rest, ortho[i]) / dot(ortho[i], ortho[i]))
        rest = [x - c * y for x, y in zip(rest, reduced[i])]
    return [x - y for x, y in zip(target, rest)]


def coordinates(basis, vector):
    """The coordinates `c` of a lattice vector: `sum (c[i] * basis[i]) = vector`.

    Gaussian elimination on the first `len(basis)` components, which determine
    the coordinates when `vector` is in the lattice. Raises if they are not
    integers.
    """
    d = len(basis)
    rows = [[basis[i][k] for i in range(d)] + [vector[k]] for k in range(d)]
    for c in range(d):
        p = next(r for r in range(c, d) if rows[r][c] != 0)
        rows[c], rows[p] = rows[p], rows[c]
        for r in range(d):
            if r != c and rows[r][c] != 0:
                f = rows[r][c] / rows[c][c]
                rows[r] = [x - f * y for x, y in zip(rows[r], rows[c])]
    coords = [rows[r][d] / rows[r][r] for r in range(d)]
    if any(c.denominator != 1 for c in coords):
        raise ArithmeticError(f"vector is not in the lattice: {coords}")
    return [int(c) for c in coords]
