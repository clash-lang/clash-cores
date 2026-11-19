"""Polynomial coefficients on a grid, by the method of Brisebarre and Chevillard
("Efficient polynomial L-infinity approximations", ARITH 18, 2007), which is
the method behind Sollya's `fpminimax`.

A table approximates a function on each of its segments by a polynomial
`c[0] + c[1] * x + .. + c[d] * x^d` in a local variable `x`, with `c[i]` an
integer multiple of `2^-grids[i]`:

  1. Take a number of nodes in the range of `x` (`nodes`, `count`, `end`).
  2. The monomials `x^i`, each scaled by `2^-grids[i]` and evaluated at the
     nodes, span a lattice. The target is the function at the nodes. For a
     relative error, the basis vectors and the target are divided by the
     function at the node.
  3. Reduce the lattice with LLL (`delta`, `order`) and find a close lattice
     vector with Babai's nearest plane algorithm. Its coordinates are the
     coefficients.

The lattices have exact ties, and many near ties. Details that look irrelevant
therefore change the coefficients of some segments: the precision the function
is evaluated with, the number of digits the nodes and the function values are
rounded to before they become exact rationals, whether a node is scaled to its
interval before or after it is rounded, and so on. A `Recipe` states all of
them, and this module carries them out to the letter. Don't simplify the
arithmetic here: expressions that look equivalent round differently.
"""

import argparse
import math
import multiprocessing
import os
import sys
from dataclasses import dataclass
from fractions import Fraction

import mpmath as mp

import lattice

# Kinds of nodes
CHEBYSHEV = "chebyshev"  # Chebyshev nodes of the first kind
MINIMAX = "minimax"  # the points where the real minimax polynomial equals the function

# Rounding, instead of a number of decimal digits
EXACT = "exact"  # no rounding: the exact binary value of the multiprecision number
DOUBLE = "double"  # nodes only: see `double_nodes`

# Precision (bits) of the Remez algorithm that finds the minimax nodes
REMEZ_PRECISION = 300


@dataclass(frozen=True, kw_only=True)
class Recipe:
    """How to compute the coefficients of one table."""

    # (segment, x) -> the function to approximate, in mpmath arithmetic. Its expression matters: it is
    # evaluated with `precision` bits, and the last bits depend on the order of the operations.
    function: object
    # Number of segments
    segments: int
    # Coefficient `i` is a multiple of `2^-grids[i]`. A list, or a function from the segment to a list.
    grids: object
    # CHEBYSHEV: `count` nodes on `[0, end]`. MINIMAX: the degree plus one points of `[0, end]` where the
    # minimax polynomial of that degree (absolute error) equals the function. The Remez algorithm finds them
    # with REMEZ_PRECISION bits, whatever `precision` is.
    nodes: str = CHEBYSHEV
    count: int = None
    end: Fraction = Fraction(1)
    # Chebyshev nodes are scaled from `[0, 1]` to `[0, end]` "before" or "after" they are rounded
    scale_nodes: str = "before"
    # mpmath working precision in bits
    precision: int
    # Decimal digits that the nodes and the function values (and the weights of a relative error) are rounded
    # to (round to nearest, `mpmath.nstr`), or EXACT
    digits: object
    # Rounding of the nodes, if it differs from `digits`
    node_digits: object = None
    # The function is evaluated at the "rounded" nodes, or at the "unrounded" ones
    evaluate_at: str = "rounded"
    # LLL's Lovasz parameter
    delta: Fraction
    # Order of the basis vectors given to LLL: by "ascending" or "descending" degree
    order: str = "ascending"
    # "absolute" or "relative" error
    error: str = "absolute"
    # The table stores the coefficients times this number. An integer, or a function from the segment to one.
    multiplier: object = 1

    def grids_of(self, segment):
        return list(self.grids(segment) if callable(self.grids) else self.grids)

    def multiplier_of(self, segment):
        return self.multiplier(segment) if callable(self.multiplier) else self.multiplier


def to_mpf(q):
    """A rational as a multiprecision number, the way all recipes do it"""
    return mp.mpf(q.numerator) / q.denominator


def rounded(x, digits):
    """A multiprecision number as an exact rational, rounded to a number of decimal digits (or EXACT)"""
    if digits == EXACT:
        mantissa, exponent = mp.frexp(x)
        return Fraction(int(mp.ldexp(mantissa, mp.mp.prec))) * Fraction(2) ** (exponent - mp.mp.prec)
    return Fraction(mp.nstr(x, digits))


def chebyshev(n):
    """The `n` Chebyshev nodes of the first kind on `[0, 1]`, in the working precision"""
    return [mp.mpf(1) / 2 - mp.cos((2 * k + 1) * mp.pi / (2 * n)) / 2 for k in range(n)]


def double_nodes(n):
    """The `n` Chebyshev nodes of the first kind on `[0, 1]`, computed in IEEE 754 double precision
    arithmetic (with a correctly rounded cosine) and then replaced by the closest fraction with a denominator
    of at most 10^12
    """
    nodes = []
    for k in range(n):
        angle = (2 * k + 1) * math.pi / (2 * n)
        with mp.workprec(53):
            cosine = float(mp.cos(mp.mpf(angle)))
        nodes.append(Fraction(0.5 - 0.5 * cosine).limit_denominator(10**12))
    return nodes


def minimax_nodes(f, degree, end):
    """The `degree + 1` points of `[0, end]` where the minimax polynomial of `f` (absolute error) of that
    degree equals `f`: the nodes that Sollya's `fpminimax` uses. Remez exchange algorithm.
    """
    n = degree
    with mp.workprec(REMEZ_PRECISION):
        b = to_mpf(end)
        xs = [b / 2 - b / 2 * mp.cos(mp.pi * k / (n + 1)) for k in range(n + 2)]
        for _ in range(30):
            m = mp.matrix(n + 2, n + 2)
            r = mp.matrix(n + 2, 1)
            for i, x in enumerate(xs):
                for k in range(n + 1):
                    m[i, k] = x**k
                m[i, n + 1] = (-1) ** i
                r[i] = f(x)
            s = mp.lu_solve(m, r)
            c = [s[k] for k in range(n + 1)]

            def err(x, c=c):
                return f(x) - sum(c[k] * x**k for k in range(n + 1))

            # The extrema of the error: search on a fine grid, then refine
            steps = 2000
            grid = [b * i / steps for i in range(steps + 1)]
            ev = [err(x) for x in grid]
            extrema = [grid[0]]
            for i in range(1, steps):
                if (ev[i] - ev[i - 1]) * (ev[i + 1] - ev[i]) <= 0:
                    try:
                        xm = mp.findroot(lambda x: mp.diff(err, x), grid[i])
                    except Exception:  # noqa: BLE001 (no convergence: keep the grid point)
                        xm = grid[i]
                    if not (0 < xm < b):
                        xm = grid[i]
                    extrema.append(xm)
            extrema.append(grid[-1])
            if len(extrema) != n + 2:
                break
            converged = max(abs(xs[i] - extrema[i]) for i in range(n + 2)) < b * mp.mpf(2) ** -150
            xs = extrema
            if converged:
                break
        return [mp.findroot(err, (xs[i], xs[i + 1]), solver="anderson") for i in range(n + 1)]


def chebyshev_nodes(recipe):
    """The nodes of a CHEBYSHEV recipe as exact rationals, and before rounding (if that exists)"""
    digits = recipe.digits if recipe.node_digits is None else recipe.node_digits
    if digits == DOUBLE:
        assert recipe.end == 1
        return double_nodes(recipe.count), None
    if recipe.scale_nodes == "before":
        points = [to_mpf(recipe.end) * x for x in chebyshev(recipe.count)]
        return [rounded(x, digits) for x in points], points
    assert recipe.scale_nodes == "after"
    return [rounded(x, digits) * recipe.end for x in chebyshev(recipe.count)], None


def make_basis(grids, nodes, weights):
    return [[Fraction(1, 2**g) * t**i * w for t, w in zip(nodes, weights)] for i, g in enumerate(grids)]


def reduce_basis(recipe, basis):
    """The LLL-reduced basis and its Gram-Schmidt orthogonalisation"""
    ordered = basis[::-1] if recipe.order == "descending" else basis
    assert recipe.order in ("ascending", "descending")
    reduced = lattice.lll(ordered, recipe.delta)
    return reduced, lattice.gram_schmidt(reduced)[0]


def shareable(recipe):
    """Whether the lattice is the same for all segments (with the same grids)"""
    return recipe.nodes == CHEBYSHEV and recipe.error == "absolute"


def shared_lattice(recipe, grids):
    """The part of a table's computation that does not depend on the segment"""
    mp.mp.prec = recipe.precision
    nodes, points = chebyshev_nodes(recipe)
    basis = make_basis(grids, nodes, [Fraction(1)] * len(nodes))
    return (nodes, points, basis) + reduce_basis(recipe, basis)


def coefficients(recipe, segment, shared=None):
    """The coefficients `[c[0], c[1], ..]` of one segment, as integers: in units of `2^-grids[i]`, times the
    recipe's multiplier
    """
    mp.mp.prec = recipe.precision
    grids = recipe.grids_of(segment)

    def f(x):
        return recipe.function(segment, x)

    if recipe.nodes == MINIMAX:
        points = minimax_nodes(f, len(grids) - 1, recipe.end)
        nodes = [rounded(x, recipe.digits) for x in points]
    elif shared is not None:
        nodes, points, basis, reduced, ortho = shared
    else:
        nodes, points = chebyshev_nodes(recipe)
    if recipe.evaluate_at == "rounded":
        points = [to_mpf(t) for t in nodes]
    else:
        assert recipe.evaluate_at == "unrounded" and points is not None
    values = [f(x) for x in points]
    target = [rounded(v, recipe.digits) for v in values]
    if recipe.error == "relative":
        weights = [rounded(1 / v, recipe.digits) for v in values]
        target = [v * w for v, w in zip(target, weights)]
    else:
        assert recipe.error == "absolute"
        weights = [Fraction(1)] * len(nodes)
    if recipe.nodes == MINIMAX or shared is None:
        basis = make_basis(grids, nodes, weights)
        reduced, ortho = reduce_basis(recipe, basis)
    close = lattice.babai(reduced, target, ortho)
    return [c * recipe.multiplier_of(segment) for c in lattice.coordinates(basis, close)]


# The tables being generated and their segment-independent lattices. Worker processes inherit them (fork).
_TABLES = {}
_SHARED = {}


def _shared_job(key):
    name, grids = key
    return shared_lattice(_TABLES[name], list(grids))


def _segment_job(job):
    name, segment = job
    recipe = _TABLES[name]
    return coefficients(recipe, segment, _SHARED.get((name, tuple(recipe.grids_of(segment)))))


def generate(tables, processes):
    """Compute all tables: a dictionary from the table's name to its `Recipe` gives one from the name to
    the list of the coefficients of its segments
    """
    global _TABLES, _SHARED
    _TABLES, _SHARED = tables, {}
    context = multiprocessing.get_context("fork")
    keys = sorted(
        {
            (name, tuple(recipe.grids_of(segment)))
            for name, recipe in tables.items()
            if shareable(recipe)
            for segment in range(recipe.segments)
        }
    )
    with context.Pool(processes) as pool:
        _SHARED = dict(zip(keys, pool.map(_shared_job, keys, chunksize=1)))
    jobs = [(name, segment) for name, recipe in tables.items() for segment in range(recipe.segments)]
    with context.Pool(processes) as pool:
        rows = pool.map(_segment_job, jobs, chunksize=1)
    result = {name: [] for name in tables}
    for (name, _), row in zip(jobs, rows):
        result[name].append(row)
    return result


def haskell_table(doc, name, element_type, rows):
    """A table as a Haskell list of tuples, with its documentation comment and type signature"""
    lines = [doc, f"{name} :: [{element_type}]", f"{name} ="]
    for i, row in enumerate(rows):
        lines.append(f"  {'[' if i == 0 else ','} ({', '.join(str(c) for c in row)})")
    lines.append("  ]")
    return "\n".join(lines)


def main(tables, render, description):
    """Command line of a generator script: compute `tables` and write `render (generate tables)` to stdout"""
    parser = argparse.ArgumentParser(description=description, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument(
        "-j", "--jobs", type=int, default=os.cpu_count(), help="number of processes (default: all cores)"
    )
    parser.add_argument(
        "--only", metavar="TABLE", action="append", help="debugging: print only this table's rows (repeatable)"
    )
    args = parser.parse_args()
    if args.only:
        for name in args.only:
            for i, row in enumerate(generate({name: tables[name]}, args.jobs)[name]):
                print(i, *row)
        return
    sys.stdout.write(render(generate(tables, args.jobs)))
