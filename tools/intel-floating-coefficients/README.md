# Coefficient generators for `Clash.Cores.Intel.Floating`

The bit-exact models of Intel's Floating Point Functions IP evaluate a polynomial per segment of
their argument. The coefficients of these polynomials are not taken from the IP: they are
computed from first principles, by the scripts in this directory. Each script writes one Haskell
module to stdout:

| Script                         | Module (in `src/Clash/Cores/Intel/Floating/Internal/`) |
|--------------------------------|--------------------------------------------------------|
| `gen_coefficients.py`          | `Coefficients.hs` (single precision)                   |
| `gen_coefficients_pow.py`      | `CoefficientsPow.hs` (single precision power function) |
| `gen_coefficients_half.py`     | `CoefficientsHalf.hs` (half precision)                 |
| `gen_coefficients_double.py`   | `CoefficientsDouble.hs` (double precision)             |

The shared code is in `recipes.py` (nodes, the Remez algorithm, the computation of a table from
its recipe, the command line) and `lattice.py` (exact LLL reduction, Babai's nearest plane
algorithm and the linear solve).

## Running

The scripts need Python 3.10 or later with [mpmath](https://mpmath.org), on Linux or macOS (they
fork worker processes). From the root of the repository:

```
python3 tools/intel-floating-coefficients/gen_coefficients_double.py \
  > src/Clash/Cores/Intel/Floating/Internal/CoefficientsDouble.hs
```

or, with Nix:

```
nix shell --impure --expr \
  '(builtins.getFlake "nixpkgs").legacyPackages.${builtins.currentSystem}.python3.withPackages (p: [ p.mpmath ])' \
  -c python3 tools/intel-floating-coefficients/gen_coefficients_double.py \
  > src/Clash/Cores/Intel/Floating/Internal/CoefficientsDouble.hs
```

Options:

- `-j N`: the number of worker processes (default: all cores).
- `--only TABLE`: print the rows of one table (`index c0 c1 ..`) instead of the module, for
  example `--only sqrtDoubleCoefficients`. Useful while working on a recipe.

Time taken, and CPU time (the scripts run one process per core):

| Script                       | 12 processes | CPU time |
|------------------------------|--------------|----------|
| `gen_coefficients.py`        | 10 s         | 2 min    |
| `gen_coefficients_pow.py`    | 0.3 s        | 2 s      |
| `gen_coefficients_half.py`   | 4 s          | 45 s     |
| `gen_coefficients_double.py` | 50 s         | 10 min   |

Nearly all of it goes to the three tables with minimax nodes (the double precision square root,
the single precision sine, the logarithm of the half precision power function).

## The output must equal the committed modules

The modules in the repository are the output of these scripts, byte for byte. After a change
here, check that with:

```
for m in :Coefficients _pow:CoefficientsPow _half:CoefficientsHalf _double:CoefficientsDouble; do
  python3 tools/intel-floating-coefficients/gen_coefficients${m%%:*}.py \
    | diff - src/Clash/Cores/Intel/Floating/Internal/${m#*:}.hs
done
```

The scripts have no inputs, no random numbers and no machine arithmetic that depends on the
platform (see `double_nodes` in `recipes.py` for the one place where double precision arithmetic
is involved), so the output is the same everywhere. The committed modules were generated with
mpmath 1.3.0 and Python 3.14; mpmath 1.4.1 gives the same output. A future version of mpmath
could round an elementary function differently in its last bit, and that can change a
coefficient: see below.

## Recipes

Every table is computed with the method of Brisebarre and Chevillard ("Efficient polynomial
L-infinity approximations", ARITH 18, 2007), the method behind Sollya's `fpminimax`: the
monomials, scaled by the weight of their coefficient's least significant bit and evaluated at a
number of nodes, span a lattice, and the coefficients are the coordinates of a lattice vector
close to the function's values at the nodes (LLL reduction, then Babai's nearest plane
algorithm).

Each script has one dictionary `TABLES` that gives the recipe of every table: the function, the
number of segments, the grids of the coefficients, the kind and the number of nodes and their
interval, how nodes and function values are rounded, the working precision, LLL's delta, the
order of the basis vectors and the kind of error. `Recipe` in `recipes.py` documents the fields.

**Don't tidy up the numerics.** The lattices have exact ties and many near ties, so details that
look irrelevant decide the coefficients of some segments: the number of digits the nodes are
rounded to, the working precision, LLL's delta (0.99 or 3/4), the order of the basis vectors,
whether the function is evaluated at a node before or after the node is rounded, the order of the
operations in a function's expression, and Python's `round` on fractions (ties to even). The
recipes differ between tables for that reason, and for a historical one: the tables were found
one by one, each with the variant that reproduced the IP's behaviour, and some details (such as
the square root of single precision, whose nodes are computed in double precision) are accidents
of that history that the committed tables now depend on. Change a recipe only to change a table
on purpose, and then verify the model against the IP again.

A few segments of the double precision tables of `rsqrt`, `log`, `log2` and `exp2` differ in the
IP from what the method gives. The model overrides them with observed values in
`BitExactDouble.hs`; the scripts emit their own value for every segment.
