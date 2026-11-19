#!/usr/bin/env python3
"""Generate `Clash.Cores.Intel.Floating.Internal.CoefficientsDouble`: the polynomial coefficients of the
double precision bit-exact models of Intel's Floating Point Functions IP.

Usage (from the root of the repository):

  python3 tools/intel-floating-coefficients/gen_coefficients_double.py \\
    > src/Clash/Cores/Intel/Floating/Internal/CoefficientsDouble.hs

The tables are computed here, from first principles (see `recipes.py`); nothing is read from the IP. The model
overrides a few segments of the tables of `rsqrt`, `log`, `log2` and `exp2` with observed values, see
`BitExactDouble.hs`; this script emits its own value for every segment.
"""

from fractions import Fraction

import mpmath as mp

from recipes import MINIMAX, Recipe, haskell_table, main, to_mpf


def sqrt(s, u):
    j, p = s & 127, s >> 7
    return mp.sqrt((1 + mp.mpf(j) / 128 + u) * 2**p)


def exp(j, t):
    return mp.exp((j + t) / 128)


def rsqrt(s, t):
    p, j = s >> 8, s & 255
    return 2 / mp.sqrt((1 + (j + t) / 256) * 2**p)


def cbrt(s, t):
    return mp.cbrt((1 + ((s % 128) + t) / 128) * 2 ** (s // 128))


def exp2(j, t):
    return mp.power(2, (j + t) / 64)


def log_quotient(index_bits, base=None):
    """`log_base (1 + z) / z` (the natural logarithm without a base) on the four regions of the logarithms,
    each of `2^index_bits` segments: `z = offset + (j + t) / segments per unit`
    """
    regions = [(Fraction(0), 2), (Fraction(1, 2), 2), (Fraction(-1, 2), 4), (Fraction(-1, 4), 4)]

    def function(s, t):
        offset, per_unit = regions[s >> index_bits]
        z = to_mpf(offset) + ((s & ((1 << index_bits) - 1)) + t) / (per_unit << index_bits)
        v = mp.log1p(z) / z if z != 0 else mp.mpf(1)
        return v if base is None else v / mp.log(base)

    return function


# All tables: absolute error, 300 bits of working precision. The grids of `rsqrt` are those of its local
# variable `t = 256 * u`: the same integers as on the grids of `u` in the module's documentation.
TABLES = {
    "sqrtDoubleCoefficients": Recipe(
        function=sqrt,
        segments=256,
        grids=[55, 48, 41, 35, 27, 20],
        nodes=MINIMAX,
        end=Fraction(1, 128),
        precision=300,
        digits=120,
        evaluate_at="unrounded",
        delta=Fraction(99, 100),
        order="ascending",
    ),
    "expDoubleCoefficients": Recipe(
        function=exp,
        segments=89,
        grids=[55, 55, 55, 56, 55, 55],
        count=7,
        precision=300,
        digits=90,
        delta=Fraction(3, 4),
        order="ascending",
    ),
    "rsqrtDoubleCoefficients": Recipe(
        function=rsqrt,
        segments=512,
        grids=[55, 55, 55, 56, 55, 55],
        count=7,
        precision=300,
        digits=90,
        delta=Fraction(3, 4),
        order="ascending",
    ),
    "log10DoubleCoefficients": Recipe(
        function=log_quotient(9, 10),
        segments=2048,
        grids=[58, 58, 58, 59, 58],
        count=6,
        end=1 - Fraction(1, 2**42),
        scale_nodes="after",
        precision=300,
        digits=100,
        delta=Fraction(99, 100),
        order="descending",
    ),
    "cbrtDoubleCoefficients": Recipe(
        function=cbrt,
        segments=384,
        grids=[56, 56, 56, 57, 56, 56],
        count=7,
        precision=300,
        digits=100,
        delta=Fraction(99, 100),
        order="descending",
    ),
    "lnDoubleCoefficients": Recipe(
        function=log_quotient(9),
        segments=2048,
        # One bit coarser where the function is 1 or more (regions 2 and 3)
        grids=lambda s: [58, 58, 58, 59, 58] if s < 1024 else [57, 57, 57, 58, 57],
        count=6,
        end=1 - Fraction(1, 2**42),
        scale_nodes="after",
        precision=300,
        digits=100,
        delta=Fraction(99, 100),
        order="descending",
    ),
    "log2DoubleCoefficients": Recipe(
        function=log_quotient(7, 2),
        segments=512,
        grids=[56, 56, 56, 57, 56, 56],
        count=7,
        end=1 - Fraction(1, 2**44),
        scale_nodes="after",
        precision=300,
        digits=100,
        delta=Fraction(99, 100),
        order="descending",
    ),
    "exp2DoubleCoefficients": Recipe(
        function=exp2,
        segments=64,
        grids=[55, 55, 55, 56, 55, 55],
        count=7,
        precision=300,
        digits=100,
        delta=Fraction(99, 100),
        order="descending",
    ),
}

DOCS = {
    "log2DoubleCoefficients": """\
{- | Base 2 logarithm: 512 segments in four regions of 128, index @r * 128 + j@:
region 0 approximates @log2 (1 + z) / z@ for @z = (j + t) / 256@, region 1 for
@z = 1/2 + (j + t) / 256@, region 2 for @z = -1/2 + (j + t) / 512@ and region
3 for @z = -1/4 + (j + t) / 512@, @t@ in @[0, 1)@, by
@C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4 + C5 * t^5@. Units: C0, C1, C2,
C4 and C5 in 2^-56, C3 in 2^-57, in all regions. Computed like the base 10
logarithm's table, at seven Chebyshev nodes on @[0, 1 - 2^-44]@.

These are the generator's coefficients for all 512 segments. In two of them
(43 and 205) the IP uses other coefficients, which no variant of the method
reproduces; the model overrides those two, see Note [Observed double precision
base 2 logarithm coefficients] in
"Clash.Cores.Intel.Floating.Internal.BitExactDouble".
-}""",
    "exp2DoubleCoefficients": """\
{- | Base 2 exponential: 64 segments. Segment @j@ approximates
@2^((j + t) / 64)@ for @t@ in @[0, 1)@ by
@C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4 + C5 * t^5@. Units: C0, C1, C2,
C4 and C5 in 2^-55, C3 in 2^-56. Absolute error, the seven Chebyshev nodes of
the first kind on @[0, 1]@, LLL with delta = 0.99 and the highest degree
first, Babai's nearest plane.

These are the generator's coefficients for all 64 segments. In segment 50 the
IP uses other coefficients, which no variant of the method reproduces; the
model overrides that one, see Note [Observed double precision base 2
exponential coefficients] in
"Clash.Cores.Intel.Floating.Internal.BitExactDouble".
-}""",
    "lnDoubleCoefficients": """\
{- | Natural logarithm: 2048 segments in four regions of 512, index
@r * 512 + j@, as for the base 10 logarithm: segment @s@ approximates
@ln (1 + z) / z@ by @C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4@. Units: C0,
C1, C2 and C4 in 2^-58 and C3 in 2^-59 in regions 0 and 1; in regions 2 and 3
(where the function is 1 or more) in 2^-57 and 2^-58. Computed like the base
10 logarithm's table.

These are the generator's coefficients for all 2048 segments. In segment 1647
the IP uses other coefficients, which no variant of the method reproduces;
the model overrides that one, see Note [Observed double precision logarithm
coefficients] in "Clash.Cores.Intel.Floating.Internal.BitExactDouble".
-}""",
    "cbrtDoubleCoefficients": """\
{- | Cube root: 384 segments, index @r * 128 + j@, with @r@ the unbiased
exponent modulo 3 and @j@ the top 7 fraction bits. Segment @s@ approximates
@cbrt ((1 + (j + t) / 128) * 2^r)@ for @t@ in @[0, 1)@ by
@C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4 + C5 * t^5@. Units: C0, C1, C2,
C4 and C5 in 2^-56, C3 in 2^-57. Absolute error, the seven Chebyshev nodes of
the first kind on @[0, 1]@, LLL with delta = 0.99 and the highest degree
first, Babai's nearest plane.
-}""",
    "sqrtDoubleCoefficients": """\
{- | Square root: 256 segments, index @p * 128 + j@, with @p@ the parity of the
unbiased exponent and @j@ the top 7 fraction bits. Segment @s@ approximates
@sqrt ((1 + j / 128 + u) * 2^p)@ for @u@ in @[0, 2^-7)@ by
@C0 + C1 * u + C2 * u^2 + C3 * u^3 + C4 * u^4 + C5 * u^5@. Units (both
parities): C0 in 2^-55, C1 in 2^-48, C2 in 2^-41, C3 in 2^-35, C4 in 2^-27 and
C5 in 2^-20. Absolute error.
-}""",
    "expDoubleCoefficients": """\
{- | Exponential: 89 segments (the reduced argument @r@ is in @[0, ln 2)@, so
only 89 of the IP's 128 table entries are used). Segment @j@ approximates
@e^((j + t) / 128)@ for @t@ in @[0, 1)@ by
@C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4 + C5 * t^5@. Units: C0, C1, C2,
C4 and C5 in 2^-55, C3 in 2^-56. Absolute error, Chebyshev nodes (see above).
-}""",
    "rsqrtDoubleCoefficients": """\
{- | Reciprocal square root: 512 segments, index @p * 256 + j@, with @p = 1@
for an even biased exponent (else 0) and @j@ the top 8 fraction bits. Segment
@s@ approximates @2 / sqrt ((1 + j / 256 + u) * 2^p)@ for @u@ in @[0, 2^-8)@
by @C0 + C1 * u + C2 * u^2 + C3 * u^3 + C4 * u^4 + C5 * u^5@. Units (both
parities): C0 in 2^-55, C1 in 2^-47, C2 in 2^-39, C3 in 2^-32, C4 in 2^-23 and
C5 in 2^-15. Absolute error, Chebyshev nodes and LLL with delta 3/4, like the
exponential.

These are the generator's coefficients for all 512 segments. In two of them
(320 and 426) the IP uses other coefficients, which no variant of the method
reproduces; the model overrides those two, see Note [Observed double precision
rsqrt coefficients] in "Clash.Cores.Intel.Floating.Internal.BitExactDouble".
-}""",
    "log10DoubleCoefficients": """\
{- | Base 10 logarithm: 2048 segments in four regions of 512, index
@r * 512 + j@: region 0 approximates @log10 (1 + z) / z@ for @z = (j + t) / 1024@,
region 1 for @z = 1/2 + (j + t) / 1024@, region 2 for @z = -1/2 + (j + t) / 2048@ and
region 3 for @z = -1/4 + (j + t) / 2048@, @t@ in @[0, 1)@, by
@C0 + C1 * t + C2 * t^2 + C3 * t^3 + C4 * t^4@. Units: C0, C1, C2 and C4 in 2^-58,
C3 in 2^-59, in all regions. Absolute error, the six Chebyshev nodes of the
first kind on @[0, 1 - 2^-42]@ (the range of @t@), LLL with delta = 0.99 and
the highest degree first, Babai's nearest plane.
-}""",
}

HEADER = r"""{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Polynomial coefficients for the double precision bit-exact models of Intel's
Floating Point Functions IP, see
"Clash.Cores.Intel.Floating.Internal.BitExactDouble".

This module is generated by
@tools\/intel-floating-coefficients\/gen_coefficients_double.py@; don't edit it
by hand.

In double precision the IP approximates its functions on each segment by a
polynomial of degree five (the natural and the base 10 logarithm: four) in a
local variable @u@. The integer coefficients of
each segment were computed from first principles, with the method of
Brisebarre and Chevillard (\"Efficient polynomial L-infinity approximations\",
ARITH 18, 2007), which is the method behind Sollya's @fpminimax@:

  1. Compute the real minimax polynomial of degree five of the function on the
     segment (absolute error, Remez algorithm), and the six points where it
     interpolates the function.
  2. The lattice is spanned by the monomials @1, u, .., u^5@, each scaled by
     the weight of its coefficient's least significant bit and evaluated at
     these points; the target is the function at these points.
  3. Reduce the lattice with LLL (delta = 0.99) and find a close lattice vector
     with Babai's nearest plane algorithm. Its coordinates are the
     coefficients.

(The single precision tables use Chebyshev nodes instead of the interpolation
points of the minimax polynomial. In double precision, that gives the same
coefficients in all but one segment of the square root, where Babai's
algorithm meets a near tie.)

The exponential's table is computed the same way, except that the points are
the seven Chebyshev nodes of the first kind on the segment (the degree plus
two), and LLL uses delta = 3/4, the textbook value of Lenstra, Lenstra and
Lovasz. With the minimax interpolation points, about 55 of its 89 segments
give results that differ from the IP's; with delta = 0.99, 14 do. The
reciprocal square root's table is computed like the exponential's.

The tables of the natural, base 10 and the base 2 logarithm are computed like
the exponential's too, but with delta = 0.99 again, the lattice basis ordered
from the highest degree down, and the Chebyshev nodes (the degree plus two) on
the range of the local variable (which ends one unit of the input below the
end of the segment). The cube root's table is computed like the logarithms',
at seven Chebyshev nodes on the whole segment. The base 2 exponential's table
is computed like the cube root's.

Coefficient formats (the position of each coefficient's least significant bit)
were inferred from the IP's outputs and its resource reports; the
coefficients themselves are not taken from the IP and the module contains no
data taken from Intel's IP. The tables hold the plain polynomial coefficients:
the models add the rounding constants of the datapath themselves.

Each list holds @(C0, C1, C2, C3, C4, C5)@ (the natural and the base 10
logarithm: @(C0, C1, C2, C3, C4)@) per segment, in table order.
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.CoefficientsDouble
  ( sqrtDoubleCoefficients
  , expDoubleCoefficients
  , rsqrtDoubleCoefficients
  , log10DoubleCoefficients
  , cbrtDoubleCoefficients
  , lnDoubleCoefficients
  , log2DoubleCoefficients
  , exp2DoubleCoefficients
  ) where

import Prelude
"""


def render(tables):
    blocks = []
    for name, rows in tables.items():
        element_type = f"({', '.join(['Integer'] * len(rows[0]))})"
        blocks.append(haskell_table(DOCS[name], name, element_type, rows) + f"\n{{-# NOINLINE {name} #-}}")
    return HEADER + "\n" + "\n\n".join(blocks) + "\n"


if __name__ == "__main__":
    main(TABLES, render, __doc__)
