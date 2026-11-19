#!/usr/bin/env python3
"""Generate `Clash.Cores.Intel.Floating.Internal.Coefficients`: the polynomial coefficients of the single
precision bit-exact models of Intel's Floating Point Functions IP (the power function has its own module, see
`gen_coefficients_pow.py`).

Usage (from the root of the repository):

  python3 tools/intel-floating-coefficients/gen_coefficients.py \\
    > src/Clash/Cores/Intel/Floating/Internal/Coefficients.hs

The tables are computed here, from first principles (see `recipes.py`); nothing is read from the IP.
"""

from fractions import Fraction

import mpmath as mp

from recipes import DOUBLE, EXACT, MINIMAX, Recipe, haskell_table, main


def sqrt(s, t):
    p, j = s >> 7, s & 127
    return mp.sqrt((1 + mp.mpf(j) / 128 + t / 128) * 2**p)


def rsqrt(s, t):
    p, j = s >> 8, s & 255
    return 2 / mp.sqrt((1 + mp.mpf(j) / 256 + t / 256) * 2**p)


def cbrt(s, t):
    r, j = s >> 7, s & 127
    return mp.cbrt((1 + (j + t) / 128) * mp.mpf(2) ** r)


def log_quotient(base=None):
    """`log_base (1 + z) / z` (the natural logarithm without a base) on the four regions of the logarithms,
    each of 128 segments
    """

    def function(s, t):
        r, j = s >> 7, s & 127
        if r == 0:
            z = (j + t) / 256
        elif r == 1:
            z = mp.mpf(1) / 2 + (j + t) / 256
        elif r == 2:
            z = -mp.mpf(1) / 2 + (j + t) / 512
        else:
            z = -mp.mpf(1) / 4 + (j + t) / 512
        v = mp.log1p(z) / z if z != 0 else mp.mpf(1) - z / 2
        return v if base is None else (1 / mp.log(base)) * v

    return function


def sinpi_over(j, t):
    """sin (pi * r) / r for r = (j + t) / 256"""
    r = (j + t) / 256
    return mp.sin(mp.pi * r) / r if r != 0 else mp.pi


def sin(j, t):
    """sin (pi * r) / r for r = (j + t) / 512"""
    return mp.sin(mp.pi * (j + t) / 512) / ((j + t) / 512) if j + t != 0 else +mp.pi


def atan_over(j, t):
    """atan r / r for r = (j + t) / 256"""
    r = (j + t) / 256
    return mp.atan(r) / r if r != 0 else mp.mpf(1)


def asin_over(j, t):
    """asin r / r for r = (j + t) / 256"""
    r = (j + t) / 256
    return mp.asin(r) / r if r != 0 else mp.mpf(1)


def recipe(function, segments, grids, **kwargs):
    """What most single precision tables share: degree two, four Chebyshev nodes on [0, 1], absolute error,
    150 bits of working precision, nodes and function values rounded to 45 digits, the function evaluated at
    the rounded nodes, LLL with delta = 0.99 on the basis in ascending order of degree. Keyword arguments
    override these.
    """
    details = {
        "count": 4,
        "precision": 150,
        "digits": 45,
        "evaluate_at": "rounded",
        "delta": Fraction(99, 100),
        "order": "ascending",
        "error": "absolute",
    }
    return Recipe(function=function, segments=segments, grids=grids, **{**details, **kwargs})


def below(k):
    """The end of the range of a local variable `t` of `k` bits: `1 - 2^-k`"""
    return 1 - Fraction(1, 2**k)


# name: recipe (function of the segment and t, number of segments, grids of [c0, c1, c2], other details)
TABLES = {
    # One bit coarser for an odd exponent (the second half). The nodes are computed in double precision, and
    # the function is evaluated with 120 bits and rounded to 40 digits.
    "sqrtCoefficients": recipe(
        sqrt, 256, lambda s: [27, 27, 27] if s < 128 else [26, 26, 26], node_digits=DOUBLE, precision=120, digits=40
    ),
    # One bit coarser for an odd biased exponent (the first half), stored in the units of the second half.
    # 200 bits, nodes and function values not rounded to decimals.
    "rsqrtCoefficients": recipe(
        rsqrt,
        512,
        lambda s: [26, 26, 26] if s < 256 else [27, 27, 27],
        precision=200,
        digits=EXACT,
        multiplier=lambda s: 2 if s < 256 else 1,
    ),
    "expCoefficients": recipe(lambda j, t: mp.exp((j + t) / 128), 89, [26, 26, 26], error="relative", multiplier=2),
    "exp2Coefficients": recipe(
        lambda j, t: mp.exp((j + t) / 128 * mp.log(2)), 128, [26, 26, 26], error="relative", multiplier=2
    ),
    "exp10Coefficients": recipe(
        lambda j, t: mp.exp((j + t) / 512 * mp.log(10)), 155, [26, 26, 26], error="relative", multiplier=2
    ),
    # One bit coarser where the function is 1 or more (regions 2 and 3)
    "logCoefficients": recipe(
        log_quotient(),
        512,
        lambda s: [29, 29, 29] if s < 256 else [28, 28, 28],
        end=below(15),
        evaluate_at="unrounded",
        multiplier=2,
    ),
    "log2Coefficients": recipe(
        log_quotient(2), 512, [27, 27, 27], end=below(15), evaluate_at="unrounded", multiplier=2
    ),
    "log10Coefficients": recipe(
        log_quotient(10), 512, [29, 29, 29], end=below(15), evaluate_at="unrounded", multiplier=2
    ),
    "cbrtCoefficients": recipe(cbrt, 384, [27, 27, 27], end=below(16), evaluate_at="unrounded"),
    "sinPiCoefficients": recipe(sinpi_over, 128, [26, 26, 26], end=below(16)),
    "tanPiRecipCoefficients": recipe(lambda j, t: 1 / (1 + (j + t) / 512), 512, [29, 29, 29], end=below(14)),
    # The nodes of fpminimax, as for the double precision square root
    "sinCoefficients": recipe(
        sin,
        256,
        [27, 27, 27],
        nodes=MINIMAX,
        count=None,
        end=below(10),
        precision=300,
        digits=120,
        evaluate_at="unrounded",
    ),
    "atanCoefficients": recipe(atan_over, 256, [29, 29, 29], end=below(18)),
    "atanRecipCoefficients": recipe(lambda j, t: 1 / (1 + (j + t) / 256), 256, [27, 27, 27], end=below(15)),
    # The last three: 169 bits (50 digits) of working precision
    "acosCoefficients": recipe(lambda j, t: mp.acos((j + t) / 256), 128, [28, 28, 28], end=below(15), precision=169),
    "asinCoefficients": recipe(asin_over, 129, [28, 28, 28], end=below(18), precision=169),
    "asinLargeCoefficients": recipe(
        lambda j, t: 2 * mp.asin((j + t) / 256), 128, [28, 28, 28], end=below(17), precision=169
    ),
}

DOCS = {
    "sqrtCoefficients": """\
{- | Square root: 256 segments, index @p * 128 + j@, with @p@ the parity of the
unbiased exponent and @j@ the top 7 fraction bits. Segment @s@ approximates
@sqrt ((1 + (j + t) / 128) * 2^p)@ for @t@ in @[0, 1)@ by
@C0 + C1 * u + C2 * u^2@ with @u = t / 128@. Units: for @p = 0@, C2 in 2^-13,
C1 in 2^-20 and C0 in 2^-27; for @p = 1@ one bit coarser (2^-12, 2^-19,
2^-26). Absolute error.
-}""",
    "rsqrtCoefficients": """\
{- | Reciprocal square root: 512 segments, index @p * 256 + j@, with @p@ = 1
for an even biased exponent and @j@ the top 8 fraction bits. Segment @s@
approximates @2 / sqrt ((1 + (j + t) / 256) * 2^p)@ for @t@ in @[0, 1)@ by
@C0 + C1 * u + C2 * u^2@ with @u = t / 256@. Units: C2 in 2^-11, C1 in 2^-19 and
C0 in 2^-27; for @p = 0@ the coefficients were generated on a grid one bit
coarser (2^-10, 2^-18, 2^-26), so they are all even. Absolute error.
-}""",
    "expCoefficients": """\
{- | Exponential: 89 segments, index @j@. Segment @j@ approximates
@e^((j + t) / 128)@ for @t@ in @[0, 1)@ by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-26, stored as @(2 c2, 2 c1, 2 c0)@ in units
of 2^-27. Relative error.
-}""",
    "exp2Coefficients": """\
{- | Base-2 exponential: 128 segments, index @j@. Segment @j@ approximates
@2^((j + t) / 128)@ for @t@ in @[0, 1)@ by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-26, stored as @(2 c2, 2 c1, 2 c0)@ in units
of 2^-27. Relative error.
-}""",
    "exp10Coefficients": """\
{- | Base-10 exponential: 155 segments, index @j@ (the reduced argument is
below log10 2, and log10 2 < 155 / 512). Segment @j@ approximates
@10^((j + t) / 512)@ for @t@ in @[0, 1)@ by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-26, stored as @(2 c2, 2 c1, 2 c0)@ in units
of 2^-27. Relative error.
-}""",
    "logCoefficients": """\
{- | Logarithm: 512 segments in four regions of 128, index @128 * r + j@ with
@j@ the fraction bits 21 to 15 of the input and @t@ in @[0, 1 - 2^-15]@ the
15 fraction bits below them:

  * r = 0: @z = (j + t) / 256@, x in [1, 1.5)
  * r = 1: @z = 1/2 + (j + t) / 256@, x in [1.5, 2)
  * r = 2: @z = -1/2 + (j + t) / 512@, x in [0.5, 0.75)
  * r = 3: @z = -1/4 + (j + t) / 512@, x in [0.75, 1)

Segment @s@ approximates @ln (1 + z) / z@ by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-29 (r = 0, 1) or 2^-28 (r = 2, 3), stored
as @(2 c2, 2 c1, 2 c0)@ in units of 2^-30 (r = 0, 1) or 2^-29 (r = 2, 3).
Absolute error.
-}""",
    "log2Coefficients": """\
{- | Base 2 logarithm: 512 segments in the four regions of 'logCoefficients'.
Segment @s@ approximates @log2 (1 + z) / z@ by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-27, stored as @(2 c2, 2 c1, 2 c0)@ in units
of 2^-28. Absolute error.
-}""",
    "log10Coefficients": """\
{- | Base 10 logarithm: 512 segments in the four regions of 'logCoefficients'.
Segment @s@ approximates @log10 (1 + z) / z@ by @c0 + c1 * t + c2 * t^2@ with
all three coefficients on the grid 2^-29, stored as @(2 c2, 2 c1, 2 c0)@ in
units of 2^-30. Absolute error.
-}""",
    "cbrtCoefficients": """\
{- | Cube root: 384 segments, index @r * 128 + j@, with @r@ the unbiased
exponent modulo 3 and @j@ the top 7 fraction bits. Segment @s@ approximates
@cbrt ((1 + (j + t) / 128) * 2^r)@ for @t@ in @[0, 1)@ by
@C0 + C1 * u + C2 * u^2@ with @u = t / 128@. Units: C2 in 2^-13, C1 in 2^-20
and C0 in 2^-27 (all three on the grid 2^-27 in @t@). Absolute error, with the
Chebyshev nodes on @[0, 1 - 2^-16]@.
-}""",
    "sinPiCoefficients": """\
{- | Sine of pi times x: 128 segments, index @j@. Segment @j@ approximates
@sin (pi * r) / r@ for @r = (j + t) / 256@, @t@ in @[0, 1)@, by
@c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-26.
Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-16]@.
-}""",
    "tanPiRecipCoefficients": """\
{- | Reciprocal for the tangent of pi times x: 512 segments, index @j@. Segment
@j@ approximates @1 / (1 + (j + t) / 512)@ for @t@ in @[0, 1)@ by
@c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-29.
Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-14]@.
-}""",
    "sinCoefficients": """\
{- | Sine, cosine and tangent (radians): 256 segments, index @j@. Segment @j@
approximates @sin (pi * r) / r@ for @r = (j + t) / 512@, @t@ in @[0, 1)@, by
@c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-27.
Absolute error, with the nodes of fpminimax: the three points of
@[0, 1 - 2^-10]@ where the minimax quadratic (Remez) equals the function.
-}""",
    "atanCoefficients": """\
{- | Arctangent: 256 segments, index @j@. Segment @j@ approximates
@atan r / r@ for @r = (j + t) / 256@, @t@ in @[0, 1)@, by
@c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-29.
Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-18]@.
-}""",
    "atanRecipCoefficients": """\
{- | Reciprocal for the arctangent of numbers above one: 256 segments, index
@j@. Segment @j@ approximates @1 / (1 + (j + t) / 256)@ for @t@ in @[0, 1)@
by @c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-27.
Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-15]@.
-}""",
    "acosCoefficients": """\
{- | Arccosine of numbers below one half: 128 segments, index @j@. Segment
@j@ approximates @acos r@ for @r = (j + t) / 256@, @t@ in @[0, 1)@, by
@c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid 2^-28.
Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-15]@.
-}""",
    "asinCoefficients": """\
{- | @asin r / r@, for the arcsine of numbers up to one half and the
arccosine of numbers of at least one half: 129 segments (the last one only for
@r = 1 / 2@), index @j@. Segment @j@ approximates @asin r / r@ for
@r = (j + t) / 256@, @t@ in @[0, 1)@, by @c0 + c1 * t + c2 * t^2@ with all
three coefficients on the grid 2^-28. Absolute error, with the Chebyshev nodes
on @[0, 1 - 2^-18]@.
-}""",
    "asinLargeCoefficients": """\
{- | @2 asin z@, for the arcsine of numbers above one half: 128 segments,
index @j@. Segment @j@ approximates @2 asin z@ for @z = (j + t) / 256@, @t@ in
@[0, 1)@, by @c0 + c1 * t + c2 * t^2@ with all three coefficients on the grid
2^-28. Absolute error, with the Chebyshev nodes on @[0, 1 - 2^-17]@.
-}""",
}

HEADER = r"""{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Polynomial coefficients for the bit-exact models of the square root,
reciprocal square root, cube root, exponentials, logarithms, sine and
tangent of pi times x, sine, arctangent, arccosine and arcsine of Intel's
Floating Point Functions IP, see
"Clash.Cores.Intel.Floating.Internal.BitExact".

This module is generated by
@tools\/intel-floating-coefficients\/gen_coefficients.py@; don't edit it
by hand.

Every table approximates its function on each segment by a polynomial of
degree two in a local variable @t@ in @[0, 1)@. The integer coefficients of
each segment were computed from first principles, with the lattice-reduction
method of Brisebarre and Chevillard ("Efficient polynomial L-infinity
approximations", ARITH 18, 2007), which is the method behind Sollya's
@fpminimax@:

  1. Take the 4 Chebyshev nodes of the first kind on the segment: on @[0, 1]@
     for @t@, except for the logarithms, the cube root, the sine of pi times
     x, the reciprocal of the tangent of pi times x, the arctangent and its
     reciprocal, the arccosine, @asin r / r@ and @2 asin z@, where they lie
     on @[0, 1 - 2^-15]@, @[0, 1 - 2^-16]@, @[0, 1 - 2^-16]@,
     @[0, 1 - 2^-14]@, @[0, 1 - 2^-18]@, @[0, 1 - 2^-15]@, @[0, 1 - 2^-15]@,
     @[0, 1 - 2^-18]@ and @[0, 1 - 2^-17]@: the range of @t@ that occurs (for
     @asin r / r@, the range of the arcsine of numbers below one half, which
     the arccosine's table shares).
  2. The lattice is spanned by the monomials @1@, @t@ and @t^2@, each scaled by
     the weight of its coefficient's least significant bit and evaluated at
     the nodes; the target is the function at the nodes.
  3. Reduce the lattice with LLL (delta = 0.99) and find a close lattice vector
     with Babai's nearest plane algorithm. Its coordinates are the
     coefficients.

The error is absolute for the square root, the reciprocal square root, the cube
root, the logarithms, the sines, the reciprocals, the arctangent, the arccosine and the
arcsine, and
relative for the exponentials (the target and the basis vectors are divided by the function at
the node). Coefficient formats (the position of each coefficient's least
significant bit) were inferred from the IP's outputs and its resource reports;
the coefficients themselves are not taken from the IP and the module contains
no data taken from Intel's IP. The tables hold the plain polynomial
coefficients: the models add the rounding constants of the datapath themselves.

Each list holds @(C2, C1, C0)@ per segment, in table order.
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.Coefficients
  ( sqrtCoefficients
  , rsqrtCoefficients
  , expCoefficients
  , exp2Coefficients
  , exp10Coefficients
  , logCoefficients
  , log2Coefficients
  , log10Coefficients
  , cbrtCoefficients
  , sinPiCoefficients
  , tanPiRecipCoefficients
  , sinCoefficients
  , atanCoefficients
  , atanRecipCoefficients
  , acosCoefficients
  , asinCoefficients
  , asinLargeCoefficients
  ) where

import Prelude
"""


def render(tables):
    # Highest degree first
    blocks = [
        haskell_table(DOCS[name], name, "(Int, Int, Int)", [row[::-1] for row in rows]) for name, rows in tables.items()
    ]
    return HEADER + "\n\n" + "\n\n".join(blocks) + "\n"


if __name__ == "__main__":
    main(TABLES, render, __doc__)
