#!/usr/bin/env python3
"""Generate `Clash.Cores.Intel.Floating.Internal.CoefficientsHalf`: the polynomial coefficients of the half
precision bit-exact models of Intel's Floating Point Functions IP.

Usage (from the root of the repository):

  python3 tools/intel-floating-coefficients/gen_coefficients_half.py \\
    > src/Clash/Cores/Intel/Floating/Internal/CoefficientsHalf.hs

The tables are computed here, from first principles (see `recipes.py`); nothing is read from the IP.
"""

from fractions import Fraction

import mpmath as mp

from recipes import MINIMAX, Recipe, haskell_table, main


def sqrt(s, t):
    return mp.sqrt((1 + ((s & 15) + t) / 16) * mp.mpf(2) ** (s >> 4))


def rsqrt(s, t):
    return 2 / mp.sqrt((1 + ((s & 31) + t) / 32) * mp.mpf(2) ** (s >> 5))


def cbrt(s, t):
    return mp.cbrt((1 + ((s & 31) + t) / 32) * mp.mpf(2) ** (s >> 5))


def log_quotient(log):
    """`log (1 + z) / z` on the four regions of the logarithms, each of 32 segments"""

    def function(s, t):
        r, j = s >> 5, s & 31
        if r == 0:
            z = (j + t) / 64
        elif r == 1:
            z = mp.mpf(1) / 2 + (j + t) / 64
        elif r == 2:
            z = -mp.mpf(1) / 2 + (j + t) / 128
        else:
            z = -mp.mpf(1) / 4 + (j + t) / 128
        return log(1 + z) / z

    return function


def pow_log(s, t):
    """`ln (1 + z) / z`, with its limit where `z = 0`: at the start of segment 0 and the end of segment 127"""
    return mp.mpf(1) if (s, t) in ((0, 0), (127, 1)) else log_quotient(mp.log)(s, t)


def asin_over(r):
    """asin r / r"""
    return mp.asin(r) / r if r != 0 else mp.mpf(1)


def atan_over(r):
    """atan r / r"""
    return mp.atan(r) / r if r != 0 else mp.mpf(1)


def sinpi_over(r):
    """sin (pi * r) / r"""
    return mp.sin(mp.pi * r) / r if r != 0 else mp.pi


def recipe(function, segments, grids, count=None, **kwargs):
    """What all half precision tables share: absolute error, 150 bits of working precision, nodes and function
    values rounded to 45 digits, the function evaluated at the rounded nodes, LLL with delta = 0.99 on the
    basis in ascending order of degree. By default as many Chebyshev nodes as the degree plus one, on [0, 1].
    """
    return Recipe(
        function=function,
        segments=segments,
        grids=grids,
        count=count,
        precision=150,
        digits=45,
        evaluate_at="rounded",
        delta=Fraction(99, 100),
        order="ascending",
        error="absolute",
        **kwargs,
    )


# name: recipe (function of the segment and t, number of segments, grids of [c0, c1 (, c2)], Chebyshev nodes)
TABLES = {
    "sqrtHalfCoefficients": recipe(sqrt, 32, [13, 13], 2),
    "rsqrtHalfCoefficients": recipe(rsqrt, 64, [13, 13], 2),
    "expHalfCoefficients": recipe(lambda s, t: mp.exp((s + t) / 64), 45, [13, 13], 2),
    "exp2HalfCoefficients": recipe(lambda s, t: mp.power(2, (s + t) / 32), 32, [13, 13], 2),
    "exp10HalfCoefficients": recipe(lambda s, t: mp.power(10, (s + t) / 128), 39, [13, 13], 2),
    "logHalfCoefficients": recipe(log_quotient(mp.log), 128, [15, 15], 2),
    "log2HalfCoefficients": recipe(log_quotient(lambda v: mp.log(v, 2)), 128, [14, 14], 2),
    "log10HalfCoefficients": recipe(log_quotient(mp.log10), 128, [16, 16], 2),
    "cbrtHalfCoefficients": recipe(cbrt, 96, [14, 14], 2),
    "sinPiHalfCoefficients": recipe(lambda s, t: sinpi_over((s + t) / 128), 64, [13, 13], 2),
    "tanPiRecipHalfCoefficients": recipe(lambda s, t: 1 / (1 + (s + t) / 128), 128, [16, 16], 2),
    "asinHalfCoefficients": recipe(lambda s, t: asin_over((s + t) / 16), 9, [15, 15, 15], 4),
    "asinLargeHalfCoefficients": recipe(lambda s, t: 2 * mp.asin((s + t) / 128), 64, [15, 15], 2),
    "acosHalfCoefficients": recipe(lambda s, t: mp.acos((s + t) / 64), 32, [15, 15], 2),
    "atanHalfCoefficients": recipe(lambda s, t: atan_over((s + t) / 128), 128, [16, 16], 2),
    "atanRecipHalfCoefficients": recipe(lambda s, t: 1 / (1 + (s + t) / 64), 64, [14, 14], 2),
    "sinHalfCoefficients": recipe(lambda s, t: sinpi_over((s + t) / 256), 128, [14, 14], 2),
    # The nodes of fpminimax. Coefficients at 2^-24 where the function is below one, at 2^-23 otherwise.
    "powLogHalfCoefficients": recipe(pow_log, 128, lambda s: [24, 24, 24] if s < 64 else [23, 23, 23], nodes=MINIMAX),
    # The nodes on the range of t
    "powExpHalfCoefficients": recipe(
        lambda s, t: mp.exp((s + t) / 128), 89, [26, 26, 26], 4, end=1 - Fraction(1, 2**14)
    ),
}

DOCS = {
    "sqrtHalfCoefficients": """\
{- | Square root: 32 segments, index @p * 16 + j@, with @p@ the parity of the
unbiased exponent and @j@ the top 4 fraction bits. Segment @s@ approximates
@sqrt ((1 + (j + t) / 16) * 2^p)@ for @t@ in @[0, 1)@ by @c0 + c1 * t@, both
coefficients in units of 2^-13.
-}""",
    "rsqrtHalfCoefficients": """\
{- | Reciprocal square root: 64 segments, index @p * 32 + j@, with @p@ = 1 for
an even biased exponent and @j@ the top 5 fraction bits. Segment @s@
approximates @2 / sqrt ((1 + (j + t) / 32) * 2^p)@ for @t@ in @[0, 1)@ by
@c0 + c1 * t@, both coefficients in units of 2^-13.
-}""",
    "expHalfCoefficients": """\
{- | Exponential: 45 segments, index @j@ (the reduced argument is below ln 2,
and ln 2 < 45 / 64). Segment @j@ approximates @e^((j + t) / 64)@ for @t@ in
@[0, 1)@ by @c0 + c1 * t@, both coefficients in units of 2^-13.
-}""",
    "exp2HalfCoefficients": """\
{- | Base-2 exponential: 32 segments, index @j@. Segment @j@ approximates
@2^((j + t) / 32)@ for @t@ in @[0, 1)@ by @c0 + c1 * t@, both coefficients in
units of 2^-13.
-}""",
    "exp10HalfCoefficients": """\
{- | Base-10 exponential: 39 segments, index @j@ (the reduced argument is
below log10 2, and log10 2 < 39 / 128). Segment @j@ approximates
@10^((j + t) / 128)@ for @t@ in @[0, 1)@ by @c0 + c1 * t@, both coefficients
in units of 2^-13.
-}""",
    "logHalfCoefficients": """\
{- | Logarithm: 128 segments in four regions of 32, index @32 * r + j@ with
@j@ the fraction bits 8 to 4 of the input and @t@ in @[0, 1)@ the 4 fraction
bits below them (as a fraction of the segment):

  * r = 0: @z = (j + t) / 64@, x in [1, 1.5)
  * r = 1: @z = 1/2 + (j + t) / 64@, x in [1.5, 2)
  * r = 2: @z = -1/2 + (j + t) / 128@, x in [0.5, 0.75)
  * r = 3: @z = -1/4 + (j + t) / 128@, x in [0.75, 1)

Segment @s@ approximates @ln (1 + z) / z@ by @c0 + c1 * t@, both coefficients
in units of 2^-15.
-}""",
    "log2HalfCoefficients": """\
{- | Base 2 logarithm: 128 segments in the four regions of
'logHalfCoefficients'. Segment @s@ approximates @log2 (1 + z) / z@ by
@c0 + c1 * t@, both coefficients in units of 2^-14.
-}""",
    "log10HalfCoefficients": """\
{- | Base 10 logarithm: 128 segments in the four regions of
'logHalfCoefficients'. Segment @s@ approximates @log10 (1 + z) / z@ by
@c0 + c1 * t@, both coefficients in units of 2^-16.
-}""",
    "cbrtHalfCoefficients": """\
{- | Cube root: 96 segments, index @r * 32 + j@, with @r@ the unbiased
exponent modulo 3 and @j@ the top 5 fraction bits. Segment @s@ approximates
@cbrt ((1 + (j + t) / 32) * 2^r)@ for @t@ in @[0, 1)@ by @c0 + c1 * t@, both
coefficients in units of 2^-14.
-}""",
    "sinPiHalfCoefficients": """\
{- | Sine of pi times x: 64 segments, index @j@. Segment @j@ approximates
@sin (pi * r) / r@ for @r = (j + t) / 128@, @t@ in @[0, 1)@, by
@c0 + c1 * t@, both coefficients in units of 2^-13.
-}""",
    "asinHalfCoefficients": """\
{- | Arcsine of numbers up to one half: 9 segments, index @j@ (the top 4 bits
of |x| at 2^-13; segment 8 only for x = 1/2). Segment @j@ approximates
@asin r / r@ for @r = (j + t) / 16@, @t@ in @[0, 1)@, by
@c0 + c1 * t + c2 * t^2@, all three coefficients in units of 2^-15, with 4
Chebyshev nodes.
-}""",
    "asinLargeHalfCoefficients": """\
{- | Arcsine of numbers above one half: 64 segments, index @j@. Segment @j@
approximates @2 asin z@ for @z = (j + t) / 128@, @t@ in @[0, 1)@, by
@c0 + c1 * t@, both coefficients in units of 2^-15.
-}""",
    "acosHalfCoefficients": """\
{- | Arccosine of numbers below one half: 32 segments, index @j@ (the top 5
bits of |x| at 2^-10). Segment @j@ approximates @acos r@ for
@r = (j + t) / 64@, @t@ in @[0, 1)@, by @c0 + c1 * t@, both coefficients in
units of 2^-15.
-}""",
    "powLogHalfCoefficients": """\
{- | Logarithm of the power function: 128 segments, index @r * 32 + j@ for the
regions @r@ of 'logHalfCoefficients'. Segment @s@ approximates
@ln (1 + z) / z@ for @t@ in @[0, 1)@ by @c0 + c1 * t + c2 * t^2@, all three
coefficients in units of 2^-24 for the regions 0 and 1 (where it is below
one) and 2^-23 for the regions 2 and 3. Unlike the other tables, the nodes
are those of fpminimax: the three points of @[0, 1]@ where the minimax
polynomial of degree two equals the function.
-}""",
    "powExpHalfCoefficients": """\
{- | Exponential of the power function: 89 segments, index @j@ (the reduced
argument is below ln 2, which is in segment 88). Segment @j@ approximates
@exp ((j + t) / 128)@ for @t@ in @[0, 1)@ by @c0 + c1 * t + c2 * t^2@, all
three coefficients in units of 2^-26, with 4 Chebyshev nodes on
@[0, 1 - 2^-14]@, the range of @t@.
-}""",
    "sinHalfCoefficients": """\
{- | Sine and cosine of x in radians: 128 segments, index @j@. Segment @j@
approximates @sin (pi * r) / r@ for @r = (j + t) / 256@, @t@ in @[0, 1)@, by
@c0 + c1 * t@, both coefficients in units of 2^-14.
-}""",
    "atanHalfCoefficients": """\
{- | Arctangent: 128 segments, index @j@ (the top 7 bits of the argument at
2^-13). Segment @j@ approximates @atan r / r@ for @r = (j + t) / 128@, @t@ in
@[0, 1)@, by @c0 + c1 * t@, both coefficients in units of 2^-16.
-}""",
    "atanRecipHalfCoefficients": """\
{- | Reciprocal for the arctangent of numbers above one: 64 segments, index
@j@ (the top 6 fraction bits). Segment @j@ approximates
@1 / (1 + (j + t) / 64)@ for @t@ in @[0, 1)@ by @c0 + c1 * t@, both
coefficients in units of 2^-14.
-}""",
    "tanPiRecipHalfCoefficients": """\
{- | Reciprocal for the tangent of pi times x: 128 segments, index @j@.
Segment @j@ approximates @1 / (1 + (j + t) / 128)@ for @t@ in @[0, 1)@ by
@c0 + c1 * t@, both coefficients in units of 2^-16.
-}""",
}

HEADER = r"""{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Polynomial coefficients for the half precision bit-exact models of Intel's
Floating Point Functions IP, see
"Clash.Cores.Intel.Floating.Internal.BitExactHalf".

This module is generated by
@tools\/intel-floating-coefficients\/gen_coefficients_half.py@; don't edit it
by hand.

In half precision the IP approximates its functions on each segment by a
polynomial of degree one in a local variable @t@ in @[0, 1)@ (degree two for
the arcsine of numbers up to one half, 'asinHalfCoefficients', which the
arccosine of numbers of at least one half uses too, and for the two tables of
the power function). The integer
coefficients of each segment were computed from first principles, with the
lattice-reduction method of Brisebarre and Chevillard (\"Efficient polynomial
L-infinity approximations\", ARITH 18, 2007), which is the method behind
Sollya's @fpminimax@, as for the single precision tables in
"Clash.Cores.Intel.Floating.Internal.Coefficients":

  1. Take the 2 Chebyshev nodes of the first kind on @[0, 1]@ (4 for degree
     two; the tables of the power function differ, see
     'powLogHalfCoefficients' and 'powExpHalfCoefficients').
  2. The lattice is spanned by the monomials @1@ and @t@ (and @t^2@), each
     scaled by the weight of its coefficient's least significant bit and
     evaluated at the nodes; the target is the function at the nodes.
  3. Reduce the lattice with LLL (delta = 0.99) and find a close lattice vector
     with Babai's nearest plane algorithm. Its coordinates are the
     coefficients.

The error is absolute. Coefficient formats (the position of each
coefficient's least significant bit) were inferred from the IP's outputs and
its resource reports; the coefficients themselves are not taken from the IP
and the module contains no data taken from Intel's IP. The tables hold the
plain polynomial coefficients: the models add the rounding constants of the
datapath themselves.

Each list holds @(C1, C0)@ (or @(C2, C1, C0)@) per segment, in table order.
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.CoefficientsHalf
  ( sqrtHalfCoefficients
  , rsqrtHalfCoefficients
  , expHalfCoefficients
  , exp2HalfCoefficients
  , exp10HalfCoefficients
  , logHalfCoefficients
  , log2HalfCoefficients
  , log10HalfCoefficients
  , cbrtHalfCoefficients
  , sinPiHalfCoefficients
  , tanPiRecipHalfCoefficients
  , asinHalfCoefficients
  , asinLargeHalfCoefficients
  , acosHalfCoefficients
  , atanHalfCoefficients
  , atanRecipHalfCoefficients
  , sinHalfCoefficients
  , powLogHalfCoefficients
  , powExpHalfCoefficients
  ) where

import Prelude
"""


def render(tables):
    blocks = []
    for name, rows in tables.items():
        element_type = f"({', '.join(['Int'] * len(rows[0]))})"
        # Highest degree first
        blocks.append(haskell_table(DOCS[name], name, element_type, [row[::-1] for row in rows]))
    return HEADER + "\n\n" + "\n\n".join(blocks) + "\n"


if __name__ == "__main__":
    main(TABLES, render, __doc__)
