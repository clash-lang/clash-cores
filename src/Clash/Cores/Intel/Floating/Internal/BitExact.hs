{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of the square root, cube root, three-dimensional
hypotenuse, reciprocal square root, reciprocal, exponentials and logarithms
(natural, base 2, base 10 and @log (1 + x)@), the sine, cosine and
tangent of x and of pi times x, the arctangent (in radians and divided by
pi) the two-argument arctangent and the power function of Intel's Floating Point Functions IP
(@altera_fp_functions@) in
single precision, see Note [Bit-exact models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExact
  ( Multipliers(..)
  , sqrtSingle
  , cbrtSingle
  , hypotSingle
  , rsqrtSingle
  , recipSingle
  , expSingle
  , exp2Single
  , exp10Single
  , logSingle
  , log2Single
  , log10Single
  , log1pSingle
  , sinPiSingle
  , cosPiSingle
  , tanPiSingle
  , sinSingle
  , cosSingle
  , tanSingle
  , atanSingle
  , atanPiSingle
  , atan2Single
  , powSingle
  ) where

import Prelude

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word32, Word64)

import Clash.Cores.Intel.Floating.Internal.CoefficientsPow (powExpCoefficients, powLogCoefficients)
import Clash.Cores.Intel.Floating.Internal.Coefficients
  ( cbrtCoefficients, exp10Coefficients, exp2Coefficients, expCoefficients, log10Coefficients
  , log2Coefficients, logCoefficients, rsqrtCoefficients, sqrtCoefficients, sinPiCoefficients
  , tanPiRecipCoefficients, sinCoefficients, atanCoefficients, atanRecipCoefficients )

{- Note [Bit-exact models]
The Floating Point Functions user guide documents the latency and the
accuracy of the IP's functions, but not how it computes them. The models in
this module give the IP's results bit for bit. Their structure (table sizes,
the position of every coefficient's least significant bit, the widths and
rounding of intermediate results and how the multipliers are cut) was
inferred by simulating the generated IP as a black box: choosing inputs and
observing outputs, and from Quartus's ordinary resource reports (RAM sizes and
multiplier sizes of a synthesis). The polynomial coefficients are computed from
first principles, see "Clash.Cores.Intel.Floating.Internal.Coefficients"; no
data was taken from the IP.

The square root, the cube root, the reciprocal square root, the exponentials
and the logarithms use a piecewise polynomial of degree two, evaluated in
Horner form:

  T1 = C2 * y'      (y' = the top bits of y)
  S1 = C1 + T1
  A  = C0 + S1 * y

where the top bits of the reduced argument select the segment (the table
index) and y holds the bits below them. The IP has two datapaths for this,
selected by the device family ('Multipliers'):

  * 'FullMultipliers' (Cyclone V, with 27x27 multipliers): T1 is rounded down,
    S1 * y is exact or rounded down.
  * 'TruncatedMultipliers' (MAX 10, Cyclone IV E, Cyclone 10 LP, with 18x18
    multipliers): T1 is rounded half up, and S1 * y is a truncated multiplier
    that leaves out the partial products below a "staircase" of tiles,
    compensated by a constant. For the square root, the cube root, the
    reciprocal square root, the base-2 exponential and the natural and base
    10 logarithms the kept partial products S1_i * y_j are: all i for
    j >= j0 + 8, i >= 3 for the 3 rows below that, i >= 6 for the rest, with
    j0 = 1 for the square root, the cube root and the base-2 exponential and 0
    otherwise (a 17x17 signed
    multiplier plus an 8x9 and a 3x3 one, as in the synthesis report). The base 2 logarithm keeps all i for j >= 8, i >= 3 for j = 6
    and 7 and i >= 5 for the rest (17x17, 8x9 and 2x2). The exponential
    leaves out S1_i * y_j for i < 2, j < 12; 2 <= i < 7, j < 7 and
    7 <= i < 11, j < 3, the base-10 exponential for i < 2, j < 11;
    2 <= i < 6, j < 7 and 6 <= i < 11, j < 2.

The results do not depend on the latency or the target frequency, only on the
device family. The full-multiplier datapath was solved first, on a MAX 10
build of the IP generated with full multipliers (an option of Intel's
generator), then the truncated one, and both were confirmed on the other
families.

How the structure was inferred:

  * The RAM sizes in the synthesis report give the table sizes and the widths
    of C0, C1 and C2; the report also gives the sizes of the multipliers.
  * For a fixed structure, the output on all inputs of a segment pins down its
    coefficients: every output gives an interval for A, which gives linear
    constraints on (C2, C1, C0). A grid search over the formats (the LSBs of
    the coefficients, the bits of y used for T1, the LSB of S1) left exactly
    one format with a feasible, unique set of coefficients for every segment.
  * With the full multipliers solved, the difference between the outputs of
    the two datapaths for the same coefficients gave the rounding of T1 (a
    sawtooth in C2 * y' mod the LSB of T1 when rounded down) and the truncated
    multiplier: a fit over the partial products S1_i * y_j gave a crisp 0/1
    pattern, refined by coordinate descent over per-row cuts, and the
    constant was scanned.
  * The coefficients the fits produced were then reproduced exactly by the
    lattice method described in
    "Clash.Cores.Intel.Floating.Internal.Coefficients", which the models now
    use.

Square root (exponent e = E - 127, p = e mod 2, x = m * 2^e with m in
[1, 2)): the result is sqrt (m * 2^p) * 2^floor(e/2). The table index is
p * 128 + the top 7 fraction bits, y the low 16 fraction bits. The
significand O comes out in units of 2^-23 in [1, 2]; O = 2 (from rounding)
carries into the exponent, which the model gets by adding O - 1 to the result's
exponent field rather than or-ing. C0 gets a rounding constant of 4 units: a
quarter unit in the last place for p = 0 and a half for p = 1. Zero and
subnormal inputs give a zero with the same sign; negative numbers (including
-inf) and NaN give NaN; +inf gives +inf.

Cube root (x = m * 2^(3k + r) with m in [1, 2) and r in {0, 1, 2}): the
result is cbrt (m * 2^r) * 2^k, with the datapath of the square root for
p = 0 (the same multipliers, coefficient formats and rounding constant, a
quarter unit in the last place) on its own 384-entry table, indexed by
r * 128 + the top 7 fraction bits. Zero and subnormal inputs give a zero of
the same sign, infinities give themselves and negative numbers the negated
cube root of their magnitude. Unlike all other functions, NaN keeps its sign:
the result is the sign of the input with 0x7F800001. The table sizes (384
entries of 29, 21 and 12 bits, plus two 255-entry tables for the quotient and
remainder of the exponent divided by 3) and the multipliers (identical to the
square root's) suggested the structure; the format search over all inputs in
[1, 8) found the coefficient formats of the square root's p = 0, with unique
coefficients for all 384 segments, equal to the ones computed from first
principles; the square root's truncated multiplier then gave the IP's results
on all of those inputs without change.

Three-dimensional hypotenuse, sqrt (x^2 + y^2 + z^2):

  1. Every input with a nonzero exponent field is squared exactly (24 x 24
     bits), and the square becomes a floating point number with its
     significand truncated to 24 bits. Zero and subnormal inputs count as zero.
  2. The two smaller squares are aligned to the largest: shifted right by the
     difference of the exponents and truncated at 2^-25 relative to the
     largest one's leading bit. A square shifted by more than 24 places is
     dropped (even though shifting by 25 places would leave its leading bit).
     The sum of the three is exact.
  3. The sum is rounded to nearest at 24 bits, ties up.
  4. Its square root is computed by the datapath of the square root, with the
     same tables, but with an extra half unit in the last place added before
     the truncation of the significand: the rounding constants are three
     quarters of a unit for an even exponent and a whole unit for an odd one.
  5. Results of 2^128 or more overflow to +inf; the result can't underflow.
     Any NaN input gives NaN, otherwise any infinite input gives +inf. The
     signs of the inputs are ignored and the result is never negative.

The IP's tables and multipliers (the synthesis report shows the square root's
tables and polynomial multipliers, and three full 24 x 24 multipliers)
suggested the structure. Single inputs (x, 0, 0) matched the square root of
the truncated square with a larger rounding constant; inputs whose sums are
exact (mantissas of a few bits) pinned the constant down to an extra half
unit in the last place for both parities; all of x in [1, 2) with y = 1
showed that the sum is rounded to nearest after truncating the squares; two
inputs at all exponent differences up to 30, and three inputs at pairs of
exponent differences in all six positions, gave the alignment (symmetric in
the positions, truncation at 2^-25 except that a shift of exactly 25 places
drops the square).

Reciprocal square root: index p * 256 + top 8 fraction bits (p = 1 for an even
biased exponent E), y the low 15 fraction bits; the result is
f(m) / 2 * 2^(-(E - 127 - p) / 2) with f(m) = 2 / sqrt (m * 2^p) in (1, 2], so
its biased exponent is floor ((380 - E) / 2), plus the carry of O = 2.
Rounding constant: 8 units of 2^-27 for p = 0, 4 for p = 1. Zero and
subnormal inputs give an infinity of the same sign; negative numbers and NaN
give NaN; +inf gives +0.

Exponential, following de Dinechin and Pasca ("Floating-point exponential
functions for DSP-enabled FPGAs", FPT 2010):

  1. X = floor (x * 2^24) in two's complement; zero and subnormal inputs keep
     their hidden bit (so e^-0 is 1 - 2^-24, 0x3F7FFFFF). |x| >= 512 overflows
     (+inf) or underflows (+0).
  2. k = floor (x / ln 2), estimated from the significand times 1/ln 2
     (floor (2^34 / ln 2) / 2^34), truncated to 27 bits relative to the
     significand. For negative x just below k * ln 2, the estimate can be one
     too high; then r below is negative and is clamped to 0.
  3. r = x - k * ln 2 in units of 2^-26, with k * ln 2 computed from
     ln 2 rounded down to 34 fraction bits, rounded down to 33 fraction bits
     and then up to 26.
  4. e^r with an 89-entry table indexed by the top 7 bits of r (r * 128 is
     below 89), y the 19 bits below them; the result's significand is
     truncated to 2^-23, with a rounding constant of 2^-24 in C0.
  5. A significand of 2 or more is normalised; a biased exponent of 255 or
     more gives +inf, of 0 or less +0 (no subnormal results). NaN gives NaN,
     +inf gives +inf and -inf gives +0.

Base-2 exponential: the exponential's structure without the range reduction.

  1. X = floor (x * 2^23) in two's complement, with the hidden bit kept for
     zeros and subnormal numbers (so 2^-0 is 0x3F7FFFFF too). |x| >= 512
     overflows (+inf) or underflows (+0).
  2. k = X >> 23 is the result's exponent; 2^f for the 23 fraction bits f of
     X uses a 128-entry table indexed by the top 7 bits of f, y the 16 bits
     below them, with the multipliers of the square root (T1 from the top 12
     bits of y; on 18x18 families the same staircase with j0 = 1, compensation
     constant 2^-29). The result's significand is truncated to 2^-23 with a
     rounding constant of 2^-24 in C0.
  3. Overflow, underflow and special inputs as for the exponential.

Base-10 exponential: 10^x = 2^k * 10^r with r = x - k * log10 2.

  1. X = floor (x * 2^24) as for the exponential.
  2. k = floor (x * log2 10), estimated from the significand times log2 10
     rounded to nearest at 26 fraction bits, truncated to 26 bits relative to
     the significand. The estimate can be one too high for inputs (of either
     sign) just below k * log10 2; r is then clamped to 0.
  3. r = x - k * log10 2 in units of 2^-27, with k * log10 2 computed from
     log10 2 rounded to nearest at 34 fraction bits, rounded up to 27.
  4. 10^r with a 155-entry table (in a 256-entry memory) indexed by the top 8
     bits of r (r * 512 is below 155), y the 18 bits below them. T1 is C2
     times the top 13 bits of y at 2^-27; on 18x18 families the truncated
     multiplier leaves out S1_i * y_j for i < 2, j < 11; 2 <= i < 6, j < 7 and
     6 <= i < 11, j < 2 (a 17x17, an 8x9, a 9x8 and two 4x4 multipliers, as in
     the synthesis report), compensated by 2^-29. The rounding constant in C0
     is a whole unit in the last place, 2^-23, so 10^0 is 1 + 2^-23
     (0x3F800001) and 10^-0 is 0x3F7FFFFE.
  5. Overflow, underflow and special inputs as for the exponential.

The structure of the base-10 exponential was found as follows: the outputs
depend on floor (x * 2^24) only (aligned groups of inputs give equal outputs);
the mean error is periodic in x - k * log10 2 with a period of 1/512; on the
inputs with k = 0 the grid search found T1's input width and LSB, with
unique coefficients for all 155 segments, identical to the ones generated
from first principles; per k, exactly one value of k * log10 2 at 2^-27
made all outputs match, which pins log10 2 down to a small interval (rounding
it up to 33 to 38 fraction bits gives the same results); the inputs where the
estimate of k is one too high select the estimator among the ones giving the
same results on all inputs; and the truncated multiplier was fitted by
coordinate descent over per-row cuts of the partial products, starting from
the exponential's.

Natural logarithm: for x = m * 2^E in [0.5, 2) (E is -1 or 0) the IP
computes ln x = z * P(x) with z = x - 1 exact and P approximating
ln(x) / (x - 1), with four regions of 128 segments each (see 'logCoefficients').
P is truncated to 24 bits (Aq). Elsewhere it computes E * ln 2 + z * Aq, with
ln 2 rounded to 21 fraction bits and z = m - 1 (the tables of [1, 2)). The
result is computed exactly and rounded once to nearest, ties away from zero.
C0 gets a rounding constant of 8 units (2^-27 for P < 1, 2^-26 for P >= 1).
Zero and subnormal inputs give -inf; negative numbers, -inf and NaN give NaN;
+inf gives +inf and 1 gives +0.

The base 2 and base 10 logarithms have the same structure as the natural
logarithm (the same regions and segments, and their own tables of
log_b(1 + z) / z, see 'log2Coefficients' and 'log10Coefficients'), with
these differences, found by the same format search on all inputs in
[0.5, 2) and by checking the constants on all inputs in [0.25, 0.5) and
[2, 4):

  * Base 2: P is in [1, 2] in all four regions. The coefficients are on the
    grid 2^-27 (one bit coarser than the natural logarithm's for P < 1, as the
    narrower tables in the synthesis report suggest), T1 uses the top 12 bits
    of y, the rounding constant is 2^-25 and P is truncated to 2^-24 (25 bits).
    Outside [0.5, 1) the result is E + z * P, computed exactly: powers of two
    give exact results.
  * Base 10: P is in [0.30, 0.61], the coefficients are on the grid 2^-29 with
    a rounding constant of 2^-27. For x in [0.5, 1) P is truncated to 2^-25;
    elsewhere (P < 0.5) the polynomial is evaluated one bit finer (as if the
    coefficients were doubled), and P truncated to 2^-26. Elsewhere the
    result is E * log10 2 + z * P, with log10 2 rounded to 30 fraction bits.

In both, the sum is computed exactly and rounded once to nearest, ties away
from zero, and the special cases are those of the natural logarithm.

ln (1 + x) uses the tables of the natural logarithm, with P truncated to 25
bits instead of 24 (a rounding constant of 8 units, as there). With
v = 1 + x:

  * For v in [0.5, 2) (x in (-0.5, 1)) the result is x * P(v), with x exact:
    the segment and y come from v rounded down to 2^-23 (v >= 1) or 2^-24
    (v < 1), the regions of the natural logarithm of v. So there is no
    cancellation for small x, and for |x| < 2^-24 the result is x.
  * Otherwise v is truncated to its 24 leading bits, v = (1 + g * 2^-23) * 2^E,
    and the result is E * ln 2 + g * 2^-23 * P(v) as for the natural
    logarithm.

The result is rounded once to nearest, ties away from zero. Zero and subnormal
inputs give a zero of the same sign, -1 gives -inf, x < -1, -inf and NaN give
NaN and +inf gives +inf. This was found by testing the hypothesis "the natural
logarithm's datapath applied to 1 + x" against the IP, and varying where P is
truncated and how v is rounded: only this combination matched.

The reciprocal is correctly rounded (round to nearest, ties to even) and
flushes subnormal inputs to zero, with a quirk at the bottom of the exponent
range: the result is computed with an unbounded exponent, and a result whose
biased exponent would be 0 keeps its 23 fraction bits with an exponent field
of 0. Note that this is not the subnormal number the bit pattern would encode:
the result has the significand of a normal number. Results below that flush to
a zero of the same sign. 1/+-0 is +-inf, 1/+-inf is +-0 and NaN gives NaN.

Sine and cosine of pi times x (the IP's \"SIN\" and \"COS\" with the angle
represented as a multiple of pi). The IP computes sin (pi * r) = r * P(r), with
r in [0, 1/2] the distance of x to the nearest integer (sine) or half integer
(cosine), and P a piecewise polynomial approximation of sin (pi * r) / r, so
that small results keep their relative accuracy:

  1. X = floor (|x| * 2^24) mod 2^25 for the sine; q = X / 2^23 is the half
     period and r = X mod 2^23 (q even) or 2^23 - X mod 2^23 (q odd), in units
     of 2^-24. The result is negative if q >= 2, for the sign of x. For the
     cosine, the same with r = 2^23 - X mod 2^23 for q even, r = X mod 2^23 for
     q odd, and a negative result for q = 1, 2, computed at a finer
     resolution (2^-37): the result is exact, apart from the polynomial.
  2. P: a 128-entry table indexed by the top 7 bits of r (r = 1/2 is segment
     127 with y = 2^16), y the 16 bits below them, T1 from the top 14 bits of
     y, all at 2^-27, truncated to 2^-23 with a rounding constant of 2^-24 in
     C0. On 18x18 families T1 is rounded half up and S1 * y is the staircase
     of the square root (j0 = 1), compensated by 2^-29.
  3. The sine's result is m * P rounded to nearest, ties away from zero, with
     m the significand of x for |x| < 2^-12 (and P = P(0) = pi rounded down to
     2^-23), the significand of x without its last bit for |x| in
     [2^-12, 1/2), and r otherwise. The cosine's is r * P, with r truncated to
     24 significant bits; on 18x18 families this product is a truncated
     multiplier of r and P as 24- and 25-bit integers (an 18x18, two 9x9 and
     two 2x2 tiles, see 'cosPiProduct'), while it is exact for the sine on all
     families.
  4. Integers (sine) and half integers (cosine) give a zero: with the sign of
     x for the sine, positive for the cosine. The sine of a zero, subnormal or
     |x| >= 2^23 is a zero with the sign of x; the cosine of |x| < 2^-13 is
     one. The sine of an infinity is NaN with the sign of x, of another NaN
     NaN with the opposite sign; the cosine gives the positive NaN.

This was found as follows: the outputs ignore the last input bit exactly for
|x| in [2^-12, 1/2) (aligned pairs give equal outputs), and every output gives
an interval for P; on the sine's inputs below 1/2, where many significands share
an r, those intervals all contain a point of the 2^-23 grid (and not of 2^-22).
The format search over all r found T1's input width and LSB with unique
coefficients for all 128 segments; the half period above 1/2 fixed how r = 1/2
is handled. The coefficients are those of the first-principles method with the
Chebyshev nodes on [0, 1 - 2^-16] (see 'sinPiCoefficients'). The cosine has the
same table (its outputs match the sine's polynomial on all inputs) and its
fixed-point resolution, the tiny-input threshold and its truncated multiplier
(fitted by coordinate descent over per-row cuts of the partial products, as in
the synthesis report) were found on random and swept inputs.

Tangent of pi times x (the IP's \"TAN\" with the angle represented as a
multiple of pi): the IP divides its sine of pi times x by its cosine of pi
times x, both rounded to single precision as above, by multiplying the sine's
significand by R, a polynomial approximation of the reciprocal of the cosine's
significand 1 + f * 2^-23: a 512-entry table indexed by the top 9 bits of f, y
the 14 bits below them, T1 from the top 12 bits of y, the coefficients at
2^-29 and T1, S1 and A at 2^-30, R truncated to 2^-26 with a rounding constant
of 2^-27 in C0. On 18x18 families T1 is rounded half up, S1 * y is the
staircase with j0 = -1 compensated by 2^-32, and the final product is the
truncated multiplier of the cosine (an 18x18, two 9x9 and two 2x2 tiles; any
compensation constant up to 2^13 gives the same results on all inputs). A
cosine of a power of two (f = 0) gives R = 1 and an exact product. The result
is rounded to nearest, ties away from zero. |x| < 2^-13 gives x times pi
rounded to single precision, rounded; zero and subnormal inputs are returned
unchanged; integers give a zero and half integers an infinity, with the sign
of the sine times the cosine; NaN and the infinities give NaN.

This was found as follows: the outputs match the IP's sine and cosine rounded
to single precision, divided with a reciprocal rounded to 26 bits, on all but
0.7% of the inputs in [0.25, 1); the format search on the intervals of the
reciprocal per cosine significand found the formats with unique coefficients
for all 512 segments; with the truncated product of the cosine, the
reciprocal of the 18x18 families was fitted by coordinate descent over
per-row cuts of the partial products.

Sine, cosine and tangent of x in radians (the IP's \"SIN\", \"COS\" and
\"TAN\"). The IP reduces x with a window of the bits of 1 / pi and then
computes the sine of pi times the reduced argument as above, with a finer
table:

  1. z = |x| / pi modulo 2 is computed with 77 fraction bits as m * W modulo
     2^78, with m the 24-bit significand of x and W = floor (2^(e - 73) / pi)
     for its biased exponent e (of which only the low 78 bits matter), and
     then truncated to 54 fraction bits for the sine and 55 for the cosine.
     q, the half period of z, and r in [0, 1/2], the distance to the nearest
     integer (sine) or half integer (cosine), follow as for the sine and
     cosine of pi times x, as do the signs. The infinities and NaNs (e = 255)
     are reduced in the same way.
  2. P: a 256-entry table for sin (pi * r) / r indexed by the top 8 bits of r
     (r = 1/2 is segment 255 with y = 2^18), y the 18 bits below them, T1 from
     the top 13 bits of y; coefficients at 2^-27, T1, S1 and A at 2^-28,
     truncated to 2^-24 with a rounding constant of 2^-25 in C0. On 18x18
     families T1 is rounded half up and S1 * y is the truncated multiplier of
     the base-10 exponential, compensated by 2^-46.
  3. The result is m' * P, with m' = r truncated to 26 significant bits, as
     an exact product on all families, rounded to nearest, ties away from
     zero. Where z is less than 2^-12 above an integer, the sine uses P(0)
     instead of P(r); just below an integer it uses P(r).
  4. The sine of |x| < 2^-11 is x, as is the sine of |x| < 2^-10 with
     |x| / pi < 2^-12; the cosine of |x| < 2^-11 is one, and so is the cosine
     where z is less than 2^-13 above an integer (with the sign of the half
     period). An exact zero of the sine (z = 0) has the sign of x, of the
     cosine (a half integer) it is positive. The sine of a zero or subnormal
     number is a zero of the same sign; the sine of NaN or an infinity is NaN
     with the sign the reduction gives, the cosine's is the positive NaN.
  5. The tangent is the sine divided by the cosine, both as above, with the
     reciprocal and the product of the tangent of pi times x. |x| < 2^-11
     gives x, zeros and subnormal numbers give a zero of the same sign, NaN
     and the infinities give NaN.

This was found as follows: the 80-bit window of 1 / pi and the 24x80 product
are in the synthesis report. The format search of the sine of pi times x, on
the intervals for P per r, found the table format with unique coefficients
for all segments, and m' = r truncated to 26 bits as the only width that
matches. The precision of the reduction only shows on a few inputs: those
were found by comparing the model with variants (76 to 99 fraction bits for
the product, truncation to 53 to 56 bits) on all inputs, and on all of them
only the combination above matches the IP. The
coefficients are those of the fpminimax method (see 'sinCoefficients'):
Chebyshev nodes left three segments off by one LSB, on exact ties in the
lattice reduction. The use of P(0) explained the last mismatches on random
inputs, all with r < 2^-12 above an even half period. The cosine and tangent
were then checked against the IP without further changes, apart from their
special cases.

Arctangent (the IP's \"ARCTAN\"), with r = |x|:

  1. For r <= 1: P(r) approximates atan r / r at 2^-26, with a 256-entry
     table indexed by the top 8 bits of X = floor (r * 2^26) (r = 1 is
     segment 255 with y = 2^18), y the 18 bits below them, T1 from the top 13
     bits of y; coefficients at 2^-29, T1, S1 and A at 2^-30, P truncated to
     2^-26 with a rounding constant of 2^-27 in C0. On 18x18 families T1 is
     rounded half up and S1 * y is the truncated multiplier of the base-10
     exponential, compensated by 2^-32. The result is m * P, with m the
     significand of x, rounded to nearest, ties away from zero.
  2. For r > 1, r = (1 + f * 2^-23) * 2^k: R approximates 1 / (1 + f * 2^-23)
     at 2^-24 (in (2^23, 2^24]), with a 256-entry table indexed by the top 8
     bits of f, y the low 15 bits, T1 from the top 12 bits of y; coefficients
     at 2^-27, T1, S1 and A at 2^-28, R truncated with a rounding constant of
     2^-25 in C0. On 18x18 families T1 is rounded half up and S1 * y is the
     truncated multiplier of the base-2 logarithm, compensated by 2^-30. Then
     z = R * 2^-24 * 2^-k, and atan z = R * P(X) exactly, with
     X = R * 2^(2 - min (k, 12)) rounded down: the IP shifts z into the
     polynomial's argument by at most 12 places, so for k > 12 it uses P at
     z * 2^(k - 12). atan z is zero for z < 2^-23.
  3. The result is pi / 2 rounded up to 2^-24 (0x3FC90FDB) minus atan z
     rounded down to 2^-24, truncated to single precision.
  4. |x| < 2^-12 gives x; zero and subnormal inputs give a zero of the same
     sign; the infinities give pi / 2 rounded down (0x3FC90FDA) with the sign
     of x, NaN gives NaN.

The arctangent divided by pi (\"ARCTAN\" with the angle represented as a
multiple of pi) is the arctangent above, rounded to single precision, times
1 / pi rounded to single precision (0x3EA2F983), rounded to nearest with ties
away from zero; results below the smallest normal number flush to zero. On
18x18 families the product is the truncated multiplier of the cosine of pi
times x (an 18x18, two 9x9 and two 2x2 tiles, as in the synthesis report),
with the constant as a 25-bit operand, 0xA2F983 * 2, and no compensation
(any constant up to 2^13 gives the same results on all inputs).

This was found as follows: the outputs depend on floor (r * 2^26) only for
r <= 1; the format search on the intervals of P, and for r > 1 on the
intervals of R (with the polynomial of r <= 1 fixed), found the formats with
unique coefficients for all segments of both tables, identical to the ones
generated from first principles. The constant and the rounding of step 3
followed from the outputs for r > 1, and the truncated multipliers were
fitted by coordinate descent over per-row cuts of the partial products. With
that, only inputs whose results are exact ties at 2^-24 (z an odd multiple of
2^-24, r >= 2^12) were left: they round down below a threshold of R that
differs between the datapaths and is not monotone on 18x18 families. That
threshold is where P(R * 2^(2 - 12)), at k = 12, drops one unit below one:
using P at that argument for all k > 12 rounds every tie as the IP does. Away
from ties the difference is invisible.

Two-argument arctangent atan2 y x (the IP's \"ARCTAN2\"):

  1. q = |y| / |x| is computed like the tangent of pi times x divides the
     sine by the cosine: the significand of y times the reciprocal of the
     significand of x (the same 512-entry table, and on 18x18 families the
     same truncated multipliers), rounded to nearest, ties away from zero.
     A quotient above the largest finite number becomes +inf, one below the
     smallest normal number +0.
  2. t is the arctangent of q as above (with its own copy of both tables).
     An infinite y gives t = pi / 2 rounded down, an infinite x t = 0, two
     infinities t = pi / 4 rounded up.
  3. For a negative x the result is pi - t, computed exactly and truncated;
     the result has the sign of y.
  4. Zeros and subnormal numbers (biased exponent 0): if only y is one, the
     result is 0 for x > 0 and pi rounded down for x < 0. If only x is one,
     it is pi / 2 rounded down for a positive subnormal x and pi / 2 rounded
     up for a zero or a negative x. If both are, it is: for y = 0, 0 for
     x = +0, pi / 2 - 1 (0x3F121FB4) for a positive subnormal x and pi
     rounded down for a negative x; for a subnormal y, pi / 2 - 1 for a
     positive subnormal x and pi - (pi / 2 - 1), truncated, otherwise. Any
     NaN input gives NaN.

This was found as follows: the synthesis report shows the reciprocal table
of the tangent of pi times x, the arctangent's two tables and a 26x24
product; the hypothesis "the arctangent of the tangent's quotient" matched
all random pairs of positive normal numbers at once; for a negative x, the
IP's results on pairs where pi - t is close to a rounding boundary (t a
small quotient q, with an exact division by a power of two) require pi to
at least 44 bits and t exactly, so the model uses pi to 200 bits. The
special cases were read off a table of all combinations of signed zeros,
subnormal numbers, the smallest normal number, one, the largest finite
number and infinity, and checked on random pairs of such inputs.

Power (@x ** y@, the IP's POWR function, IEEE 754's powr): the IP computes
exp (y * ln x) with its own logarithm and exponential of about 34 bits, both
piecewise cubic polynomials evaluated in Horner form (T1 = C3 * t,
S1 = C2 + T1, T2 = S1 * t, S2 = C1 + T2, T3 = S2 * t, A = C0 + T3), with the
tables of "Clash.Cores.Intel.Floating.Internal.CoefficientsPow":

  1. Logarithm (x positive and normal, x /= 1, x = m * 2^E, m = 1.f): for
     E = 0 and E = -1 (x in [0.5, 2)), ln x = z * P with z = x - 1 (exact)
     and P = ln (1 + z) / z from one of four regions of 128 segments (as for
     the single precision logarithm: x in [1, 1.5), [1.5, 2), [0.5, 0.75) and
     [0.75, 1)), t = the low 15 fraction bits. P is truncated to 33
     significant bits (the 2^-33 grid for P < 1, 2^-32 for P >= 1) and
     L = z * P is rounded to 34 significant bits, to nearest with ties away
     from zero. For other E, L = E * ln2 + f * P, with f * P from the tables
     of [1, 2), ln 2 rounded up to 36 fraction bits, the sum exact and then
     rounded the same way. The datapath works on the 2^-39 grid (2^-38 for
     P >= 1); T1 uses only the top 14 bits of t. With full multipliers every
     product is rounded down. With truncated multipliers T1 is rounded half up,
     T3 gets a quarter unit before it is rounded down, and T2 leaves out
     S1_i * t_j for i < 7, j < 5 and for i < 4, j < 8 and adds half a unit.
  2. The product: y * L, exact, its significand truncated to 34 bits.
  3. Exponential of that product a (as in the single precision exponential):
     k = floor (a / ln 2), estimated from the significand times 1 / ln 2
     (rounded up at 2^-40, the product floored at 2^-37; just below a multiple
     of ln 2 it can be one too high, and then the reduced argument, negative,
     is clamped to zero); r = a - k * ln2, with a on the 2^-34 grid and ln 2
     rounded DOWN to 36 fraction bits (unlike the logarithm's constant), the
     product exact; 178 segments of exp (r) on [0, ln 2), the top 8 bits of r
     (2^-36 grid) select the segment and the 28 bits below them form t
     (T1 uses its top 13 bits, T2 its top 22 bits and T3 all but its last 2;
     below, j counts the bits of t, from its least significant bit, that
     the product uses).
     A is on the 2^-40 grid with a rounding constant of 2^-36; the significand
     is A truncated to 34 bits, a carry into 2 increments the exponent. The
     result is truncated to single precision. With full multipliers (Cyclone
     V) every product is rounded down and T3 leaves out the partial products
     S2_i * t_j for i < 17, j < 8 and for i < 14, 8 <= j < 11; with truncated
     ones T1 is rounded half up, T2 is a truncated multiplier leaving out
     S1_i * t_j for i < 9, j < 8, T3 one leaving out i < 18 for j < 4,
     i < 10 for 4 <= j < 12 and i < 1 for 12 <= j < 21, both with half a unit
     added. Results above the largest finite number give +inf, below the
     smallest normal number +0.
  4. Special cases: a NaN input gives NaN; a negative x (normal or -inf)
     gives NaN; a zero or subnormal x (of either sign) gives NaN for a zero or
     subnormal y, +inf for a negative y and +0 for a positive one; x = +inf
     gives NaN for a zero or subnormal y, +0 for a negative y and +inf for a
     positive one; a zero or subnormal y gives 1; an infinite y gives +inf or
     +0 depending on its sign and on whether x is above or below one, and NaN
     for x = 1. For x = 1 and finite y the result is 1, except that the IP's
     logarithm of one is 2^-57 rather than 0, which shows when the exponential
     of y * 2^-57 overflows or underflows: for 2^63 <= |y| < 2^66 (+inf or +0
     as for that exponential) and for y <= -1.365 * 2^127 (about; +0, as if
     y were 2^64 times smaller).

This was found as follows. The synthesis report shows two cubic tables (512
entries for the logarithm, 256 for the exponential), the exponential's
multipliers equal those of the generator's own 33 bit exponential (whose
standalone build gives 42 bit results, which pinned down its datapath on
both kinds of multipliers), a 36x26 product (z * P) and a 24x34 one (y * L).
For x = 2^E, L is E * ln 2 rounded, which fixed the product's truncation and
the exponential's constants. The logarithm cannot be observed directly: for
each x, about 40 values of y with |y * ln x| between 40 and 85 give
intervals for y * L, and their intersection on the 34 bit grid gives L
exactly (more values of y where it doesn't). From L and z, every such x then
gives P on its 33 bit grid exactly, and the polynomial's formats and
truncations were fitted to these values of P, on both datapaths. The
coefficients reproduce from first principles (see the module of the tables).
The special cases were read off all combinations of special values, and the
thresholds for x = 1 off sweeps of y.

The IP generates the NaN 0x7F800001.
-}

-- | Which datapath the IP uses for its polynomials, determined by the device
-- family, see Note [Bit-exact models]
data Multipliers
  = TruncatedMultipliers
  -- ^ MAX 10, Cyclone IV E and Cyclone 10 LP (18x18 multipliers)
  | FullMultipliers
  -- ^ Cyclone V (27x27 multipliers)
  deriving (Eq, Show, Enum, Bounded)

-- | The NaN the IP generates
nanSingle :: Word32
nanSingle = 0x7F800001

-- | Positive infinity
infSingle :: Word32
infSingle = 0x7F800000

-- | A coefficient table as three arrays: C2, C1, C0
data Table = Table !(UArray Int Int64) !(UArray Int Int64) !(UArray Int Int64)

mkTable :: [(Int, Int, Int)] -> Table
mkTable cs =
  Table (arr [c | (c, _, _) <- cs]) (arr [c | (_, c, _) <- cs]) (arr [c | (_, _, c) <- cs])
 where
  arr xs = listArray (0, length cs - 1) (map fromIntegral xs)

-- | Look up the coefficients of a segment. Callers guarantee that the index is
-- in range.
coefs :: Table -> Int -> (Int64, Int64, Int64)
coefs (Table c2 c1 c0) i = (unsafeAt c2 i, unsafeAt c1 i, unsafeAt c0 i)
{-# INLINE coefs #-}

sqrtTable, cbrtTable, rsqrtTable, expTable, exp2Table, exp10Table, logTable, log2Table, log10Table :: Table
sqrtTable = mkTable sqrtCoefficients
{-# NOINLINE sqrtTable #-}
cbrtTable = mkTable cbrtCoefficients
{-# NOINLINE cbrtTable #-}
rsqrtTable = mkTable rsqrtCoefficients
{-# NOINLINE rsqrtTable #-}
expTable = mkTable expCoefficients
{-# NOINLINE expTable #-}
exp2Table = mkTable exp2Coefficients
{-# NOINLINE exp2Table #-}
exp10Table = mkTable exp10Coefficients
{-# NOINLINE exp10Table #-}
logTable = mkTable logCoefficients
{-# NOINLINE logTable #-}
log2Table = mkTable log2Coefficients
{-# NOINLINE log2Table #-}
log10Table = mkTable log10Coefficients
{-# NOINLINE log10Table #-}

-- | The table of 'sinPiSingle' and 'cosPiSingle'
sinPiTable :: Table
sinPiTable = mkTable sinPiCoefficients
{-# NOINLINE sinPiTable #-}

-- | The reciprocal table of 'tanPiSingle'
tanPiRecipTable :: Table
tanPiRecipTable = mkTable tanPiRecipCoefficients
{-# NOINLINE tanPiRecipTable #-}

-- | The table of 'sinSingle', 'cosSingle' and 'tanSingle'
sinTable :: Table
sinTable = mkTable sinCoefficients
{-# NOINLINE sinTable #-}

-- | Fields of a single precision number: sign bit, biased exponent, fraction
fields :: Word32 -> (Bool, Int, Int64)
fields w =
  (testBit w 31, fromIntegral ((w `shiftR` 23) .&. 0xFF), fromIntegral (w .&. 0x7FFFFF))
{-# INLINE fields #-}

-- | The truncated multiplier of the square root, the reciprocal square root
-- and the logarithm: S1 * y without the partial products S1_i * y_j with
-- (i < 3, j < j0 + 8) or (i < 6, j < j0 + 5), where y has j0 + 15 bits and the
-- low j0 bits of y are ignored by the two small tiles
staircase :: Int -> Int64 -> Int64 -> Int64
staircase j0 s1 y =
    ((s1 `shiftR` 6) * y) `shiftL` 6
  + ((s1 .&. 63) * (y `shiftR` (j0 + 8))) `shiftL` (j0 + 8)
  + (((s1 `shiftR` 3) .&. 7) * ((y `shiftR` (j0 + 5)) .&. 7)) `shiftL` (j0 + 8)
{-# INLINE staircase #-}

-- * Square root

-- | The IP's square root in single precision, on bit patterns
sqrtSingle :: Multipliers -> Word32 -> Word32
sqrtSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  | neg = nanSingle
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 127) `shiftL` 23) + fromIntegral (o - bit 23)
 where
  (neg, e, f) = fields w
  k = (e - 127) `shiftR` 1
  p = (e - 127) .&. 1
  o = sqrtSig mults p f

-- | Significand of sqrt (m * 2^p) in units of 2^-23 (2^23 to 2^24), for the
-- fraction @f@ of m.
sqrtSig :: Multipliers -> Int -> Int64 -> Int64
sqrtSig mults p f = rootPoly sqrtTable mults p (p * 128) f `shiftR` 21

-- | The polynomial of the square root and the cube root: A at 2^-44 (before
-- truncation to the significand at 2^-23) for the fraction @f@, with the
-- segment @s0@ plus the top 7 bits of @f@. The coefficients have their LSBs at
-- 2^-(13 - p), 2^-(20 - p) and 2^-(27 - p); S1 at 2^-21.
rootPoly :: Table -> Multipliers -> Int -> Int -> Int64 -> Int64
rootPoly table mults p s0 f = a
 where
  (c2, c1, c0) = coefs table (s0 + fromIntegral (f `shiftR` 16))
  y = f .&. 0xFFFF
  -- C2 times the top 12 bits of y, to 2^-21
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 4)) `shiftR` (11 - p)
    TruncatedMultipliers -> (c2 * (y `shiftR` 4) + bit (10 - p)) `shiftR` (11 - p)
  s1 = (c1 `shiftL` (1 + p)) + t1
  -- C0 plus the rounding constant of 4 LSBs, at 2^-44
  a0 = (c0 + 4) `shiftL` (17 + p)
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    -- Compensation constant 2^-30
    TruncatedMultipliers -> a0 + staircase 1 s1 y + bit 14

-- * Cube root

-- | The IP's cube root in single precision, on bit patterns
cbrtSingle :: Multipliers -> Word32 -> Word32
cbrtSingle mults w
  -- NaN keeps its sign
  | e == 255 = if f /= 0 then sgn .|. nanSingle else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = sgn
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = sgn .|. ((fromIntegral (k + 127) `shiftL` 23) + fromIntegral (o - bit 23))
 where
  (_, e, f) = fields w
  sgn = w .&. bit 31
  -- x = m * 2^(3k + r), cbrt x = cbrt (m * 2^r) * 2^k
  (k, r) = (e - 127) `divMod` 3
  -- The polynomial of the square root with the formats of p = 0
  o = rootPoly cbrtTable mults 0 (r * 128) f `shiftR` 21

-- * Hypotenuse

-- | The IP's three-dimensional hypotenuse, sqrt (x^2 + y^2 + z^2), in single
-- precision, on bit patterns
hypotSingle :: Multipliers -> Word32 -> Word32 -> Word32 -> Word32
hypotSingle mults x y z
  | any isNaN' xs = nanSingle
  | any isInf' xs = infSingle
  | null sqs = 0
  | be >= 255 = infSingle
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 127) `shiftL` 23) + fromIntegral (o - bit 23)
 where
  xs = [x, y, z]
  isNaN' w = let (_, e, f) = fields w in e == 255 && f /= 0
  isInf' w = let (_, e, f) = fields w in e == 255 && f == 0
  -- The squares of the normal inputs as floating point numbers: the exponent
  -- and the significand, truncated to 24 bits, at 2^-23
  sqs = [square e f | w <- xs, let (_, e, f) = fields w, e /= 0]
  square e f =
    let m = f .|. bit 23
        s = m * m
        h = fromIntegral (s `shiftR` 47)
     in (2 * (e - 127) + h, s `shiftR` (23 + h))
  eMax = maximum (map fst sqs)
  -- Aligned to the largest square at 2^-25, rounded down; a square shifted by
  -- more than 24 places counts as zero. The sum is in [1, 12) (at 2^-25).
  sumSq = sum [(m `shiftL` 2) `shiftR` d | (e, m) <- sqs, let d = eMax - e, d <= 24]
  l | sumSq >= bit 27 = 2
    | sumSq >= bit 26 = 1
    | otherwise = 0 :: Int
  -- Rounded to nearest at 24 bits, ties up
  q0 = (sumSq `shiftR` (1 + l) + 1) `shiftR` 1
  (q, l') = if q0 >= bit 24 then (q0 `shiftR` 1, l + 1) else (q0, l)
  -- The square root of q * 2^(eMax + l' - 23), with an extra half unit in the
  -- last place before the truncation
  (k, p) = (eMax + l') `divMod` 2
  o = (rootPoly sqrtTable mults p (p * 128) (q - bit 23) + bit 20) `shiftR` 21
  be = k + 127 + fromIntegral (o `shiftR` 24)

-- * Reciprocal square root

-- | The IP's reciprocal square root in single precision, on bit patterns
rsqrtSingle :: Multipliers -> Word32 -> Word32
rsqrtSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else 0
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = (w .&. bit 31) .|. infSingle
  | neg = nanSingle
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise =
      (fromIntegral ((380 - e) `shiftR` 1) `shiftL` 23) + fromIntegral (o - bit 23)
 where
  (neg, e, f) = fields w
  o = rsqrtSig mults (if even e then 1 else 0) f

-- | Significand of 2 / sqrt (m * 2^p) in units of 2^-23 (2^23 to 2^24), for
-- the fraction @f@ of m. Coefficients at 2^-11, 2^-19, 2^-27; S1 at 2^-20.
rsqrtSig :: Multipliers -> Int -> Int64 -> Int64
rsqrtSig mults p f = case mults of
  FullMultipliers ->
    let t1 = (c2 * (y `shiftR` 3)) `shiftR` 11
        s1 = 2 * c1 + t1
    in  (c0' + ((s1 * y) `shiftR` 16)) `shiftR` 4
  TruncatedMultipliers ->
    let t1 = (c2 * (y `shiftR` 3) + bit 10) `shiftR` 11
        s1 = 2 * c1 + t1
    -- At 2^-37, with the compensation constant 2^-30
    in  ((c0' `shiftL` 10) + bit 7 + (staircase 0 s1 y `shiftR` 6)) `shiftR` 14
 where
  (c2, c1, c0) = coefs rsqrtTable (p * 256 + fromIntegral (f `shiftR` 15))
  y = f .&. 0x7FFF
  -- Rounding constant: half an output LSB for p = 0, a quarter for p = 1
  c0' = c0 + (if p == 1 then 4 else 8)

-- * Reciprocal

-- | The IP's reciprocal in single precision, on bit patterns
recipSingle :: Word32 -> Word32
recipSingle w
  | e == 255 = if f /= 0 then nanSingle else s
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = s .|. infSingle
  | h < -127 = s
  | h > 127 = s .|. infSingle
  -- h = -127 gives an exponent field of 0 with the fraction bits of q
  | otherwise = s .|. (fromIntegral (h + 127) `shiftL` 23) .|. fromIntegral (q - bit 23)
 where
  (_, e, f) = fields w
  s = w .&. bit 31
  m = f .|. bit 23
  (q0, r) = bit 47 `quotRem` m
  -- 2^47 / m, rounded to nearest, ties to even
  q1 | 2 * r > m || (2 * r == m && odd q0) = q0 + 1
     | otherwise = q0
  -- The result is q * 2^(h - 23); only m = 2^23 gives q1 = 2^24
  (h, q) | q1 >= bit 24 = (127 - e, q1 `shiftR` 1)
         | otherwise = (126 - e, q1)

-- * Exponential

-- | The IP's exponential in single precision, on bit patterns
expSingle :: Multipliers -> Word32 -> Word32
expSingle mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 512
  | sh > 8 = if neg then 0 else infSingle
  | be >= 255 = infSingle
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 23) .|. fromIntegral (o .&. 0x7FFFFF)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-24
  sh = e - 126
  -- |x| at 2^-24, rounded down for positive x and up for negative x (below
  -- 2^-24 the significand is shifted out completely)
  xAbs | sh >= 0 = m `shiftL` sh
       | otherwise =
           let d = min 24 (negate sh)
           in  (m `shiftR` d) + (if neg && m .&. (bit d - 1) /= 0 then 1 else 0)
  -- x = floor (x * 2^24)
  x = if neg then negate xAbs else xAbs
  -- m / ln 2 to 27 fraction bits (relative to m), with 1/ln 2 rounded down to
  -- 34 fraction bits
  q = (if neg then negate else id) ((m * 24785312074) `shiftR` 30)
  -- k = floor (x / ln 2), estimated
  k = q `shiftR` min 63 (154 - e)
  -- k * ln 2 with ln 2 rounded down to 34 fraction bits, rounded down to 33
  -- fraction bits and then up to 26
  kLn2 = negate (negate ((k * 11908177887) `shiftR` 1) `shiftR` 7)
  -- r = x - k * ln 2 in units of 2^-26; when k is one too high, r = 0
  r = max 0 (4 * x - kLn2)
  (c2, c1, c0) = coefs expTable (fromIntegral (r `shiftR` 19))
  -- The low 19 bits of r, at 2^-19 of a segment (weights of 2^-27 units)
  y = r .&. (bit 19 - 1)
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 5)) `shiftR` 14
    TruncatedMultipliers -> (c2 * (y `shiftR` 5) + bit 13) `shiftR` 14
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-24, at 2^-54
  a0 = (c0 + 8) `shiftL` 27
  a = case mults of
    FullMultipliers -> a0 + (s1 * y) `shiftL` 8
    TruncatedMultipliers ->
      let drop' = (s1 .&. 0x3) * (y .&. 0xFFF)
                + (s1 .&. 0x7C) * (y .&. 0x7F)
                + (s1 .&. 0x780) * (y .&. 0x7)
      -- Compensation constant 2^-29
      in  a0 + (s1 * y - drop' + bit 17) `shiftL` 8
  -- The significand in units of 2^-23: [1, 4)
  o0 = a `shiftR` 31
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, fromIntegral k + 128 :: Int)
          | otherwise = (o0, fromIntegral k + 127)

-- * Exponential, base 2

-- | The IP's base-2 exponential in single precision, on bit patterns
exp2Single :: Multipliers -> Word32 -> Word32
exp2Single mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 512
  | sh > 8 = if neg then 0 else infSingle
  | otherwise = expResult k (a `shiftR` 20)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-23
  sh = e - 127
  -- x = floor (x * 2^23) = k * 2^23 + the fraction bits of x
  x = expFixed neg sh m
  k = x `shiftR` 23
  (c2, c1, c0) = coefs exp2Table (fromIntegral ((x `shiftR` 16) .&. 127))
  -- The low 16 fraction bits
  y = x .&. 0xFFFF
  -- C2 times the top 12 bits of y, to 2^-27
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 4)) `shiftR` 12
    TruncatedMultipliers -> (c2 * (y `shiftR` 4) + bit 11) `shiftR` 12
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-24, at 2^-43
  a0 = (c0 + 8) `shiftL` 16
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    -- Compensation constant 2^-29
    TruncatedMultipliers -> a0 + staircase 1 s1 y + bit 14

-- * Exponential, base 10

-- | The IP's base-10 exponential in single precision, on bit patterns
exp10Single :: Multipliers -> Word32 -> Word32
exp10Single mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 256
  | sh > 8 = if neg then 0 else infSingle
  | otherwise = expResult k (a `shiftR` 22)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-24
  sh = e - 126
  -- x = floor (x * 2^24)
  x = expFixed neg sh m
  -- m * log2 10 to 26 fraction bits (relative to m), with log2 10 rounded to
  -- nearest at 26 fraction bits
  q = (if neg then negate else id) ((m * 222930821) `shiftR` 23)
  -- k = floor (x * log2 10), estimated
  k = q `shiftR` min 63 (153 - e)
  -- k * log10 2 with log10 2 rounded to nearest at 34 fraction bits, rounded
  -- up to 27 fraction bits
  kLog = negate (negate (k * 5171655946) `shiftR` 7)
  -- r = x - k * log10 2 in units of 2^-27; when k is one too high, r = 0
  r = max 0 (8 * x - kLog)
  (c2, c1, c0) = coefs exp10Table (fromIntegral (r `shiftR` 18))
  -- The low 18 bits of r
  y = r .&. (bit 18 - 1)
  -- C2 times the top 13 bits of y, to 2^-27
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 5)) `shiftR` 13
    TruncatedMultipliers -> (c2 * (y `shiftR` 5) + bit 12) `shiftR` 13
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-23, at 2^-45
  a0 = (c0 + 16) `shiftL` 18
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    TruncatedMultipliers ->
      let drop' = (s1 .&. 0x3) * (y .&. 0x7FF)
                + (s1 .&. 0x3C) * (y .&. 0x7F)
                + (s1 .&. 0x7C0) * (y .&. 0x3)
      -- Compensation constant 2^-29
      in  a0 + s1 * y - drop' + bit 16

-- | @floor (x * 2^n)@ in two's complement for @|x| = m * 2^(sh - n)@ with
-- @m < 2^24@ (below 2^-n the significand is shifted out completely)
expFixed :: Bool -> Int -> Int64 -> Int64
expFixed neg sh m = if neg then negate xAbs else xAbs
 where
  -- |x| rounded down for positive x and up for negative x
  xAbs | sh >= 0 = m `shiftL` sh
       | otherwise =
           let d = min 24 (negate sh)
           in  (m `shiftR` d) + (if neg && m .&. (bit d - 1) /= 0 then 1 else 0)

-- | The result of 'exp2Single' and 'exp10Single' from the unbiased exponent
-- @k@ and the significand @o0@ in units of 2^-23 (in [1, 4)): normalised,
-- with overflow to +inf and underflow to +0 (no subnormal results)
expResult :: Int64 -> Int64 -> Word32
expResult k o0
  | be >= 255 = infSingle
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 23) .|. fromIntegral (o .&. 0x7FFFFF)
 where
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, k + 128)
          | otherwise = (o0, k + 127)

-- * Natural logarithm

-- | The IP's natural logarithm in single precision, on bit patterns
logSingle :: Multipliers -> Word32 -> Word32
logSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  | otherwise = roundAt 47 s
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF
  -- The result at 2^-47
  s | e == 126 =
        -- x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-23
        (f - bit 23) * logPoly mults ((if hi then 384 else 256) + j) y
    | otherwise =
        -- z at 2^-23, P at 2^-24, (e - 127) * ln 2 with ln 2 rounded to 21
        -- fraction bits
        f * logPoly mults ((if hi then 128 else 0) + j) y
          + (fromIntegral (e - 127) * 1453635) `shiftL` 26

-- | P = ln (1 + z) / z truncated to 24 bits (units of 2^-24 or 2^-23)
logPoly :: Multipliers -> Int -> Int64 -> Int64
logPoly mults i y = (c0 + 8 + lnT2 mults c2 c1 y) `shiftR` 6
 where
  (c2, c1, c0) = coefs logTable i

-- | The part @C2 * y^2 + C1 * y@ of the polynomials of the natural and base
-- 10 logarithms and of @log (1 + x)@, in the units of the table
lnT2 :: Multipliers -> Int64 -> Int64 -> Int64 -> Int64
lnT2 mults c2 c1 y = case mults of
  FullMultipliers ->
    let s1 = c1 + ((c2 * (y `shiftR` 2)) `shiftR` 13)
    in  (s1 * y) `shiftR` 15
  TruncatedMultipliers ->
    let s1 = c1 + ((c2 * (y `shiftR` 2) + bit 12) `shiftR` 13)
    -- Compensation constant 2^13 (2^-32 for the regions with P < 1 of the
    -- natural logarithm)
    in  (staircase 0 s1 y + bit 13) `shiftR` 15

-- | Round a non-zero value in units of 2^-g to single precision: to nearest,
-- ties away from zero. The result must be a normal number.
roundAt :: Int -> Int64 -> Word32
roundAt g s =
      (if s < 0 then bit 31 else 0)
  .|. (fromIntegral be `shiftL` 23)
  .|. fromIntegral (o .&. 0x7FFFFF)
 where
  a = abs s
  -- The number of bits to drop
  d = 64 - countLeadingZeros a - 24
  o0 | d <= 0 = a `shiftL` negate d
     | otherwise = (a + bit (d - 1)) `shiftR` d
  -- The biased exponent: 2^-g * 2^d * 2^23 * 2^127
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, d - g + 151)
          | otherwise = (o0, d - g + 150)

-- * Base 2 logarithm

-- | The IP's base 2 logarithm in single precision, on bit patterns
log2Single :: Multipliers -> Word32 -> Word32
log2Single mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  -- x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-24
  | e == 126 = roundAt 48 ((f - bit 23) * log2Poly mults ((if hi then 384 else 256) + j) y)
  -- z at 2^-23, P at 2^-24, the exponent (e - 127) exactly
  | otherwise =
      roundAt 47
        (f * log2Poly mults ((if hi then 128 else 0) + j) y
          + fromIntegral (e - 127) `shiftL` 47)
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF

-- | P = log2 (1 + z) / z, in [1, 2], truncated to 2^-24
log2Poly :: Multipliers -> Int -> Int64 -> Int64
log2Poly mults i y = (c0 + 8 + t2) `shiftR` 4
 where
  (c2, c1, c0) = coefs log2Table i
  t2 = case mults of
    FullMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 3)) `shiftR` 12)
      in  (s1 * y) `shiftR` 15
    TruncatedMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 3) + bit 11) `shiftR` 12)
          -- Leaves out S1_i * y_j for (i < 3, j < 8) and (i < 5, j < 6)
          p = ((s1 `shiftR` 5) * y) `shiftL` 5
            + ((s1 .&. 31) * (y `shiftR` 8)) `shiftL` 8
            + (((s1 `shiftR` 3) .&. 3) * ((y `shiftR` 6) .&. 3)) `shiftL` 9
      -- Compensation constant 2^13 (2^-30)
      in  (p + bit 13) `shiftR` 15

-- * Base 10 logarithm

-- | The IP's base 10 logarithm in single precision, on bit patterns
log10Single :: Multipliers -> Word32 -> Word32
log10Single mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  -- The result at 2^-49. x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-25
  | e == 126 =
      roundAt 49 ((f - bit 23) * log10Poly mults 0 ((if hi then 384 else 256) + j) y)
  -- z at 2^-23, P at 2^-26, (e - 127) * log10 2 with log10 2 rounded to 30
  -- fraction bits (20201781 * 2^-26)
  | otherwise =
      roundAt 49
        (f * log10Poly mults 1 ((if hi then 128 else 0) + j) y
          + (fromIntegral (e - 127) * 20201781) `shiftL` 23)
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF

-- | P = log10 (1 + z) / z truncated to 2^-(25 + k). For k = 1 (x in [1, 2),
-- where P < 0.5), the polynomial is evaluated with the coefficients doubled,
-- one bit finer.
log10Poly :: Multipliers -> Int -> Int -> Int64 -> Int64
log10Poly mults k i y =
  (((c0 + 8) `shiftL` k) + lnT2 mults (c2 `shiftL` k) (c1 `shiftL` k) y) `shiftR` 5
 where
  (c2, c1, c0) = coefs log10Table i

-- * Natural logarithm of 1 + x

-- | The IP's @log (1 + x)@ in single precision, on bit patterns
log1pSingle :: Multipliers -> Word32 -> Word32
log1pSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  -- x <= -1
  | neg && e >= 127 = if e == 127 && f == 0 then 0xFF800000 else nanSingle
  -- 1 + x in [0.5, 2): x * P (1 + x), at 2^-(e' + 25), with x = m * 2^-e' at
  -- 2^-e' = 2^(e - 150) and P at 2^-25 or 2^-24
  | small =
      if v >= bit 24
        then
          -- 1 + x in [1, 2), at 2^-23
          let v' = (v - bit 24) `shiftR` 1
          in  roundAt (175 - e) (mx * log1pPoly mults ((if testBit v' 22 then 128 else 0) + jv v') v')
        else
          -- 1 + x in [0.5, 1), at 2^-24
          let v' = v - bit 23
          in  roundAt (174 - e) (mx * log1pPoly mults ((if testBit v' 22 then 384 else 256) + jv v') v')
  -- Otherwise 1 + x = (1 + g * 2^-23) * 2^ex truncated to 24 bits:
  -- ex * ln 2 + g * P at 2^-48, ln 2 rounded to 21 fraction bits
  | otherwise =
      roundAt 48
        (g * log1pPoly mults ((if testBit g 22 then 128 else 0) + jv g) g
          + (fromIntegral ex * 1453635) `shiftL` 27)
 where
  (neg, e, f) = fields w
  m = f .|. bit 23
  mx = if neg then negate m else m
  -- 1 + x in [0.5, 2): x in (-0.5, 1)
  small = if neg then e <= 125 else e <= 126
  -- 1 + x at 2^-24, rounded down (x = +-m * 2^-(sh + 24))
  sh = 126 - e
  v | neg = bit 24 + (negate m `shiftR` sh)
    | otherwise = bit 24 + (m `shiftR` sh)
  jv u = fromIntegral ((u `shiftR` 15) .&. 127)
  -- 1 + x with its 24 leading bits, as 1 + g * 2^-23 times 2^ex
  (ex, g)
    -- 1 is below the last bit of x
    | e > 150 = (e - 127, f)
    | otherwise =
        let -- 1 + x at 2^-(150 - e), x is positive or in (-1, -0.5]
            u = bit (150 - e) + mx
            nb = 64 - countLeadingZeros u
            u' | nb >= 24 = u `shiftR` (nb - 24)
               | otherwise = u `shiftL` (24 - nb)
        in  (nb - 1 - (150 - e), u' - bit 23)

-- | P = ln (1 + z) / z truncated to 25 bits (units of 2^-25 or 2^-24): the
-- 'logPoly' of the natural logarithm with one more bit
log1pPoly :: Multipliers -> Int -> Int64 -> Int64
log1pPoly mults i u = (c0 + 8 + lnT2 mults c2 c1 (u .&. 0x7FFF)) `shiftR` 5
 where
  (c2, c1, c0) = coefs logTable i

-- * Sine and cosine of pi times x

-- | The IP's @sin (pi * x)@ in single precision, on bit patterns
sinPiSingle :: Multipliers -> Word32 -> Word32
sinPiSingle mults w
  -- Infinities give NaN with the input's sign, NaNs with the opposite sign
  | e == 255 = (if f /= 0 then complement w .&. bit 31 else w .&. bit 31) .|. nanSingle
  -- Zeros, subnormal numbers and integers give a zero of the input's sign
  | e == 0 || e >= 150 = w .&. bit 31
  -- |x| < 2^-12: the significand times sin (pi r) / r at r = 0
  | e <= 114 = sinPiRound sgn (bit 23 .|. f) (e - 150) (sinPiPoly mults 0 0)
  | r == 0 = w .&. bit 31
  -- |x| < 1/2: the significand truncated to 23 bits
  | e <= 125 = sinPiRound sgn' ((bit 23 .|. f) `shiftR` 1) (e - 149) p
  -- Otherwise r is exact
  | otherwise = sinPiRound sgn' r (-24) p
 where
  (neg, e, f) = fields w
  sgn = if neg then bit 31 else 0
  -- floor (|x| * 2^24) mod 2^25: q is the half period, r in [0, 2^23] the
  -- distance to the nearest integer, in units of 2^-24
  xx | e >= 126 = ((bit 23 .|. f) `shiftL` (e - 126)) .&. (bit 25 - 1)
     | otherwise = (bit 23 .|. f) `shiftR` (126 - e)
  q = xx `shiftR` 23
  lo = xx .&. (bit 23 - 1)
  r = if odd q then bit 23 - lo else lo
  sgn' = if q >= 2 then sgn `xor` bit 31 else sgn
  p = sinPiPoly mults (fromIntegral (r `shiftR` 16)) (r .&. 0xFFFF)

-- | The IP's @cos (pi * x)@ in single precision, on bit patterns
cosPiSingle :: Multipliers -> Word32 -> Word32
cosPiSingle mults w
  | e == 255 = nanSingle
  -- |x| < 2^-13 (including zeros and subnormal numbers) gives 1
  | e <= 113 = 0x3F800000
  -- x + 1/2 is an integer
  | rf == 0 = 0
  | otherwise = case mults of
      FullMultipliers -> sinPiRound sgn m em p
      TruncatedMultipliers ->
        let mn = m `shiftL` (23 - (63 - countLeadingZeros m))
            emn = em - (23 - (63 - countLeadingZeros m))
        in  sgn .|. roundAt (23 - emn) (cosPiProduct mn p)
 where
  (_, e, f) = fields w
  -- floor (|x| * 2^37) mod 2^38: q is the half period, rf in [0, 2^36] the
  -- distance to the nearest half integer, in units of 2^-37
  sh = e - 113
  xx | sh >= 38 = 0
     | otherwise = ((bit 23 .|. f) `shiftL` sh) .&. (bit 38 - 1)
  q = xx `shiftR` 36
  lo = xx .&. (bit 36 - 1)
  rf = if odd q then lo else bit 36 - lo
  sgn = if q == 1 || q == 2 then bit 31 else 0
  -- The polynomial at r = rf truncated to 2^-24
  r = rf `shiftR` 13
  p = sinPiPoly mults (fromIntegral (r `shiftR` 16)) (r .&. 0xFFFF)
  -- rf truncated to 24 significant bits
  nb = 64 - countLeadingZeros rf
  (m, em) | nb > 24 = (rf `shiftR` (nb - 24), nb - 24 - 37)
          | otherwise = (rf, -37)

-- | Round @m * 2^e * p * 2^-23@ to nearest, ties away from zero, with sign @s@
sinPiRound :: Word32 -> Int64 -> Int -> Int64 -> Word32
sinPiRound s m e p = s .|. roundAt (23 - e) (m * p)

-- | P = sin (pi * r) / r at 2^-23 for r = (j + y * 2^-16) / 256; r = 1/2 is
-- segment 127 with y = 2^16
sinPiPoly :: Multipliers -> Int -> Int64 -> Int64
sinPiPoly mults j0 y0 = (2 * (c0 + 4) + t2) `shiftR` 4
 where
  (j, y) = if j0 == 128 then (127, bit 16) else (j0, y0)
  (c2, c1, c0) = coefs sinPiTable j
  -- At 2^-27
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 2)) `shiftR` 13)
      in  (s1 * y) `shiftR` 16
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 2) + bit 12) `shiftR` 13)
      -- Compensation constant 2^14 (2^-29)
      in  (staircase 1 s1 y + bit 14) `shiftR` 16

-- | The truncated 24x25 multiplier of the cosine on 18x18 families: m times p
-- without the partial products below a staircase of tiles, plus 2^22
cosPiProduct :: Int64 -> Int64 -> Int64
cosPiProduct m p = truncatedProduct24x25 m p + bit 22

-- | The tiles of the truncated 24x25 multiplier of the cosine (and the
-- arctangent divided by pi) on 18x18 families: an 18x18, two 9x9 and two 2x2
-- multipliers, without a compensation constant
truncatedProduct24x25 :: Int64 -> Int64 -> Int64
truncatedProduct24x25 m p =
    ((m `shiftR` 6) * (p `shiftR` 7)) `shiftL` 13
  + ((m .&. 63) * (p `shiftR` 16)) `shiftL` 16
  + ((m `shiftR` 15) * (p .&. 127)) `shiftL` 15
  + (((m `shiftR` 4) .&. 3) * ((p `shiftR` 14) .&. 3)) `shiftL` 18
  + (((m `shiftR` 13) .&. 3) * ((p `shiftR` 5) .&. 3)) `shiftL` 18

-- * Tangent of pi times x

-- | The IP's @tan (pi * x)@ in single precision, on bit patterns: the IP's
-- sine of pi times x divided by its cosine of pi times x
tanPiSingle :: Multipliers -> Word32 -> Word32
tanPiSingle mults w
  | e == 255 = nanSingle
  -- Zeros and subnormal numbers are returned unchanged
  | e == 0 = w
  -- |x| < 2^-13: x times pi rounded to single precision (0x40490FDB), rounded
  | e <= 113 = (w .&. bit 31) .|. roundAt (173 - e) ((bit 23 .|. f) * 26353590)
  | otherwise = tanQuotient mults (sinPiSingle mults w) (cosPiSingle mults w)
 where
  (_, e, f) = fields w

-- | The tangent from the IP's sine and cosine rounded to single precision:
-- the sine's significand times the reciprocal of the cosine's significand
tanQuotient :: Multipliers -> Word32 -> Word32 -> Word32
tanQuotient mults s c
  -- The sine is zero: a zero
  | s .&. 0x7FFFFFFF == 0 = sgn
  -- The cosine is zero: an infinity
  | c .&. 0x7FFFFFFF == 0 = sgn .|. infSingle
  -- The quotient at 2^-g
  | otherwise = sgn .|. roundAt (49 - es + ec) p
 where
  sgn = (s `xor` c) .&. bit 31
  (_, es, fs) = fields s
  (_, ec, fc) = fields c
  sm = bit 23 .|. fs
  r = tanPiRecip mults fc
  p = case mults of
    TruncatedMultipliers | fc /= 0 -> tanPiProduct sm r
    _ -> sm * r

-- | The reciprocal @1 / (1 + f * 2^-23)@ of the cosine's significand at
-- 2^-26, for its fraction @f@. The coefficients have their LSBs at 2^-29, T1
-- (from the top 12 bits of y) and S1 at 2^-30.
tanPiRecip :: Multipliers -> Int64 -> Int64
tanPiRecip mults f = (2 * (c0 + 4) + t2) `shiftR` 4
 where
  (c2, c1, c0) = coefs tanPiRecipTable (fromIntegral (f `shiftR` 14))
  y = f .&. 0x3FFF
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 2)) `shiftR` 12)
      in  (s1 * y) `shiftR` 14
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 2) + bit 11) `shiftR` 12)
      -- Compensation constant 2^12 (2^-32)
      in  (staircase (-1) s1 y + bit 12) `shiftR` 14

-- | The truncated 24x26 multiplier of the tangent on 18x18 families (the tiles
-- of 'cosPiProduct'): the sine's significand times the reciprocal. Any
-- compensation constant from 0 to 2^13 gives the same results on all inputs;
-- the model adds none.
tanPiProduct :: Int64 -> Int64 -> Int64
tanPiProduct a b =
    ((a `shiftR` 6) * (b `shiftR` 8)) `shiftL` 14
  + ((a .&. 63) * (b `shiftR` 17)) `shiftL` 17
  + ((a `shiftR` 15) * (b .&. 255)) `shiftL` 15
  + (((a `shiftR` 4) .&. 3) * ((b `shiftR` 15) .&. 3)) `shiftL` 19
  + (((a `shiftR` 13) .&. 3) * ((b `shiftR` 6) .&. 3)) `shiftL` 19

-- * Sine, cosine and tangent

-- | @floor (2^200 / pi)@: 1 / pi to 200 fraction bits
invPi :: Integer
invPi = 0x517cc1b727220a94fe13abe8fa9a6ee06db14acc9e21c820ff

-- | The reduced argument of the sine and cosine for the biased exponent @e@
-- (116 to 255) and the fraction @f@: the half period @q@ (0 to 3) and the
-- fraction @z@ in it (in units of 2^-77) of @|x| / pi@ modulo 2. It is
-- computed with 77 fraction bits as the significand times
-- @floor (2^(e - 73) / pi)@, modulo 2^78, and then truncated to @t@ fraction
-- bits.
trigReduce :: Int -> Int -> Int64 -> (Int, Integer)
trigReduce t e f = (fromInteger (zz `shiftR` 76), zz .&. (bit 76 - bit (77 - t)))
 where
  zz = (toInteger (bit 23 .|. f) * (invPi `shiftR` (273 - e))) .&. (bit 78 - 1)

-- | @sin (pi * r)@ for @r@ in @(0, 1/2]@ in units of 2^-77, with sign @s@,
-- using the polynomial at @r@ or, if @p0@, at 0
trigSinPi :: Multipliers -> Word32 -> Bool -> Integer -> Word32
trigSinPi mults s p0 r = s .|. roundAt (24 - em) (m * sinPoly mults j y)
 where
  -- r at 2^-27
  rr = fromInteger (r `shiftR` 50) :: Int64
  (j, y) | p0 = (0, 0)
         -- r = 1/2: the last segment at y = 2^18
         | rr >= bit 26 = (255, bit 18)
         | otherwise = (fromIntegral (rr `shiftR` 18), rr .&. (bit 18 - 1))
  -- r truncated to 26 significant bits: m * 2^em
  nb | rr /= 0 = 114 - countLeadingZeros rr
     | otherwise = 64 - countLeadingZeros (fromInteger r :: Int64)
  m | nb >= 26 = fromInteger (r `shiftR` (nb - 26))
    | otherwise = fromInteger (r `shiftL` (26 - nb))
  em = nb - 26 - 77

-- | P = sin (pi * r) / r at 2^-24 for @r = (j + y * 2^-18) / 512@. The
-- coefficients have their LSBs at 2^-27; T1 (from the top 13 bits of y), S1
-- and A are at 2^-28, and C0 has a rounding constant of 2^-25.
sinPoly :: Multipliers -> Int -> Int64 -> Int64
sinPoly mults j y = (2 * (c0 + 4) + t2) `shiftR` 4
 where
  (c2, c1, c0) = coefs sinTable j
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 5)) `shiftR` 13)
      in  (s1 * y) `shiftR` 18
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 5) + bit 12) `shiftR` 13)
          -- The partial products S1_i * y_j left out by the truncated
          -- multiplier of the base-10 exponential (see 'exp10Single'): i < 2
          -- and j < 11, 2 <= i < 6 and j < 7, 6 <= i < 11 and j < 2
          drop' = (s1 .&. 0x3) * (y .&. 0x7FF)
                + (s1 .&. 0x3C) * (y .&. 0x7F)
                + (s1 .&. 0x7C0) * (y .&. 0x3)
      -- Compensation constant 2^16 (2^-46)
      in  (s1 * y - drop' + bit 16) `shiftR` 18

-- | The IP's sine in single precision, on bit patterns
sinSingle :: Multipliers -> Word32 -> Word32
sinSingle mults w
  -- NaN, with the sign the reduction of its bit pattern gives
  | e == 255 = sgn .|. nanSingle
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  -- |x| < 2^-11, and |x| < 2^-10 with |x| / pi < 2^-12: x
  | e <= 115 || (e == 116 && q == 0 && z < bit 65) = w
  | z == 0 = w .&. bit 31
  -- Less than 2^-12 above an integer: the polynomial at 0
  | even q = trigSinPi mults sgn (z < bit 65) z
  | otherwise = trigSinPi mults sgn False (bit 76 - z)
 where
  (neg, e, f) = fields w
  -- With 54 fraction bits
  (q, z) = trigReduce 54 e f
  sgn = if neg /= (q >= 2) then bit 31 else 0

-- | The IP's cosine in single precision, on bit patterns
cosSingle :: Multipliers -> Word32 -> Word32
cosSingle mults w
  | e == 255 = nanSingle
  -- |x| < 2^-11, including zeros and subnormal numbers
  | e <= 115 = 0x3F800000
  -- Less than 2^-13 above an integer: +-1
  | even q && z < bit 64 = sgn .|. 0x3F800000
  | even q = trigSinPi mults sgn False (bit 76 - z)
  -- A half integer: +0
  | z == 0 = 0
  | otherwise = trigSinPi mults sgn False z
 where
  (_, e, f) = fields w
  -- With 55 fraction bits
  (q, z) = trigReduce 55 e f
  sgn = if q == 1 || q == 2 then bit 31 else 0

-- | The IP's tangent in single precision, on bit patterns: its sine divided
-- by its cosine as for 'tanPiSingle'
tanSingle :: Multipliers -> Word32 -> Word32
tanSingle mults w
  | e == 255 = nanSingle
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  -- |x| < 2^-11: x
  | e <= 115 = w
  | otherwise = tanQuotient mults (sinSingle mults w) (cosSingle mults w)
 where
  (_, e, _) = fields w

-- * Arctangent

-- | The table of 'atanSingle'
atanTable :: Table
atanTable = mkTable atanCoefficients
{-# NOINLINE atanTable #-}

-- | The reciprocal table of 'atanSingle'
atanRecipTable :: Table
atanRecipTable = mkTable atanRecipCoefficients
{-# NOINLINE atanRecipTable #-}

-- | The IP's arctangent in single precision, on bit patterns
atanSingle :: Multipliers -> Word32 -> Word32
atanSingle mults w
  -- The infinities give pi / 2 rounded down, NaN gives NaN
  | e == 255 = if f /= 0 then nanSingle else sgn .|. 0x3FC90FDA
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = sgn
  -- |x| < 2^-12: x
  | e <= 115 = w
  -- |x| <= 1: x * P(x), rounded once
  | e < 127 || e == 127 && f == 0 = sgn .|. roundAt (176 - e) (m * atanPoly mults xFix)
  | otherwise = sgn .|. atanLarge mults (e - 127) f
 where
  (_, e, f) = fields w
  sgn = w .&. bit 31
  m = bit 23 .|. f
  -- floor (|x| * 2^26)
  xFix | e >= 124 = m `shiftL` (e - 124)
       | otherwise = m `shiftR` (124 - e)

-- | @atan x@ for @x = (1 + f * 2^-23) * 2^k > 1@ (positive): pi / 2 - atan z
-- with z = 1 / x
atanLarge :: Multipliers -> Int -> Int64 -> Word32
atanLarge mults k f
  | d < bit 24 = (126 `shiftL` 23) .|. fromIntegral (d - bit 23)
  | otherwise = (127 `shiftL` 23) .|. fromIntegral ((d `shiftR` 1) - bit 23)
 where
  -- z = r * 2^-24 * 2^-k, r in (2^23, 2^24]
  r = atanRecip mults f
  -- The argument of the polynomial, floor (z * 2^26), but shifted by at most
  -- 12 places: for k > 12 the polynomial gets z * 2^(k - 12) (where it is
  -- one, or one unit below it), see Note [Bit-exact models]
  p = atanPoly mults ((r `shiftL` 2) `shiftR` min k 12)
  -- atan z = r * p at 2^-(50 + k); zero below 2^-23
  v | k >= 24 || k == 23 && r < bit 24 = 0
    | otherwise = r * p
  -- pi / 2 rounded up to 2^-24, minus atan z rounded down to 2^-24; the
  -- result is truncated
  d = 26353590 - (v `shiftR` (26 + k))

-- | P = atan r / r at 2^-26 for r = X * 2^-26 in [0, 1]: 256 segments, y the
-- low 18 bits of X; r = 1 is segment 255 with y = 2^18. The coefficients are
-- at 2^-29, T1 (from the top 13 bits of y) at 2^-30.
atanPoly :: Multipliers -> Int64 -> Int64
atanPoly mults xx = (2 * (c0 + 4) + t2) `shiftR` 4
 where
  (j, y) | xx >= bit 26 = (255, bit 18)
         | otherwise = (fromIntegral (xx `shiftR` 18), xx .&. (bit 18 - 1))
  (c2, c1, c0) = coefs atanTable j
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 5)) `shiftR` 12)
      in  (s1 * y) `shiftR` 18
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 5) + bit 11) `shiftR` 12)
          -- The truncated multiplier of the base-10 exponential
          dropped = (s1 .&. 0x3) * (y .&. 0x7FF)
                  + (s1 .&. 0x3C) * (y .&. 0x7F)
                  + (s1 .&. 0x7C0) * (y .&. 0x3)
      -- Compensation constant 2^16 (2^-32)
      in  (s1 * y - dropped + bit 16) `shiftR` 18

-- | The reciprocal @1 / (1 + f * 2^-23)@ at 2^-24, in (2^23, 2^24]: 256
-- segments, y the low 15 bits of f, coefficients at 2^-27, T1 (from the top
-- 12 bits of y), S1 and A at 2^-28
atanRecip :: Multipliers -> Int64 -> Int64
atanRecip mults f = (2 * c0 + 8 + t2) `shiftR` 4
 where
  (c2, c1, c0) = coefs atanRecipTable (fromIntegral (f `shiftR` 15))
  y = f .&. 0x7FFF
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 3)) `shiftR` 12)
      in  (s1 * y) `shiftR` 15
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((2 * c2 * (y `shiftR` 3) + bit 11) `shiftR` 12)
          -- The truncated multiplier of the base-2 logarithm
          dropped = (s1 .&. 0x7) * (y .&. 0xFF) + (s1 .&. 0x18) * (y .&. 0x3F)
      -- Compensation constant 2^13 (2^-30)
      in  (s1 * y - dropped + bit 13) `shiftR` 15

-- | The IP's arctangent divided by pi in single precision, on bit patterns:
-- 'atanSingle' times 1 / pi rounded to single precision (0x3EA2F983), rounded
-- to nearest, ties away from zero, with results below the smallest normal
-- number flushed to zero. On 18x18 families the product is the truncated
-- multiplier of the cosine of pi times x, with 1 / pi as the 25-bit
-- 0xA2F983 * 2; any compensation constant up to 2^13 gives the same results
-- on all inputs, the model adds none.
atanPiSingle :: Multipliers -> Word32 -> Word32
atanPiSingle mults w
  | e == 255 && f /= 0 = nanSingle
  | ea == 0 = sa
  | be <= 0 = sa
  | otherwise = sa .|. (fromIntegral be `shiftL` 23) .|. fromIntegral (q .&. 0x7FFFFF)
 where
  (_, e, f) = fields w
  a = atanSingle mults w
  sa = a .&. bit 31
  (_, ea, fa) = fields a
  ma = bit 23 .|. fa
  -- The product of the significands at 2^-47 (relative to the result's
  -- exponent before normalisation), 2^47 <= p < 2^49
  p = case mults of
    FullMultipliers -> ma * (2 * 0xA2F983)
    TruncatedMultipliers -> truncatedProduct24x25 ma (2 * 0xA2F983)
  sh = if p >= bit 48 then 25 else 24
  q0 = (p + bit (sh - 1)) `shiftR` sh
  (q, carry) = if q0 == bit 24 then (q0 `shiftR` 1, 1) else (q0, 0)
  be = ea - 2 + (sh - 24) + carry

-- * Two-argument arctangent

-- | @floor (pi * 2^200)@: pi to 200 fraction bits
piFixed :: Integer
piFixed = 0x3243f6a8885a308d313198a2e03707344a4093822299f31d008

-- | pi rounded down to single precision
piDown :: Word32
piDown = 0x40490FDA

-- | pi / 2 - 1, as the IP gives it for some combinations of zeros and
-- subnormal numbers
halfPiMinusOne :: Word32
halfPiMinusOne = 0x3F121FB4

-- | The IP's two-argument arctangent @atan2 y x@ in single precision, on bit
-- patterns: the arctangent ('atanSingle') of the quotient @|y| / |x|@, computed
-- like the tangent of pi times x divides the sine by the cosine, subtracted
-- from pi for a negative @x@, with the sign of @y@
atan2Single :: Multipliers -> Word32 -> Word32 -> Word32
atan2Single mults y x
  | ey == 255 && fy /= 0 || ex == 255 && fx /= 0 = nanSingle
  | ey == 0 || ex == 0 = sy .|. zeroish
  | negX = sy .|. piMinus t
  | otherwise = sy .|. t
 where
  (_, ey, fy) = fields y
  (negX, ex, fx) = fields x
  sy = y .&. bit 31
  t | ey == 255 && ex == 255 = 0x3F490FDB
    | ey == 255 = 0x3FC90FDA
    | ex == 255 = 0
    | otherwise = atanSingle mults (atan2Quotient mults ey fy ex fx)
  -- Zeros and subnormal numbers: y = 0 gives 0 or pi as usual, x = 0 gives
  -- pi / 2 rounded up, but the IP's results depend on whether the other input
  -- is a zero or a subnormal number
  zeroish
    | ey == 0 && ex /= 0 = if negX then piDown else 0
    | ey == 0 && fy == 0 =
        if negX then piDown else if fx /= 0 then halfPiMinusOne else 0
    | ey == 0 = if negX || fx == 0 then piMinus halfPiMinusOne else halfPiMinusOne
    | fx /= 0 && not negX = 0x3FC90FDA
    | otherwise = 0x3FC90FDB

-- | The quotient @|y| / |x|@ of two normal numbers, from their biased
-- exponents and fractions, as the tangent of pi times x computes it
-- ('tanQuotient'): rounded to nearest, ties away from zero, +inf above the
-- largest finite number and +0 below the smallest normal number
atan2Quotient :: Multipliers -> Int -> Int64 -> Int -> Int64 -> Word32
atan2Quotient mults ey fy ex fx
  | be >= 255 = infSingle
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 23) .|. fromIntegral (o .&. 0x7FFFFF)
 where
  sm = bit 23 .|. fy
  r = tanPiRecip mults fx
  p = case mults of
    TruncatedMultipliers | fx /= 0 -> tanPiProduct sm r
    _ -> sm * r
  -- p at 2^-g, rounded to 24 significant bits
  g = 49 - ey + ex
  d = 64 - countLeadingZeros p - 24
  o0 | d <= 0 = p `shiftL` negate d
     | otherwise = (p + bit (d - 1)) `shiftR` d
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, d - g + 151)
          | otherwise = (o0, d - g + 150)

-- | pi minus a non-negative number below pi / 2, truncated to single
-- precision
piMinus :: Word32 -> Word32
piMinus t = (fromIntegral (len - 74) `shiftL` 23) .|. (fromInteger (v `shiftR` (len - 24)) .&. 0x7FFFFF)
 where
  (_, e, f) = fields t
  -- At 2^-200
  tv | e == 0 = 0
     | otherwise = toInteger (bit 23 .|. f) `shiftL` (e + 50)
  v = piFixed - tv
  -- pi / 2 <= v * 2^-200 < 4
  len = if v >= bit 201 then 202 else 201 :: Int

-- * Power

-- | A table of cubic coefficients as four arrays: C0, C1, C2, C3
data Table4 = Table4 !(UArray Int Int64) !(UArray Int Int64) !(UArray Int Int64) !(UArray Int Int64)

mkTable4 :: [(Integer, Integer, Integer, Integer)] -> Table4
mkTable4 cs =
  Table4 (arr [c | (c, _, _, _) <- cs]) (arr [c | (_, c, _, _) <- cs])
         (arr [c | (_, _, c, _) <- cs]) (arr [c | (_, _, _, c) <- cs])
 where
  arr xs = listArray (0, length cs - 1) (map fromIntegral xs)

-- | Look up the coefficients (C0, C1, C2, C3) of a segment. Callers guarantee
-- that the index is in range.
coefs4 :: Table4 -> Int -> (Integer, Integer, Integer, Integer)
coefs4 (Table4 c0 c1 c2 c3) i =
  (toInteger (unsafeAt c0 i), toInteger (unsafeAt c1 i), toInteger (unsafeAt c2 i), toInteger (unsafeAt c3 i))
{-# INLINE coefs4 #-}

powLogTable, powExpTable :: Table4
powLogTable = mkTable4 powLogCoefficients
{-# NOINLINE powLogTable #-}
powExpTable = mkTable4 powExpCoefficients
{-# NOINLINE powExpTable #-}

-- | The IP's power function @x ** y@ in single precision, on bit patterns
powSingle :: Multipliers -> Word32 -> Word32 -> Word32
powSingle m x y
  | xe == 0xFF && xf /= 0 || ye == 0xFF && yf /= 0 = nanSingle
  | xe == 0 = if ye == 0 then nanSingle else if ys then infSingle else 0
  | xs = nanSingle
  | xe == 0xFF = if ye == 0 then nanSingle else if ys then 0 else infSingle
  | ye == 0 = one
  | x == one = if ye == 0xFF then nanSingle else powOfOne
  | ye == 0xFF = if (x > one) /= ys then infSingle else 0
  | otherwise = powResult (powExp m (powProduct y (powLog m x)))
 where
  one = 0x3F800000
  xs = testBit x 31
  ys = testBit y 31
  xe = (x `shiftR` 23) .&. 0xFF
  ye = (y `shiftR` 23) .&. 0xFF
  xf = x .&. 0x7FFFFF
  yf = y .&. 0x7FFFFF
  -- The logarithm of one is 2^-57 inside the IP; only an overflow or underflow
  -- of the exponential of y * 2^-57 shows in the result.
  powOfOne
    | ye' >= 58
    , r <- powExp m (mkWide ys (ye' - 57) (toInteger yf `shiftL` 10))
    , r == wideInf || r == 0
    = powResult r
    | otherwise = one
   where
    ye' | ye <= 192 = fromIntegral ye
        | ye == 254 && ys = 190
        | otherwise = 0 :: Int

-- ** The internal floating point format
-- A 42 bit floating point number: sign, 8 bit biased exponent, 33 bit fraction.

mkWide :: Bool -> Int -> Integer -> Word64
mkWide s e f = (if s then bit 41 else 0) .|. (fromIntegral e `shiftL` 33) .|. fromInteger (f .&. (bit 33 - 1))

wideInf :: Word64
wideInf = 0xFF `shiftL` 33

-- | The result: the exponential's 42 bit result truncated to single precision
powResult :: Word64 -> Word32
powResult r = fromIntegral (r `shiftR` 10)

-- | Round a positive @v * 2^ve@ to 34 significant bits, to nearest with ties
-- away from zero
round34 :: Bool -> Integer -> Int -> Word64
round34 s v ve
  | n > 34 =
      let sh = n - 34
          r = (v + bit (sh - 1)) `shiftR` sh
      in if testBit r 34 then mkWide s (ve + sh + 1 + 33 + 127) (r `shiftR` 1)
                         else mkWide s (ve + sh + 33 + 127) r
  | otherwise = mkWide s (ve - (34 - n) + 33 + 127) (v `shiftL` (34 - n))
 where
  n = integerLog2' v + 1

integerLog2' :: Integer -> Int
integerLog2' v = go 0 v where go k w = if w <= 1 then k else go (k + 1) (w `shiftR` 1)

-- ** Logarithm

-- | The IP's natural logarithm of a positive normal number other than one, to
-- 34 bits
powLog :: Multipliers -> Word32 -> Word64
powLog m x
  | e == 127 = round34 False (f * powLogPoly m idx0 t) (-23 - 33)
  | e == 126 = round34 True ((bit 23 - f) * powLogPoly m idx1 t) (-24 - 32)
  | otherwise =
      let s = toInteger (e - 127) * ln2Up * bit 20 + f * powLogPoly m idx0 t
      in round34 (s < 0) (abs s) (-56)
 where
  e = fromIntegral ((x `shiftR` 23) .&. 0xFF) :: Int
  f = toInteger (x .&. 0x7FFFFF)
  t = f .&. 0x7FFF
  idx0 = fromInteger (f `shiftR` 15)
  idx1 = 256 + fromInteger (((f `shiftR` 22) .&. 1) * 128 + ((f `shiftR` 15) .&. 127))
  -- ln 2 rounded up to 36 fraction bits
  ln2Up = 0xB17217F7E

-- | @ln (1 + z) / z@ on a segment, truncated to 33 significant bits: on the
-- 2^-33 grid for the regions with @P < 1@ (indices below 256), on 2^-32 for
-- the others
powLogPoly :: Multipliers -> Int -> Integer -> Integer
powLogPoly m idx t = (c0 `shiftL` k + t3 + bit (k + 2)) `shiftR` 6
 where
  (c0, c1, c2, c3) = coefs4 powLogTable idx
  -- coefficient LSBs 2^-37 (C3 2^-38) to the datapath's 2^-39 (2^-38)
  k = if idx < 256 then 2 else 1
  t14 = t `shiftR` 1
  (_, _, t3) = case m of
    FullMultipliers ->
      let p1 = (c3 * t14) `shiftR` (15 - k)
          p2 = ((c2 `shiftL` k + p1) * t) `shiftR` 15
          p3 = ((c1 `shiftL` k + p2) * t) `shiftR` 15
      in (p1, p2, p3)
    TruncatedMultipliers ->
      let p1 = (c3 * (k' * t14) + bit 13) `shiftR` 14
          s1 = c2 `shiftL` k + p1
          p2 = (logStaircase s1 t + bit 14) `shiftR` 15
          p3 = ((c1 `shiftL` k + p2) * t + bit 13) `shiftR` 15
      in (p1, p2, p3)
  k' = if idx < 256 then 2 else 1


-- | The truncated multiplier of the logarithm: S1 * t without the partial
-- products S1_i * t_j with (i < 7, j < 5) or (i < 4, j < 8)
logStaircase :: Integer -> Integer -> Integer
logStaircase s1 t =
    s1 * (t `shiftR` 8 `shiftL` 8)
  + (s1 `shiftR` 4 `shiftL` 4) * (t .&. 0xE0)
  + (s1 `shiftR` 7 `shiftL` 7) * (t .&. 0x1F)

-- ** Product

-- | @y * L@ with the exact product's significand truncated to 34 bits
powProduct :: Word32 -> Word64 -> Word64
powProduct y l
  | le == 0 = mkWide s 0 0
  | be <= 0 = mkWide s 0 0
  | be >= 255 = mkWide s 255 0
  | otherwise = mkWide s be (p `shiftR` (n - 34))
 where
  s = testBit y 31 /= testBit l 41
  ye = fromIntegral ((y `shiftR` 23) .&. 0xFF) :: Int
  le = fromIntegral ((l `shiftR` 33) .&. 0xFF) :: Int
  p = toInteger ((y .&. 0x7FFFFF) .|. bit 23) * toInteger ((l .&. (bit 33 - 1)) .|. bit 33)
  n = integerLog2' p + 1
  be = (ye - 150) + (le - 160) + n - 34 + 33 + 127

-- ** Exponential

-- | The IP's exponential of a 42 bit number, to 34 bits
powExp :: Multipliers -> Word64 -> Word64
powExp m a
  | e == 0xFF = if fr /= 0 then wideInf .|. 1 else if s then 0 else wideInf
  | sh > 9 = if s then 0 else wideInf
  | ex >= 255 = wideInf
  | ex <= 0 = 0
  | otherwise = mkWide False ex o'
 where
  s = testBit a 41
  e = fromIntegral ((a `shiftR` 33) .&. 0xFF) :: Int
  fr = toInteger (a .&. (bit 33 - 1))
  mant = fr .|. bit 33
  sh = e - 126
  -- x * 2^34, rounded down
  xFix | sh >= 0 = (if s then negate else id) (mant `shiftL` sh)
       | s = negate ((mant + bit (-sh) - 1) `shiftR` (-sh))
       | otherwise = mant `shiftR` (-sh)
  -- k: the significand times 1/ln 2 (rounded up, 2^-40) floored at 2^-37
  q = (mant * invLn2) `shiftR` 36
  k = (if s then negate q else q) `shiftR` (37 - (e - 127))
  r = max 0 (4 * xFix - k * ln2Down)
  j = fromInteger (r `shiftR` 28)
  y30 = (r .&. (bit 28 - 1)) `shiftL` 2
  o = powExpPoly m j y30 `shiftR` 7
  (o', ex0) = if testBit o 34 then (o `shiftR` 1, k + 128) else (o, k + 127)
  ex = fromInteger ex0
  invLn2 = 0x171547652B9    -- floor(2^40 / ln 2) + 1
  ln2Down = 0xB17217F7D     -- ln 2 rounded down to 36 fraction bits

-- | @exp ((j + y / 2^28) / 256)@ for a 28 bit @y@ (given as @y30 = 4 y@), on
-- the 2^-40 grid, plus the rounding constant 2^-36
powExpPoly :: Multipliers -> Int -> Integer -> Integer
powExpPoly m j y30 = (c0 `shiftL` 2) + t3 + 16
 where
  (c0, c1, c2, c3) = coefs4 powExpTable j
  y1 = y30 `shiftR` 15
  y2 = y30 `shiftR` 6
  t3 = case m of
    FullMultipliers ->
      let s1 = (c2 `shiftL` 1) + ((c3 * y1) `shiftR` 15)
          s2 = (c1 `shiftL` 1) + ((s1 * y2) `shiftR` 24)
      in truncMul expCut3Full s2 ((y30 `shiftR` 2) .&. complement 3) `shiftR` 27
    TruncatedMultipliers ->
      let s1 = (c2 `shiftL` 1) + ((c3 * y1 + bit 14) `shiftR` 15)
          s2 = (c1 `shiftL` 1) + (truncMul expCut2 s1 y2 + bit 23) `shiftR` 24
      in (truncMul expCut3 s2 ((y30 `shiftR` 2) .&. complement 3) + bit 26) `shiftR` 27

-- | A truncated multiplier: @a * b@ without the partial products @a_i * b_j@
-- with @i@ below the cut of row @j@
truncMul :: [Int] -> Integer -> Integer -> Integer
truncMul cuts a b = sum [ ((a `shiftR` c) `shiftL` c) `shiftL` jj | (jj, c) <- zip [0..] cuts, testBit b jj ]

expCut2, expCut3, expCut3Full :: [Int]
expCut2 = replicate 8 9 ++ replicate 16 0
expCut3 = replicate 4 18 ++ replicate 8 10 ++ replicate 9 1 ++ replicate 7 0
expCut3Full = replicate 8 17 ++ replicate 3 14 ++ replicate 17 0
