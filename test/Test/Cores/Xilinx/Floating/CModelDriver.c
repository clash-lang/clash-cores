/*
 * Copyright  :  (C) 2026, QBayLogic B.V.
 * License    :  BSD2 (see the file LICENSE)
 * Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
 *
 * Driver for the bit-accurate C model of the Xilinx Floating-Point LogiCORE IP
 * v7.1. Used by test/Test/Cores/Xilinx/Floating.hs, which embeds this file,
 * compiles it against the C model at test time and talks to it over
 * stdin/stdout. See that module for how the C model is located.
 *
 * Protocol. After start-up the driver prints
 *
 *   ready <C model version>
 *
 * and then reads requests, one per line:
 *
 *   <precision> <op> <a> <b>
 *
 * where <precision> is one of half, single, double, <op> is one of add, sub,
 * mul, div, cmp, sqrt, abs, recip, rsqrt, exp, log, u32, s32, u64, s64, tos32,
 * tohalf, tosingle, todouble, and <a> and <b> are operands in hexadecimal. For add, sub, mul,
 * div and cmp the operands are the bit patterns of floating point numbers of
 * the given precision. The other operations have one operand, <a>, and ignore
 * <b>:
 *
 * - sqrt, abs, recip (reciprocal), rsqrt (reciprocal square root), exp
 *   (exponential), log (natural logarithm): <a> is the bit pattern of a
 *   floating point number of the given precision, and so is the result. exp
 *   uses the C model's function for a single number, which recomputes its
 *   tables on every call: it takes about 0.1 ms in single precision and 0.3 ms
 *   in double precision. (Its functions for arrays of floats and doubles return
 *   NaNs with the sign bit set, unlike the IP.)
 *
 * - u32, s32, u64, s64: <a> is the 32-bit or 64-bit integer to convert to the
 *   given precision.
 *
 * - tos32: <a> is the bit pattern of a floating point number of the given
 *   precision, converted to a 32-bit signed integer.
 *
 * - tohalf, tosingle, todouble: <a> is the bit pattern of a floating point
 *   number of the given precision, converted to the precision in the name of
 *   the operation.
 *
 * The IP cannot convert between integers and half precision, so neither can
 * the driver. For every request it prints
 *
 *   <result> <exceptions>
 *
 * where <result> is the bit pattern of the resulting floating point number in
 * hexadecimal, for tos32 the bit pattern of the 32-bit integer, or for cmp the
 * 4-bit condition code (bit 0: equal, bit 1: less than, bit 2: greater than,
 * bit 3: unordered). <exceptions> are the exception
 * flags returned by the C model in decimal (bit 0: underflow, bit 1: overflow,
 * bit 2: invalid operation, bit 3: divide by zero, bit 4: operation not
 * supported); they are informational only. Invalid requests get a single line
 * starting with "error".
 *
 * Getting bits in and out of the C model. The C model represents numbers in
 * any precision as MPFR numbers with a limited exponent range, and converts
 * them from and to C's float and double:
 *
 * - Assigning a number below the smallest normal number of the precision
 *   (after rounding it to the precision) flushes it to a zero of the same
 *   sign, like the IP does on its inputs. Every half and single precision
 *   number is exactly representable as a double, so the driver assigns half
 *   precision numbers as doubles. Single precision numbers are assigned as
 *   floats.
 *
 * - The C model's NaNs have no sign or payload. xip_fpo_get_flt and
 *   xip_fpo_get_d return a positive quiet NaN without payload (0x7fc00000 and
 *   0x7ff8000000000000): they clear the sign bit of MPFR's NaN. There is no
 *   such function for half precision: the driver reads half precision results
 *   as a double (exact for every half precision number) and converts that to
 *   half precision bits itself. For NaNs it keeps the most significant
 *   fraction bits, like the IEEE conversions do, which turns the C model's NaN
 *   into 0x7e00.
 *
 * Absolute value. The IP only clears the sign bit: unlike the other
 * operations, it passes subnormal numbers and NaN payloads through (checked in
 * Vivado). So do the C model's xip_fpo_abs_flt and xip_fpo_abs_d, which work
 * on the bits of a float or double. The generic xip_fpo_abs cannot see
 * subnormal numbers or NaN payloads (see above), and there is no half
 * precision variant. For half precision, the driver therefore widens the
 * number to single precision, exactly, keeping NaN payloads in the most
 * significant fraction bits, applies xip_fpo_abs_flt, and narrows the result
 * back.
 */

#include <inttypes.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "floating_point_v7_1_bitacc_cmodel.h"

/* The precisions supported by Clash.Cores.Xilinx.Floating. The exponent width
   and fraction width (including the hidden bit) are C_A_Exponent_Width and
   C_A_Fraction_Width in the IP's settings. The C model always rounds to
   nearest, ties to even, like the IP. */
enum precision { HALF, SINGLE, DOUBLE };

static const struct {
  const char *name;
  int exponent_width;
  int fraction_width;
} precisions[] = {
  [HALF]   = { "half",   5,  11 },
  [SINGLE] = { "single", 8,  24 },
  [DOUBLE] = { "double", 11, 53 },
};

#define N_PRECISIONS (sizeof precisions / sizeof precisions[0])

/* Width of the bit pattern of a number in the given precision */
static int total_width(enum precision p) {
  return precisions[p].exponent_width + precisions[p].fraction_width;
}

static float bits_to_float(uint32_t bits) {
  float f;
  memcpy(&f, &bits, sizeof f);
  return f;
}

static uint32_t float_to_bits(float f) {
  uint32_t bits;
  memcpy(&bits, &f, sizeof bits);
  return bits;
}

static double bits_to_double(uint64_t bits) {
  double d;
  memcpy(&d, &bits, sizeof d);
  return d;
}

static uint64_t double_to_bits(double d) {
  uint64_t bits;
  memcpy(&bits, &d, sizeof bits);
  return bits;
}

/* Value of a half precision number, exactly. The C model has no NaN
   payloads, so all NaNs map to the same NaN. */
static double half_to_double(uint16_t h) {
  int exponent = (h >> 10) & 0x1f;
  unsigned int fraction = h & 0x3ff;
  double magnitude;
  if (exponent == 0x1f) {
    magnitude = fraction ? NAN : INFINITY;
  } else if (exponent == 0) {
    magnitude = ldexp(fraction, -24);
  } else {
    magnitude = ldexp(fraction | 0x400, exponent - 25);
  }
  return (h & 0x8000) ? -magnitude : magnitude;
}

/* Bit pattern of a double as a half precision number. Returns 0 if it is not
   exactly representable. NaNs keep their sign and the 10 most significant bits
   of their fraction (and stay NaNs). */
static int double_to_half(double d, uint16_t *h) {
  uint64_t bits = double_to_bits(d);
  uint16_t sign = (uint16_t) ((bits >> 48) & 0x8000);
  double magnitude = fabs(d);
  int e;

  if (isnan(d)) {
    uint16_t fraction = (uint16_t) ((bits >> 42) & 0x3ff);
    *h = sign | 0x7c00 | (fraction ? fraction : 0x200);
    return 1;
  }
  if (isinf(d)) {
    *h = sign | 0x7c00;
    return 1;
  }
  if (magnitude == 0) {
    *h = sign;
    return 1;
  }

  /* magnitude = m * 2^e with m in [0.5, 1) */
  double m = frexp(magnitude, &e);
  if (e - 1 > 15) {
    return 0;
  } else if (e - 1 >= -14) {
    double significand = ldexp(m, 11);  /* in [1024, 2048) */
    if (significand != floor(significand)) return 0;
    *h = sign | (uint16_t) ((e - 1 + 15) << 10) | ((uint16_t) significand & 0x3ff);
  } else {
    double fraction = ldexp(magnitude, 24);  /* multiple of the smallest subnormal */
    if (fraction != floor(fraction)) return 0;
    *h = sign | (uint16_t) fraction;
  }
  return 1;
}

/* Bit pattern of a half precision number widened to single precision. Exact,
   also for subnormal numbers (which are normal in single precision) and NaNs,
   whose fraction moves to the most significant fraction bits. */
static uint32_t half_to_single_bits(uint16_t h) {
  if ((h & 0x7c00) == 0x7c00 && (h & 0x3ff) != 0) {
    return ((uint32_t) (h & 0x8000) << 16) | 0x7f800000 | ((uint32_t) (h & 0x3ff) << 13);
  }
  return float_to_bits((float) half_to_double(h));
}

/* Inverse of half_to_single_bits, for numbers it can produce */
static uint16_t single_to_half_bits(uint32_t s) {
  uint16_t h = 0;
  if ((s & 0x7f800000) == 0x7f800000 && (s & 0x7fffff) != 0) {
    return (uint16_t) (((s >> 16) & 0x8000) | 0x7c00 | ((s >> 13) & 0x3ff));
  }
  double_to_half((double) bits_to_float(s), &h);
  return h;
}

/* Find a precision by name */
static int find_precision(const char *name, enum precision *p) {
  for (unsigned int i = 0; i < N_PRECISIONS; i++) {
    if (!strcmp(name, precisions[i].name)) {
      *p = (enum precision) i;
      return 1;
    }
  }
  return 0;
}

/* Assign the number with the given bit pattern */
static void set_bits(enum precision p, xip_fpo_ptr x, uint64_t bits) {
  switch (p) {
    case HALF:   xip_fpo_set_d(x, half_to_double((uint16_t) bits)); break;
    case SINGLE: xip_fpo_set_flt(x, bits_to_float((uint32_t) bits)); break;
    case DOUBLE: xip_fpo_set_d(x, bits_to_double(bits)); break;
  }
}

/* Bit pattern of a number. Returns 0 if it cannot be represented. */
static int get_bits(enum precision p, xip_fpo_srcptr x, uint64_t *bits) {
  uint16_t h;
  switch (p) {
    case HALF:
      if (!double_to_half(xip_fpo_get_d(x), &h)) return 0;
      *bits = h;
      return 1;
    case SINGLE:
      *bits = float_to_bits(xip_fpo_get_flt(x));
      return 1;
    case DOUBLE:
      *bits = double_to_bits(xip_fpo_get_d(x));
      return 1;
  }
  return 0;
}

/* Print an error reply */
static void fail(const char *format, ...) {
  va_list args;
  va_start(args, format);
  printf("error: ");
  vprintf(format, args);
  printf("\n");
  va_end(args);
  fflush(stdout);
}

/* Convert an integer to the given precision: u32, s32, u64 or s64. Returns 0
   if the precision does not support it. */
static int convert_integer(enum precision p, const char *op, uint64_t a,
                           uint64_t *result, xip_fpo_exc_t *exc) {
  float f;
  double d;
  if (p == SINGLE) {
    if (!strcmp(op, "u32")) {
      *exc = xip_fpo_fixtoflt_flt_uint(&f, (unsigned int) a);
    } else if (!strcmp(op, "s32")) {
      *exc = xip_fpo_fixtoflt_flt_int(&f, (int) (int32_t) (uint32_t) a);
    } else if (!strcmp(op, "u64")) {
      *exc = xip_fpo_fixtoflt_flt_uint64(&f, (unsigned long long) a);
    } else {
      *exc = xip_fpo_fixtoflt_flt_int64(&f, (long long) (int64_t) a);
    }
    *result = float_to_bits(f);
    return 1;
  } else if (p == DOUBLE) {
    if (!strcmp(op, "u32")) {
      *exc = xip_fpo_fixtoflt_d_uint(&d, (unsigned int) a);
    } else if (!strcmp(op, "s32")) {
      *exc = xip_fpo_fixtoflt_d_int(&d, (int) (int32_t) (uint32_t) a);
    } else if (!strcmp(op, "u64")) {
      *exc = xip_fpo_fixtoflt_d_uint64(&d, (unsigned long long) a);
    } else {
      *exc = xip_fpo_fixtoflt_d_int64(&d, (long long) (int64_t) a);
    }
    *result = double_to_bits(d);
    return 1;
  }
  return 0;
}

/* Absolute value of the number with the given bit pattern, see the comment at
   the top of this file */
static xip_fpo_exc_t absolute(enum precision p, uint64_t a, uint64_t *result) {
  xip_fpo_exc_t exc = 0;
  float f;
  double d;
  switch (p) {
    case HALF:
      exc = xip_fpo_abs_flt(&f, bits_to_float(half_to_single_bits((uint16_t) a)));
      *result = single_to_half_bits(float_to_bits(f));
      break;
    case SINGLE:
      exc = xip_fpo_abs_flt(&f, bits_to_float((uint32_t) a));
      *result = float_to_bits(f);
      break;
    case DOUBLE:
      exc = xip_fpo_abs_d(&d, bits_to_double(a));
      *result = double_to_bits(d);
      break;
  }
  return exc;
}

int main(void) {
  char precision_name[8], op[16];
  uint64_t a, b;
  xip_fpo_t x[N_PRECISIONS], y[N_PRECISIONS], r[N_PRECISIONS];
  xip_fpo_fix_t fix32;

  for (unsigned int p = 0; p < N_PRECISIONS; p++) {
    xip_fpo_inits2(precisions[p].exponent_width, precisions[p].fraction_width,
                   x[p], y[p], r[p], (xip_fpo_ptr) 0);
  }
  /* A 32-bit signed integer: 32 integer bits, no fractional bits */
  xip_fpo_fix_init2(fix32, 32, 0);
  printf("ready %s\n", xip_fpo_get_version());
  fflush(stdout);

  while (scanf("%7s %15s %" SCNx64 " %" SCNx64, precision_name, op, &a, &b) == 4) {
    uint64_t result = 0;
    xip_fpo_exc_t exc = 0;
    enum precision p, target;
    int width;

    if (!find_precision(precision_name, &p)) {
      fail("unknown precision '%s'", precision_name);
      continue;
    }
    width = total_width(p);

    if (!strcmp(op, "u32") || !strcmp(op, "s32") || !strcmp(op, "u64") || !strcmp(op, "s64")) {
      if (!convert_integer(p, op, a, &result, &exc)) {
        fail("integer conversions not supported in %s precision", precision_name);
        continue;
      }
    } else if (width < 64 && ((a >> width) | (b >> width)) != 0) {
      fail("operands wider than the %s precision", precision_name);
      continue;
    } else if (!strcmp(op, "abs")) {
      exc = absolute(p, a, &result);
    } else {
      set_bits(p, x[p], a);
      set_bits(p, y[p], b);
      target = p;

      if (!strcmp(op, "cmp")) {
        int cc;
        exc = xip_fpo_condcode(&cc, x[p], y[p]);
        result = (uint64_t) cc;
      } else if (!strcmp(op, "tos32")) {
        if (p == HALF) {
          fail("tos32 not supported in %s precision", precision_name);
          continue;
        }
        exc = xip_fpo_flttofix(fix32, x[p]);
        result = (uint32_t) xip_fpo_fix_get_si(fix32);
      } else {
        if (!strcmp(op, "add")) {
          exc = xip_fpo_add(r[p], x[p], y[p]);
        } else if (!strcmp(op, "sub")) {
          exc = xip_fpo_sub(r[p], x[p], y[p]);
        } else if (!strcmp(op, "mul")) {
          exc = xip_fpo_mul(r[p], x[p], y[p]);
        } else if (!strcmp(op, "div")) {
          exc = xip_fpo_div(r[p], x[p], y[p]);
        } else if (!strcmp(op, "sqrt")) {
          exc = xip_fpo_sqrt(r[p], x[p]);
        } else if (!strcmp(op, "recip")) {
          exc = xip_fpo_rec(r[p], x[p]);
        } else if (!strcmp(op, "rsqrt")) {
          exc = xip_fpo_recsqrt(r[p], x[p]);
        } else if (!strcmp(op, "exp")) {
          exc = xip_fpo_exp(r[p], x[p]);
        } else if (!strcmp(op, "log")) {
          exc = xip_fpo_log(r[p], x[p]);
        } else if (!strncmp(op, "to", 2) && find_precision(op + 2, &target)) {
          exc = xip_fpo_flttoflt(r[target], x[p]);
        } else {
          fail("unknown operation '%s'", op);
          continue;
        }
        if (!get_bits(target, r[target], &result)) {
          fail("result not representable in %s precision", precisions[target].name);
          continue;
        }
      }
    }

    printf("%" PRIx64 " %d\n", result, (int) exc);
    fflush(stdout);
  }

  for (unsigned int p = 0; p < N_PRECISIONS; p++) {
    xip_fpo_clears(x[p], y[p], r[p], (xip_fpo_ptr) 0);
  }
  xip_fpo_fix_clear(fix32);
  return 0;
}
