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
 *   <op> <a> <b>
 *
 * where <op> is one of add, sub, mul, div, cmp, u32, s32 and <a> and <b> are
 * 32-bit operands in hexadecimal. For add, sub, mul, div and cmp the operands
 * are the bit patterns of single precision floats. For u32 and s32, <a> is the
 * 32-bit integer to convert and <b> is ignored. For every request it prints
 *
 *   <result> <exceptions>
 *
 * where <result> is the bit pattern of the resulting single precision float
 * in hexadecimal, or for cmp the 4-bit condition code (bit 0: equal, bit 1:
 * less than, bit 2: greater than, bit 3: unordered). <exceptions> are the
 * exception flags returned by the C model in decimal (bit 0: underflow, bit 1:
 * overflow, bit 2: invalid operation, bit 3: divide by zero, bit 4: operation
 * not supported); they are informational only.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "floating_point_v7_1_bitacc_cmodel.h"

/* Single precision, as instantiated by Clash.Cores.Xilinx.Floating:
   C_A_Exponent_Width = 8 and C_A_Fraction_Width = 24 (including the hidden
   bit). The C model always rounds to nearest, ties to even, like the IP. */
#define EXPONENT_WIDTH 8
#define MANTISSA_WIDTH 24

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

int main(void) {
  char op[8];
  unsigned int a, b;
  xip_fpo_t x, y, r;

  xip_fpo_inits2(EXPONENT_WIDTH, MANTISSA_WIDTH, x, y, r, (xip_fpo_ptr) 0);
  printf("ready %s\n", xip_fpo_get_version());
  fflush(stdout);

  while (scanf("%7s %x %x", op, &a, &b) == 3) {
    uint32_t result;
    xip_fpo_exc_t exc;

    if (!strcmp(op, "u32") || !strcmp(op, "s32")) {
      float f;
      if (op[0] == 'u') {
        exc = xip_fpo_fixtoflt_flt_uint(&f, (unsigned int) a);
      } else {
        exc = xip_fpo_fixtoflt_flt_int(&f, (int) (int32_t) a);
      }
      result = float_to_bits(f);
    } else {
      /* Assigning a subnormal number flushes it to a zero of the same sign,
         like the IP does on its inputs. */
      xip_fpo_set_flt(x, bits_to_float(a));
      xip_fpo_set_flt(y, bits_to_float(b));

      if (!strcmp(op, "cmp")) {
        int cc;
        exc = xip_fpo_condcode(&cc, x, y);
        result = (uint32_t) cc;
      } else {
        if (!strcmp(op, "add")) {
          exc = xip_fpo_add(r, x, y);
        } else if (!strcmp(op, "sub")) {
          exc = xip_fpo_sub(r, x, y);
        } else if (!strcmp(op, "mul")) {
          exc = xip_fpo_mul(r, x, y);
        } else if (!strcmp(op, "div")) {
          exc = xip_fpo_div(r, x, y);
        } else {
          printf("error: unknown operation '%s'\n", op);
          return 1;
        }
        result = float_to_bits(xip_fpo_get_flt(r));
      }
    }

    printf("%08x %d\n", (unsigned int) result, (int) exc);
    fflush(stdout);
  }

  xip_fpo_clears(x, y, r, (xip_fpo_ptr) 0);
  return 0;
}
