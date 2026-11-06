/*
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Calls Xilinx's bit-accurate C model of the Fast Fourier Transform IP v9.1.
  The C model passes structures by value, which Haskell's FFI cannot do, so
  test/Test/Cores/Xilinx/Fft.hs calls the C model through these functions. They
  take the C model's functions as pointers (looked up with dlsym), so this file
  does not need the C model's header or library at compile time. The structures
  below follow xfft_v9_1_bitacc_cmodel.h, which ships with the C model.
*/

struct xfft_generics {
  int C_NFFT_MAX;
  int C_ARCH;
  int C_HAS_NFFT;
  int C_USE_FLT_PT;
  int C_INPUT_WIDTH;
  int C_TWIDDLE_WIDTH;
  int C_HAS_SCALING;
  int C_HAS_BFP;
  int C_HAS_ROUNDING;
};

struct xfft_inputs {
  int nfft;
  double *xn_re;
  int xn_re_size;
  double *xn_im;
  int xn_im_size;
  int *scaling_sch;
  int scaling_sch_size;
  int direction;
};

struct xfft_outputs {
  double *xk_re;
  int xk_re_size;
  double *xk_im;
  int xk_im_size;
  int blk_exp;
  int overflow;
};

typedef void *(*xfft_create_state)(struct xfft_generics);
typedef int (*xfft_simulate)(void *, struct xfft_inputs, struct xfft_outputs *);

/* generics: the nine generics, in the order of struct xfft_generics */
void *clash_cores_xfft_create(void *create, const int *generics) {
  struct xfft_generics g;
  g.C_NFFT_MAX = generics[0];
  g.C_ARCH = generics[1];
  g.C_HAS_NFFT = generics[2];
  g.C_USE_FLT_PT = generics[3];
  g.C_INPUT_WIDTH = generics[4];
  g.C_TWIDDLE_WIDTH = generics[5];
  g.C_HAS_SCALING = generics[6];
  g.C_HAS_BFP = generics[7];
  g.C_HAS_ROUNDING = generics[8];
  return ((xfft_create_state)create)(g);
}

/* Run one transform of 2^nfft points. Gives the C model's status; on success
   the outputs are in xk_re, xk_im, blk_exp and overflow. */
int clash_cores_xfft_simulate(void *simulate, void *state, int nfft,
                              double *xn_re, double *xn_im, int *scaling_sch,
                              int scaling_sch_size, int direction,
                              double *xk_re, double *xk_im, int *blk_exp,
                              int *overflow) {
  struct xfft_inputs in;
  struct xfft_outputs out;
  int n = 1 << nfft;
  int status;
  in.nfft = nfft;
  in.xn_re = xn_re;
  in.xn_re_size = n;
  in.xn_im = xn_im;
  in.xn_im_size = n;
  in.scaling_sch = scaling_sch;
  in.scaling_sch_size = scaling_sch_size;
  in.direction = direction;
  out.xk_re = xk_re;
  out.xk_re_size = n;
  out.xk_im = xk_im;
  out.xk_im_size = n;
  out.blk_exp = 0;
  out.overflow = 0;
  status = ((xfft_simulate)simulate)(state, in, &out);
  *blk_exp = out.blk_exp;
  *overflow = out.overflow;
  return status;
}
