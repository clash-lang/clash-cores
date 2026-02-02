{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Clash.Cores.ClashFloPoCo.MPFR where

import Foreign
import Foreign.C.Types
import System.IO (IO)
import Prelude (Double, ($), return, realToFrac, Ordering(..), compare, fromIntegral)

-- | Opaque type for MPFR structure
data MPFR

-- | Pointer to MPFR structure
type MPFRPtr = Ptr MPFR

-- | MPFR rounding modes (mpfr_rnd_t)
-- typedef enum {
--   MPFR_RNDN=0,  /* round to nearest, with ties to even */
--   MPFR_RNDZ,    /* round toward zero */
--   MPFR_RNDU,    /* round toward plus infinity */
--   MPFR_RNDD,    /* round toward minus infinity */
--   MPFR_RNDA,    /* round away from zero */
--   MPFR_RNDF,    /* faithful rounding */
--   MPFR_RNDNA = -1 /* round to nearest, with ties away from zero (mpfr_round) */
-- } mpfr_rnd_t;
type MPFRRnd = CInt

rndN, rndZ, rndU, rndD, rndA, rndF, rndNA :: MPFRRnd
rndN = 0
rndZ = 1
rndU = 2
rndD = 3
rndA = 4
rndF = 5
rndNA = -1

-- | Initialize MPFR variable with given precision
foreign import ccall "mpfr.h mpfr_init2"
  c_mpfr_init2 :: MPFRPtr -> CLong -> IO ()

-- | Clear MPFR variable
foreign import ccall "mpfr.h mpfr_clear"
  c_mpfr_clear :: MPFRPtr -> IO ()

-- | Set MPFR variable from double
foreign import ccall "mpfr.h mpfr_set_d"
  c_mpfr_set_d :: MPFRPtr -> CDouble -> MPFRRnd -> IO CInt

-- | Get double from MPFR variable
foreign import ccall "mpfr.h mpfr_get_d"
  c_mpfr_get_d :: MPFRPtr -> MPFRRnd -> IO CDouble

-- | Add two MPFR variables
foreign import ccall "mpfr.h mpfr_add"
  c_mpfr_add :: MPFRPtr -> MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

foreign import ccall "mpfr.h mpfr_mul"
  c_mpfr_mul :: MPFRPtr -> MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

foreign import ccall "mpfr.h mpfr_exp"
  c_mpfr_exp :: MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Subtract two MPFR variables
foreign import ccall "mpfr.h mpfr_sub"
  c_mpfr_sub :: MPFRPtr -> MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Divide two MPFR variables
foreign import ccall "mpfr.h mpfr_div"
  c_mpfr_div :: MPFRPtr -> MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Logarithm
foreign import ccall "mpfr.h mpfr_log"
  c_mpfr_log :: MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Sine
foreign import ccall "mpfr.h mpfr_sin"
  c_mpfr_sin :: MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Cosine
foreign import ccall "mpfr.h mpfr_cos"
  c_mpfr_cos :: MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Absolute value
foreign import ccall "mpfr.h mpfr_abs"
  c_mpfr_abs :: MPFRPtr -> MPFRPtr -> MPFRRnd -> IO CInt

-- | Compare two MPFR variables
foreign import ccall "mpfr.h mpfr_cmp"
  c_mpfr_cmp :: MPFRPtr -> MPFRPtr -> IO CInt

-- | Get the size of mpfr_t structure from C
foreign import ccall "mpfr.h mpfr_custom_get_size"
  c_mpfr_custom_get_size :: CLong -> CSize

-- | Helper to run a computation with temporary MPFR variables
-- Uses a conservative struct size for safe allocation across platforms
-- mpfr_t struct contains: precision, sign, exponent, and limb pointer
-- Typical sizes: 16 bytes (32-bit), 32 bytes (64-bit), but we use 64 for safety
withMPFR :: CLong -> (MPFRPtr -> IO a) -> IO a
withMPFR prec action = do
  let structSize = (64 :: Int)  -- Conservative estimate for mpfr_t header
  allocaBytes structSize $ \ptr -> do
    c_mpfr_init2 ptr prec
    result <- action ptr
    c_mpfr_clear ptr
    return result

-- | Addition with custom rounding mode
mpfrAddRnd :: CLong -> MPFRRnd -> Double -> Double -> IO Double
mpfrAddRnd prec rnd a b =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \op2 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rnd
    _ <- c_mpfr_set_d op2 (realToFrac b) rnd
    _ <- c_mpfr_add res op1 op2 rnd
    d <- c_mpfr_get_d res rnd
    return (realToFrac d)

-- | Addition with default rounding (round to nearest)
mpfrAdd :: CLong -> Double -> Double -> IO Double
mpfrAdd prec = mpfrAddRnd prec rndN

mpfrMulRnd :: CLong -> MPFRRnd -> Double -> Double -> IO Double
mpfrMulRnd prec rnd a b =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \op2 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rnd
    _ <- c_mpfr_set_d op2 (realToFrac b) rnd
    _ <- c_mpfr_mul res op1 op2 rnd
    d <- c_mpfr_get_d res rnd
    return (realToFrac d)

mpfrMul :: CLong -> Double -> Double -> IO Double
mpfrMul prec = mpfrMulRnd prec rndN

-- | Subtraction with custom rounding mode
mpfrSubRnd :: CLong -> MPFRRnd -> Double -> Double -> IO Double
mpfrSubRnd prec rnd a b =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \op2 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rnd
    _ <- c_mpfr_set_d op2 (realToFrac b) rnd
    _ <- c_mpfr_sub res op1 op2 rnd
    d <- c_mpfr_get_d res rnd
    return (realToFrac d)

mpfrSub :: CLong -> Double -> Double -> IO Double
mpfrSub prec = mpfrSubRnd prec rndN

-- | Division with custom rounding mode
mpfrDivRnd :: CLong -> MPFRRnd -> Double -> Double -> IO Double
mpfrDivRnd prec rnd a b =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \op2 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rnd
    _ <- c_mpfr_set_d op2 (realToFrac b) rnd
    _ <- c_mpfr_div res op1 op2 rnd
    d <- c_mpfr_get_d res rnd
    return (realToFrac d)

mpfrDiv :: CLong -> Double -> Double -> IO Double
mpfrDiv prec = mpfrDivRnd prec rndN

-- | Simple wrapper for logarithm
mpfrLog :: CLong -> Double -> IO Double
mpfrLog prec a =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_log res op1 rndN
    d <- c_mpfr_get_d res rndN
    return (realToFrac d)

-- | Simple wrapper for exponential
mpfrExp :: CLong -> Double -> IO Double
mpfrExp prec a =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_exp res op1 rndN
    d <- c_mpfr_get_d res rndN
    return (realToFrac d)

-- | Simple wrapper for sine
mpfrSin :: CLong -> Double -> IO Double
mpfrSin prec a =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_sin res op1 rndN
    d <- c_mpfr_get_d res rndN
    return (realToFrac d)

-- | Simple wrapper for cosine
mpfrCos :: CLong -> Double -> IO Double
mpfrCos prec a =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_cos res op1 rndN
    d <- c_mpfr_get_d res rndN
    return (realToFrac d)

-- | Simple wrapper for absolute value
mpfrAbs :: CLong -> Double -> IO Double
mpfrAbs prec a =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \res -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_abs res op1 rndN
    d <- c_mpfr_get_d res rndN
    return (realToFrac d)

-- | Simple wrapper for comparison
mpfrCmp :: CLong -> Double -> Double -> IO Ordering
mpfrCmp prec a b =
  withMPFR prec $ \op1 ->
  withMPFR prec $ \op2 -> do
    _ <- c_mpfr_set_d op1 (realToFrac a) rndN
    _ <- c_mpfr_set_d op2 (realToFrac b) rndN
    r <- c_mpfr_cmp op1 op2
    return $ case compare r 0 of
      LT -> Prelude.LT
      EQ -> Prelude.EQ
      GT -> Prelude.GT
