{- | Functions under test for "FloatingDouble". They are defined here, as the
Template Haskell in "FloatingDouble" simulates them to create reference
traces.
-}
{-# LANGUAGE TemplateHaskell #-}

module FloatingDoubleDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import qualified Clash.Cores.Intel.Floating.Double.Explicit as E

import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)

import FloatingDuts (simFamily)
import IpTestCommon

-- | 'trace' for the functions whose models don't cover Cyclone V: for that
-- family the trace has no expected outputs, so the test bench runs the IP but
-- checks nothing
traceNoCycloneV ::
  (NFDataX i, NFDataX o, BitPack o, KnownNat k) =>
  Dut i (Vec k o) ->
  [Cycle i] ->
  [Entry i k (BitSize o)]
traceNoCycloneV dut cs
  | simFamily == E.CycloneV = [(cycleInput c, cycleReset c, cycleEnable c, repeat Nothing) | c <- cs]
  | otherwise = trace dut cs

-- | E.add, E.addWith @4
addDut :: Dut (Double, Double) (Vec 2 Double)
addDut clk rst en i = bundle (binary E.add clk rst en i :> binary (E.addWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE addDut #-}

-- | E.sub, E.subWith @4
subDut :: Dut (Double, Double) (Vec 2 Double)
subDut clk rst en i = bundle (binary E.sub clk rst en i :> binary (E.subWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE subDut #-}

-- | E.mul, E.mulWith @4
mulDut :: Dut (Double, Double) (Vec 2 Double)
mulDut clk rst en i = bundle (binary E.mul clk rst en i :> binary (E.mulWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE mulDut #-}

-- | E.div, E.divWith @16
divDut :: Dut (Double, Double) (Vec 2 Double)
divDut clk rst en i = bundle (binary E.div clk rst en i :> binary (E.divWith @16 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE divDut #-}

-- | E.sqrt, E.sqrtWith @12
sqrtDut :: Dut Double (Vec 2 Double)
sqrtDut clk rst en i = bundle (unary (E.sqrt simFamily) clk rst en i :> unary (E.sqrtWith @12 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE sqrtDut #-}

-- | E.recip, E.recipWith @15
recipDut :: Dut Double (Vec 2 Double)
recipDut clk rst en i = bundle (unary E.recip clk rst en i :> unary (E.recipWith @15 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE recipDut #-}

-- | E.exp, E.expWith @20
expDut :: Dut Double (Vec 2 Double)
expDut clk rst en i = bundle (unary (E.exp simFamily) clk rst en i :> unary (E.expWith @20 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE expDut #-}

-- | E.rsqrt, E.rsqrtWith @12
rsqrtDut :: Dut Double (Vec 2 Double)
rsqrtDut clk rst en i = bundle (unary (E.rsqrt simFamily) clk rst en i :> unary (E.rsqrtWith @12 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE rsqrtDut #-}

-- | Inputs of the reciprocal square root: inputs in the two table segments
-- whose coefficients were observed from the IP (where the computed
-- coefficients would give other results on MAX 10), inputs for which the
-- models of MAX 10 and Cyclone V differ, and 'doublesIn'
rsqrtDoublesIn :: Int -> Word64 -> [Double]
rsqrtDoublesIn n seed =
  P.map castWord64ToDouble (rsqrtObservedWords <> rsqrtDatapathWords) <> doublesIn 1 2046 n seed

-- | Inputs in segments 320 and 426 of the reciprocal square root (24 each),
-- see Note [Observed double precision rsqrt coefficients] in
-- "Clash.Cores.Intel.Floating.Internal.BitExactDouble"
rsqrtObservedWords :: [Word64]
rsqrtObservedWords =
  [ 0x31c40e9fab2d9aec, 0x3004096cee65ab4f, 0x45040d77e687ac1f, 0x3d840c4b1e72c37b
  , 0x5c840fbcfeab438e, 0x73640f51f6925e9f, 0x2a240e896d930b6c, 0x26640e06647cb5d4
  , 0x47840cd2e164a5ae, 0x50240c64b7e4db0d, 0x03a40893218b3867, 0x33840971f129e85d
  , 0x01e40f2c1539940c, 0x0b640ef9131109c3, 0x10840e98f307099f, 0x0ac404859da65325
  , 0x38e40d9b14a05bdd, 0x03c40dc26f25e2cc, 0x3b0405bdc8b1726e, 0x56a403a6c4df0737
  , 0x49a40f4b8bc09e52, 0x40840a6a7fcf8de0, 0x53a4082ebca4d66f, 0x2b240f85736ec19b
  , 0x496aaf6197a4488b, 0x1f6aa0a570d640e1, 0x56caa24c9df9c587, 0x124aa1d5a944d475
  , 0x116aa012bc778e60, 0x3e4aafa44611e4c3, 0x56aaa8c37c07d55d, 0x5eaaa02a69f757d6
  , 0x0b4aa75de95459fa, 0x278aa03e7ad11316, 0x75aaaf50386b1c55, 0x6ccaaa178e19c076
  , 0x4a4aa3e40da09142, 0x278aaff288243291, 0x52aaa5cbaa0ba229, 0x6f0aaffb60aa02af
  , 0x15aaabaf66403895, 0x462aa83969e2b259, 0x29eaa9147fe58e03, 0x228aab371a5f8fd3
  , 0x0bcaae41cd05a09a, 0x1deaaee4157a4380, 0x3daaa97ef8b86e4b, 0x570aa0ce4cb74c99 ]

-- | Inputs for which the reciprocal square roots of MAX 10 and Cyclone V differ
rsqrtDatapathWords :: [Word64]
rsqrtDatapathWords =
  [ 0x40316fcb19955df8, 0x32f40347539a4ad4, 0x63d7ff0432dff295, 0x779a4d7e09ad9cf5
  , 0x4f5388b3c62a9f5a, 0x01f7974353cd1ded, 0x12480ec75221f2df, 0x76b1f509b408a49c
  , 0x363dbb6d5560818e, 0x60e5343c9326a33c, 0x2c9e6e2ebc7f9c81, 0x78057f12987d8f1e
  , 0x0c9dae2494e4705c, 0x3a6a3e5b888eff19, 0x48488e7fa47dab37, 0x32ff3c2818aaa2fe
  , 0x73d58a38baab9b60, 0x69c98d4afe42eb20, 0x57f31e537f15b6e3, 0x406965a1a545b2bd
  , 0x50ea1e13d4b2c45f, 0x111e4518812fd07e, 0x2d9cdfb963ac0493, 0x5b26380c1481cb70
  , 0x6879c3d8c7e6677b, 0x29303fa312d14a03, 0x7e657ff64cd1d2d3, 0x6eaaffb78e30718c
  , 0x6988c9051db697f3, 0x3a38f0439d53de7a, 0x1192beada87d859c, 0x7fde88cb37ef39df ]

-- | Inputs of the exponential: 'specialDoubleWords', the doubles just below a
-- multiple of ln 2 for which the IP's estimate of the multiple is one too high
-- (on some family), inputs for which the models of MAX 10 and Cyclone V
-- differ, and @n@ random inputs of several kinds: in the range where the
-- result is finite and nonzero, in @[0, ln 2)@ (where the result only depends
-- on the polynomial; up to 1/2), with random exponents, and random bit patterns
expDoublesIn :: Int -> Word64 -> [Double]
expDoublesIn n seed =
  P.map castWord64ToDouble (specialDoubleWords <> expBoundaryWords <> expDatapathWords <> P.take n (go (randoms seed)))
 where
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : rs) =
    let w = case k `mod` 4 of
          0 -> withExp x (0x3f8 + (k `shiftR` 8) `mod` 0x11)
          1 -> withExp x (0x3f9 + (k `shiftR` 8) `mod` 5) .&. 0x7fffffffffffffff
          2 -> withExp x (0x3c4 + (k `shiftR` 8) `mod` 0x45)
          _ -> x
     in w : go rs
  go _ = []

-- | The doubles closest to (and below) a multiple of ln 2 for which the IP's
-- estimate of the multiple is one too high, on MAX 10 or Cyclone V
expBoundaryWords :: [Word64]
expBoundaryWords =
  [ 0x403bb9d3beb8c86b, 0x404bb9d3beb8c86b, 0x405127c7d13588cf, 0x4057bd83b4e7d202
  , 0x405bb9d3beb8c86b, 0x406127c7d13588cf, 0x4062747dbe263234, 0x406670cdc7f7289d
  , 0x4067bd83b4e7d202, 0x406bb9d3beb8c86b, 0x406d0689aba971d0, 0x407127c7d13588cf
  , 0x4072747dbe263234, 0x4073cc4acc9658b6, 0x40751900b987021b, 0x407670cdc7f7289d
  , 0x4077bd83b4e7d202, 0x40791550c357f884, 0x407a6206b048a1e9, 0x407bb9d3beb8c86b
  , 0x407d0689aba971d0, 0x407e5e56ba199852, 0x407fab0ca70a41b7, 0x40807be149fd758e
  , 0x408127c7d13588cf, 0x4081d3ae586d9c10, 0x4082747dbe263234, 0x40832064455e4575
  , 0x4083cc4acc9658b6, 0x4084783153ce6bf7, 0x40851900b987021b, 0x4085c4e740bf155c
  , 0xc0452417db067f38, 0xc0552417db067f38, 0xc0592067e4d775a1, 0xc05fb623c889bed4
  , 0xc063d761ee15d5d3, 0xc0652417db067f38, 0xc0692067e4d775a1, 0xc06a6d1dd1c81f06
  , 0xc06e696ddb99156f, 0xc06fb623c889bed4, 0xc07132def2b505ec, 0xc0727f94dfa5af51
  , 0xc073d761ee15d5d3, 0xc0752417db067f38, 0xc0767be4e976a5ba, 0xc077c89ad6674f1f
  , 0xc0792067e4d775a1, 0xc07a6d1dd1c81f06, 0xc07bc4eae0384588, 0xc07d11a0cd28eeed
  , 0xc07e696ddb99156f, 0xc07fb623c889bed4, 0xc08086f86b7cf2ab, 0xc08132def2b505ec
  , 0xc081dec579ed192d, 0xc0827f94dfa5af51, 0xc0832b7b66ddc292, 0xc083d761ee15d5d3
  , 0xc0848348754de914, 0xc0852417db067f38, 0xc085cffe623e9279 ]

-- | Inputs for which the exponentials of MAX 10 and Cyclone V differ
expDatapathWords :: [Word64]
expDatapathWords =
  [ 0x3fd5fc0f6ef40e7e, 0x4059146813450bed, 0xc07a2a008656a150, 0xbf188f9cc4b4eb74
  , 0xc07ee0a1a3819382, 0xbebb0f9a54eaf8c7, 0x405a78f69b0bc1f0, 0xc07a3bb7f6a42f5a
  , 0x3fd9f88e32d788b7, 0x4075074684b8802c, 0xbf122238f50d6ab5, 0x3fe1b00534683f0e
  , 0x408606c529f6d42c, 0x3fd4dd5b5efa5d40, 0x3fd3e1192fd7a9b0, 0xbf5356c19abdf730
  , 0x400ed600383281a8, 0x3fcc7ee2454fb5c7, 0xbe6fb9491bd1d549, 0x40579a8049e6ce70
  , 0x3fe33f53662e6c65, 0x3fb43da0939e7568, 0x3fe22229cfbb7a22, 0x3fe3a7f247d0ad1a
  , 0xc07292db6f944066, 0x3fcfe36e401b45e1, 0x3fccd968ee567394, 0x407d1bf520fdd2f4
  , 0x40838e495e46a688, 0x3fc29f77178e113b, 0x40771ec6bf1c7c20, 0xc07d757631a92a75
  , 0x3fd086cc36dd1a41, 0x3fd70e0b311471cb, 0xc077e4817ef4d288, 0x40018aa4a8d49767
  , 0x3ee573feb1fef6b0, 0x405e1dfb687676f8, 0x3fdae4e90b98e4d5, 0x4082c39e6121aa2c
  , 0xbf551fbdd5607650, 0x407026d86d0cce6e, 0xc077bbaeb529145b, 0x3fd52ac92ea19bfd
  , 0x3fdcd4f2be48abb2, 0x3fe1b79d5e83f847, 0x3fe615d11ead64bf, 0x403db894cbe6d800
  , 0x3fb5dcd496865822, 0x3fd1eaeac6ff593b, 0x3fe1f580d17a237f, 0x3fdce95c375e203d
  , 0xc0517087fe728a50, 0xbefa25ee301b4476, 0x3f88175c841bcfb4, 0x40228cea3b971140
  , 0xbff9a8995c685c64, 0xbfc1242cc730e40f, 0x3fbd4590aa0c74f1, 0x4070731c44dcaa8e
  , 0x3f9c9fde9fa0940c, 0x3f6d6b62cef827be, 0x4077a1473188d77e, 0x3fd3751546b67b67
  , 0x3fe4b1e8c7e794bf, 0xbef5d6e0f70d46cd, 0x3fbf0650cc82cd8c, 0x3fab99b176be7ea3
  , 0x3fc2f0601dfcd2f7, 0xc05ed6f53717a5f0, 0xc070386c6437e46c, 0xc0668a727bdc0ee0
  , 0xbe7450978d9401cf, 0x3fd05c48b9924961, 0x3fdd68955c34f3fd, 0xc07ca40464993493
  , 0x3fd1f9b6ac7a590b, 0x3fddef84a5bedd68, 0xc069d5cfea338db4, 0xc05265c58c8e9620
  , 0x3fe36dba69153be9, 0x407a1bc0f9a0ac94, 0x407f8c4827be8338, 0xc07864df9795516f
  , 0x3f9f0559d32c61af, 0x4078a1cc05d74070, 0x3f8c7e5392387cff, 0xbef2b668c730d178
  , 0x3fcda90145e01e94, 0xc08196e420e8db04, 0xc0282005d9f13613, 0x3fdedab931e7416a
  , 0x3fe2a67fd8c7614d, 0xbf3d9ee136c14dc9, 0x4082ef22f5966be0, 0xbf16bffca8852676
  , 0xbeab9828fafa213b, 0x40657dfd9e6c7b10, 0xc0107c0a639dfde9, 0x3fd23e21b4f9db93
  , 0xc065046086c842c2, 0xc050f224835c30e0, 0xc05625dc05a87bb0, 0x3fe5fb334c13a7e6
  , 0x3fc2f5142aa14ef5, 0x3fc4afe3ed6cfd69, 0xc0779e290c0e0298, 0x4079f3d50a989b84
  , 0xc0844b4ad3cdc851, 0xc059ea02e78fa560, 0xc085d6285d7477b3, 0xc07553f282a9c312
  , 0xbe6ebf0c3abc7f84, 0x4081710e02ad9090, 0x3fd9aef27efb0823, 0x3fc9d8d9b3cc2d1a
  , 0x3fb76f05fffc6747, 0xc05955cb1037dc98, 0x406c414e7e9a65d4, 0xbed5d23e86d0383c
  , 0x4081ce9c4f02ee40, 0x3fe21ac6a279867a, 0x3ef57622fbf705c5, 0x408006c82b35eaf4
  , 0xc065bc6931339cf8, 0x3fd63f2dfc8eae90, 0x4074360dfc690b6c, 0xbfa4d6087cfdbd59
  , 0x3fd84f5cc9e41cf4, 0x40300315f175afa5, 0x404e5d388c1eca80, 0xc07db8936390f4ee
  , 0x40860f9bec8c2a9c, 0xc041702cedd9f9e8, 0x3fe375e464d652b7, 0x3fe166a07ea79cd6
  , 0xbeb13e09fe76ed52, 0x3fd1a4e5623c3a39, 0x3fd5a23762efcb9e, 0x3fe51bc5840adb28
  , 0xc0825db08daaf14f, 0xbf7cc23a848d1920, 0x3fd57dd0eb36cdd1, 0x4079aa5ca787007c
  , 0xc0847334f3dc988a, 0xbea135ba7d0c8601, 0x3fa38c3bef00b202, 0x40111679242e2d0a
  , 0xbf5b763bcee467d3, 0xc0752b0f40f146fc, 0x3eb7e196b791d732, 0x3fd3652fc3528723
  , 0x3fe456bcae9b67e8, 0x3f8f1a825c5db703, 0xc053b4363598c07a, 0xc08567a231557020
  , 0xc07c2784d47cdeb1, 0xbf6c5e926da18b4d, 0x4060a7d9642416b8, 0xc040177d63569590 ]

-- | Bit patterns of interesting doubles: zeros, subnormal numbers, the
-- smallest and largest normal numbers, infinities, NaNs, numbers around one
-- and powers of two
specialDoubleWords :: [Word64]
specialDoubleWords =
  [ 0x0000000000000000, 0x8000000000000000, 0x0000000000000001, 0x8000000000000001
  , 0x000fffffffffffff, 0x800fffffffffffff, 0x0010000000000000, 0x8010000000000000
  , 0x0010000000000001, 0x001fffffffffffff, 0x0020000000000000, 0x7fefffffffffffff
  , 0xffefffffffffffff, 0x7fe0000000000000, 0x7fdfffffffffffff, 0x7ff0000000000000
  , 0xfff0000000000000, 0x7ff8000000000000, 0xfff8000000000000, 0x7ff0000000000001
  , 0xffffffffffffffff, 0x3ff0000000000000, 0xbff0000000000000, 0x3ff0000000000001
  , 0x3fefffffffffffff, 0x3fffffffffffffff, 0x4000000000000000, 0x3fe0000000000000
  , 0x4010000000000000, 0x3fd0000000000000, 0x400921fb54442d18, 0x3fb999999999999a ]

-- | Doubles: 'specialDoubleWords', followed by @n@ random ones of several
-- kinds: all bit patterns, positive normal numbers with biased exponents in
-- @[lo, lo + m)@, numbers with few fraction bits set, and numbers at the ends
-- of the exponent range
doublesIn :: Word64 -> Word64 -> Int -> Word64 -> [Double]
doublesIn lo m n seed =
  P.map castWord64ToDouble (specialDoubleWords <> P.take n (go (randoms seed)))
 where
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : rs) =
    let w = case k `mod` 5 of
          0 -> x
          1 -> withExp x (lo + (k `shiftR` 8) `mod` m) .&. 0x7fffffffffffffff
          2 -> withExp (x .&. 0xfff00000000000ff) (lo + (k `shiftR` 8) `mod` m) .&. 0x7fffffffffffffff
          3 -> withExp x (if testBit k 9 then 1 + (k `shiftR` 10) `mod` 3 else 2046 - (k `shiftR` 10) `mod` 3)
          _ -> withExp x (0x3fe + (k `shiftR` 8) `mod` 4) .&. 0x7fffffffffffffff
     in w : go rs
  go _ = []

-- | Pairs of doubles: all pairs of the first 20 'specialDoubleWords', followed
-- by @n@ random pairs: random bit patterns, numbers close to each other (with
-- either sign) and pairs whose product or quotient is close to the smallest
-- normal number
doublePairs :: Int -> Word64 -> [(Double, Double)]
doublePairs n seed =
  [(castWord64ToDouble x, castWord64ToDouble y) | x <- sp, y <- sp]
    <> P.take n (go (randoms seed))
 where
  sp = P.take 20 specialDoubleWords
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : y : rs) =
    let a = withExp x (0x3c0 + (k `shiftR` 8) `mod` 0x80)
        ec = 0x3e0 + (k `shiftR` 8) `mod` 0x20
        c = withExp x ec
        d = (k `shiftR` 20) `mod` 3
        pair = case k `mod` 5 of
          0 -> (castWord64ToDouble x, castWord64ToDouble y)
          1 -> (castWord64ToDouble a, castWord64ToDouble ((a `xor` (y `mod` 16)) `xor` (y .&. bit 63)))
          -- a * b close to the smallest normal number
          2 -> (castWord64ToDouble c, castWord64ToDouble (withExp y (0x3ff - ec + d)))
          -- a / b close to the smallest normal number
          3 -> (castWord64ToDouble c, castWord64ToDouble (withExp y (ec + 0x3fe - d)))
          _ -> (castWord64ToDouble a, castWord64ToDouble (withExp y (0x3c0 + (k `shiftR` 16) `mod` 0x80)))
     in pair : go rs
  go _ = []

-- | E.hypot, E.hypotWith @14
hypotDut :: Dut (Double, Double, Double) (Vec 2 Double)
hypotDut clk rst en i = bundle (ternary (E.hypot simFamily) clk rst en i :> ternary (E.hypotWith @14 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE hypotDut #-}

-- | Inputs of the hypotenuse: all triples of a few special values, triples
-- whose results differ between the MAX 10 and Cyclone V datapaths, and @n@
-- random triples of either sign whose (biased) exponents are @e@, @e - d1@ and
-- @e - d2@ in a random order, with @d1@ and @d2@ below 64, so that their
-- squares overlap or just miss each other when 'E.hypot' adds them
hypotDoublesIn :: Int -> Word64 -> [(Double, Double, Double)]
hypotDoublesIn n seed =
  [(f x, f y, f z) | x <- sp, y <- sp, z <- sp]
    <> P.map (\(x, y, z) -> (f x, f y, f z)) hypotDatapathWords
    <> P.take n (go (randoms seed))
 where
  f = castWord64ToDouble
  sp = [0, 0x8000000000000001, 0x7ff0000000000000, 0x7ff8000000000000, 0x3ff0000000000000, 0x7fefffffffffffff, 0x0010000000000000, 0xbff8000000000000]
  go (k : a : b : c : rs) =
    let e = 64 + k `P.mod` 1980
        d1 = (k `shiftR` 32) `P.mod` 64
        d2 = (k `shiftR` 40) `P.mod` 64
        mk r d = f ((r .&. 0x800fffffffffffff) .|. ((e - d) `shiftL` 52))
        (x, y, z) = (mk a 0, mk b d1, mk c d2)
     in (case (k `shiftR` 48) `P.mod` 3 of
           0 -> (x, y, z)
           1 -> (y, z, x)
           _ -> (z, x, y)) : go rs
  go _ = []

-- | Input triples whose results differ between the MAX 10 and Cyclone V
-- datapaths
hypotDatapathWords :: [(Word64, Word64, Word64)]
hypotDatapathWords =
  [ (0x768bda9424b90690, 0xf68b49f1574ca113, 0x76c5e498b6450d3f)
  , (0xdd4518fd5053f427, 0x5d8dc0ad8737311f, 0x5d94d0f97084ecf8)
  , (0x8afc120665e31d14, 0x0b18011cf80a2044, 0x8ac6e815ccd2e9b6)
  , (0x4b5abd8a475ceab4, 0xcb7953b11b917c1d, 0xcb31dc1738e7746c)
  , (0x48a4c48acc4dcafd, 0x4863e0ad75b83bac, 0x48b494dc74bb449c)
  , (0xd8cae3bc7fff3ca8, 0xd8e142c0905c3e2e, 0xd8e1c74e6198cf15)
  , (0xfe015df73d4e8e7d, 0x7e44d1838d1ea8fa, 0xfe520ad33ce193de)
  , (0xb88feb4f10256ad6, 0x389c89acf028ecd1, 0xb8c45aa40f7294e6)
  , (0xeec5ef844dc99b0b, 0xeeda12abcf407205, 0x6edd54d1f0ca099c)
  , (0x96f1b0b4d2e1d578, 0x170a186479ead60c, 0x170bf63f48d287c8)
  , (0x0b1418c207a8bcc8, 0x8b229b57ee366064, 0x0b2c065c28016f96)
  , (0x597ef9acea34dc00, 0xd97c4259c51f87ea, 0x598e51d5d7a212dc)
  , (0x0510d91e4ca9ac00, 0x85531116c65d1a98, 0x856817e54ea88a94)
  , (0x400f7b1e15ba4d8e, 0xbfc20fb4fcd6b00c, 0x3fb8d8d773b30148)
  , (0xd947709bc148d219, 0xd9515ff473f46f7f, 0x595602700bceb121)
  , (0xe1ea33b660772eed, 0xe2088d2cf0edd96f, 0xe1f55d6cafb191d7)
  , (0xb77bce6473edc184, 0xb75feea81c5bd4e4, 0xb784266eab7f7e88)
  , (0x47b8e0c9ab665632, 0xc7f313f7ede2270d, 0x47bb9aaceca00ea4)
  , (0x67bbcb302b72a39f, 0x67951a6cd6d452c0, 0xe797e278c88dd234)
  , (0x5f078010c4ad6537, 0xdf5a7e331873bece, 0x5f22a17963eba5cc)
  , (0x70c0c5cf63c6596c, 0x70e0d5e9cfb7bedf, 0xf0b715561b124df0)
  , (0x3517d1910b8f4cdb, 0xb4ff1c446875d368, 0x351299ffca798ffc)
  , (0xe30c0e36a4188aae, 0x630318b551f220e9, 0xe30fecfc8143a321)
  , (0x42e5df5d15e0a626, 0x42dc912a5ef289bb, 0x42bc479cdb43bc9b) ]

-- | E.log10, E.log10With @13
log10Dut :: Dut Double (Vec 2 Double)
log10Dut clk rst en i = bundle (unary (E.log10 simFamily) clk rst en i :> unary (E.log10With @13 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log10Dut #-}

-- | Inputs of the base 10 logarithm: inputs whose results depend on how the
-- IP truncates its products, and 'doublesIn' (special values, all bit
-- patterns, and many inputs in [0.5, 2), where the polynomial is used on its
-- own)
log10DoublesIn :: Int -> Word64 -> [Double]
log10DoublesIn n seed = P.map castWord64ToDouble log10DatapathWords <> doublesIn 1 2046 n seed

-- | Inputs for which the base 10 logarithm of MAX 10 differs from the one the
-- IP would compute with full multipliers
log10DatapathWords :: [Word64]
log10DatapathWords =
  [ 0x3fe62fd9328612bb, 0x0ff81c4c7900f237, 0x56ba844c07b8f93a, 0x3ffe370d6717269d
  , 0x3fd0e51fab98cef7, 0x3fec12eba4727386, 0x20df2f12f60f6dd1, 0x3ff70f8f8f2485a1
  , 0x40018f638119af7c, 0x3f8e19d3668e3009, 0x400a838d5418495e, 0x3fed7edd6598cd46
  , 0x40015d2387a640a6, 0x40032620fac67252, 0x3fae0a86dace5408, 0x5e84f2c0d998a68c
  , 0x3fe5c1ffa748709a, 0x3fd038b66fe50096, 0x3fdcaba47af2ddd2, 0x380dcfee6aebaccb
  , 0x36dfd87a9f4cb518, 0x3fe1b813ef7138ae, 0x3f90b32cf0608bb2, 0x3feb4ddfea5b17d4
  , 0x3ffc43bbb1703fdd, 0x71495f50e578c9c1, 0x3fff1373dd407edb, 0x3ffa56f7f26c159e
  , 0x0c0a624f9ff9498b, 0x3fe83278b53728b8, 0x404dd967acfd0c28, 0x4005df33c566bf1b
  , 0x3ff15cfb1905e4d8, 0x3ff6d4f7938ecdbb, 0x3feb1c4fd4b39524, 0x3ff07d6fe0ce565c
  , 0x3ffd22e46f700049, 0x3fe983db71d042c5, 0x3f8746d130e84b81, 0x3fbfc99be98b824f
  , 0x403bfaa644e5b9e0, 0x3feaa66c88c5d33a, 0x6baff2c8744a0c7c, 0x5a11c328a2dddec2
  , 0x3ff4f4a9447bf361, 0x3fe2f6630b690ef4, 0x0b2f6f693275e87a, 0x404cc10958fb5f9c ]

-- | E.cbrt, E.cbrtWith @14
cbrtDut :: Dut Double (Vec 2 Double)
cbrtDut clk rst en i = bundle (unary (E.cbrt simFamily) clk rst en i :> unary (E.cbrtWith @14 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE cbrtDut #-}

-- | Inputs of the cube root: inputs whose results depend on how the IP
-- truncates its products, and 'doublesIn'
cbrtDoublesIn :: Int -> Word64 -> [Double]
cbrtDoublesIn n seed = P.map castWord64ToDouble cbrtDatapathWords <> doublesIn 1 2046 n seed

-- | Inputs for which the cube root of MAX 10 differs from the one the IP
-- would compute with full multipliers
cbrtDatapathWords :: [Word64]
cbrtDatapathWords =
  [ 0x40100f92f3f31118, 0x40141df9f1f8a75e, 0x4008fee2abbbfb70, 0x4014d8ac4b7b70ac
  , 0x401af7f7b09db77c, 0x400a578c7110e48b, 0x3ff61d21d76f3fc8, 0x401cf71b4e6fa1e6
  , 0xf5d7f546f1894ac9, 0x8ddd5895029c824d, 0x26f33f23b5daeeaa, 0x32e6460eceb0c0bc
  , 0xc7d96d287a6e832e, 0xb5a7c0082b8892c2, 0x966c180cb8f21948, 0x0ce8747c1951efd5
  , 0x07d998170cc2b03a, 0x8d476e2affaf80ff, 0x1877acf4e2e8e974, 0x7805ee2b39a08958
  , 0x9d391e68a5eb2a91, 0x5123d66c6a44acfb, 0xdc32d10ad5ad19e1, 0x221c5eafaf3e246d
  , 0x278c07f1046e26e1, 0xb89418358aa60465, 0x252b85ffca526da2, 0x7541d60252ac8404
  , 0x1ba40aa5488b8171, 0x6c94949a18674522, 0x6178d849fa86eae9, 0xe6b15b25d6ea6a74
  , 0x94bc0aef40038dc3, 0x33e34c67af9d3b26, 0x4e5e5d5f83cb062b, 0xbf2be658341dcde4
  , 0xd2863687ad3fbdfb, 0x53f8a72f24d66a6f, 0x524c3891a84f2992, 0x0e9bda2378bb61b6
  , 0x738ad736a00b27a1, 0xc6415f764c18f452, 0xed7c5b3500131006, 0xa628fffff06a2ec4
  , 0x304cb99489d1c764, 0xea9c1e8275e39931, 0x9cdfe57c16e328c1, 0x0ef0fe0fc291d624 ]

-- | E.log, E.logWith @13
logDut :: Dut Double (Vec 2 Double)
logDut clk rst en i = bundle (unary (E.log simFamily) clk rst en i :> unary (E.logWith @13 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE logDut #-}

-- | Inputs of the natural logarithm: inputs in the table segment whose
-- coefficients were observed from the IP (for which the computed coefficients
-- would give other results), inputs whose results depend on how the IP
-- truncates its products, and 'doublesIn'
logDoublesIn :: Int -> Word64 -> [Double]
logDoublesIn n seed = P.map castWord64ToDouble (logObservedWords <> logDatapathWords) <> doublesIn 1 2046 n seed

-- | Inputs in segment 1647 of the natural logarithm, see Note [Observed
-- double precision logarithm coefficients] in
-- "Clash.Cores.Intel.Floating.Internal.BitExactDouble"
logObservedWords :: [Word64]
logObservedWords =
  [ 0x3fe9bfaf7d142e2e, 0x3fe9be989da5bd8d, 0x3fe9be3e9f677694, 0x3fe9bdd4d29e4042
  , 0x3fe9bf03a0340000, 0x3fe9befd947a772b, 0x3fe9be53a3076ec6, 0x3fe9bff3ca1b1c0b
  , 0x3fe9bd6ab6800000, 0x3fe9bddff884228a, 0x3fe9be314e4b9e71, 0x3fe9be3e92426a80
  , 0x3fe9bf90b71a6a70, 0x3fe9bfc65f3c8000, 0x3fe9be1d030aaa15, 0x3fe9bf062dafc30f
  , 0x3fe9bf52a3d86ff2, 0x3fe9be6dab780000, 0x3fe9bf4dfc17b543, 0x3fe9bcaaa886f888
  , 0x3fe9bc3a94188a65, 0x3fe9bf903d0a8ea5, 0x3fe9bf20185fbdfd, 0x3fe9bf92e2142729 ]

-- | Inputs for which the natural logarithm of MAX 10 differs from the one the
-- IP would compute with full multipliers
logDatapathWords :: [Word64]
logDatapathWords =
  [ 0x3fe70ff75fa866d0, 0x3fee1d9f6888ab39, 0x3ff24718416fe42d, 0x3fef35c12700aa5c
  , 0x3fe5a7c828ff046c, 0x3ff0bb10d75c6b9f, 0x3ff201ff0a1ae5b6, 0x3ff2e94877e69063
  , 0x3ffd5b9accd19d6c, 0x3ff03fe072d0f0e7, 0x3fe686c1d67d3327, 0x3fff7615bf7d6e87
  , 0x3fe9c78e0a122cca, 0x3feb47d1b8a69bc7, 0x3ff726d8200bce32, 0x3ffbead55b24000a
  , 0x3ff1ab6d2ba01a1e, 0x3ff9ddaa938776b2, 0x3ffcbf741c2e3e5b, 0x3fec348f5edea12d
  , 0x3fef26bd5230bba9, 0x3ff22a25ec80c267, 0x3ff60621dba4181f, 0x3fe14af81eeda3f2
  , 0x3ffc5fcc22b7c113, 0x3ff7d79f591d146f, 0x3feb3dbf1d26cd39, 0x3ff9fa1721af39fc
  , 0x3ff2de95ef32dfb7, 0x3fedb752cd263ef1, 0x3feb4459e949c2cb, 0x3ff3e2a7b2f6292c
  , 0x3ff575be43a671ab, 0x3ffdc3e41948ad0c, 0x3fec90b37bf8828a, 0x3ffa23a04326d399
  , 0x3fee4775b71144d5, 0x3fee9b9c54fba217, 0x3feb90f90c2d2d11, 0x3ff53acae5106681
  , 0x3ff9336aaecf6996, 0x3ff827d7b774493f, 0x3fe6d72a9915e1aa, 0x3ffb36fed832883e
  , 0x3febe9fee7ee42d6, 0x3fe6bddc8273e8cd, 0x3fef49146bb46a06, 0x3fe13756632f0b53 ]

-- | E.log1p, E.log1pWith @13
log1pDut :: Dut Double (Vec 2 Double)
log1pDut clk rst en i = bundle (unary (E.log1p simFamily) clk rst en i :> unary (E.log1pWith @13 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log1pDut #-}

-- | Inputs of @log (1 + x)@: inputs for which @1 + x@ is in the table segment
-- whose coefficients were observed from the IP (and the computed coefficients
-- would give other results), inputs whose results depend on how the IP
-- truncates its products, 'specialDoubleWords', and @n@ random inputs for
-- which the result is finite: positive ones with (biased) exponents in
-- @[960, 1090)@ and negative ones above -1 (many of them with few fraction
-- bits)
log1pDoublesIn :: Int -> Word64 -> [Double]
log1pDoublesIn n seed =
  P.map castWord64ToDouble (log1pObservedWords <> log1pDatapathWords <> specialDoubleWords <> P.take n (go (randoms seed)))
 where
  go (k : x : rs) =
    let e = 960 + (k `shiftR` 8) `mod` 130
        s = if testBit k 40 && e < 1023 then bit 63 else 0
        f = if k `mod` 4 == 0 then x .&. 0x000fffff00000000 else x .&. 0x000fffffffffffff
     in (s .|. (e `shiftL` 52) .|. f) : go rs
  go _ = []

-- | Inputs for which @1 + x@ is in segment 1647 of the natural logarithm's
-- table, see Note [Observed double precision logarithm coefficients] in
-- "Clash.Cores.Intel.Floating.Internal.BitExactDouble"
log1pObservedWords :: [Word64]
log1pObservedWords =
  [ 0xbfc901a1896fec30, 0xbfc90325f55a29a4, 0xbfc9014e9b896c40, 0xbfc902095ce1b330
  , 0xbfc901cb1bdc5500, 0xbfc903183683333c, 0xbfc904f222db66ec, 0xbfc902c1a6000000
  , 0xbfc90ec4aa45cc98, 0xbfc90f81a4a44a08, 0xbfc9098a9111dc98, 0xbfc901b881f78280
  , 0xbfc9038c13d097dc, 0xbfc90168601e0000, 0xbfc90f6ac61fd1d0, 0xbfc903646837deb4
  , 0xbfc903f4cc000000, 0xbfc9001309e445ec, 0xbfc90210f474ee38, 0xbfc904555d82e000
  , 0xbfc903f303229398, 0xbfc9010d6b8a1140, 0xbfc90da1964066f0, 0xbfc900a37a7371a0 ]

-- | Inputs for which @log (1 + x)@ of MAX 10 differs from the one the IP
-- would compute with full multipliers
log1pDatapathWords :: [Word64]
log1pDatapathWords =
  [ 0xbf72117ccef9621c, 0xbfbe890413225295, 0x3ff7a44ea27e3935, 0xbe76deda4be560ff
  , 0xbefd8537f5880de8, 0xbf636ee8fb8ebb73, 0xbebf1390c79119c7, 0xbefea366d87b9185
  , 0xbfd740f5a2442c23, 0xbf03c8e3f5b13047, 0x3fb94e110d573f50, 0x3fea2e6c0f250d93
  , 0x3ede64b8641ad58f, 0xbe961b2087e068b5, 0x3f8f6a0518e6e7b5, 0x3feb75c3074d8d5d
  , 0xbe727ab4b95b36f8, 0xbecad563383da34c, 0xbe712331ba3ead97, 0xbe45a2b6897f04eb
  , 0xbe44c649eb07a4f4, 0x3fc03dc793b8b9fc, 0xbfe3d59378f83088, 0xbfe926073a32fd6d
  , 0xbf8e5cf15a3d211e, 0xbeaf71270822146b, 0x3e6dff6883eb781e, 0xbfecce0d5b7d1eff
  , 0xbee1b0ecb86c0de9, 0xbe44e06070aceaa8, 0xbe59fc04abb41770, 0xbee1745e46eb210d
  , 0xbe5108dc8bd0bed8, 0xbfdf6166641fa7ea, 0xbe9a0bac6bddf4ac, 0xbf07c92bae9b67c6
  , 0xbfbd87be21b38654, 0xbe56043d2596330c, 0xbe515643cbfe0765, 0xbe9d4fc7d4a14c7c
  , 0x3f2665fbf42cd159, 0xbf0ef52c62f0ee10, 0xbee22515566fb9e6, 0xbed07cf5aa1947f1
  , 0x3ff8762276ce1ce7, 0xbee90677bd0e3dc5, 0xbf082e8cc4e20a2d, 0xbef122e6b719db85 ]

-- | E.log2, E.log2With @14
log2Dut :: Dut Double (Vec 2 Double)
log2Dut clk rst en i = bundle (unary (E.log2 simFamily) clk rst en i :> unary (E.log2With @14 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log2Dut #-}

-- | Inputs of the base 2 logarithm: inputs in the two table segments whose
-- coefficients were observed from the IP (for which the computed coefficients
-- would give other results), inputs whose results depend on how the IP
-- truncates its products, and 'doublesIn'
log2DoublesIn :: Int -> Word64 -> [Double]
log2DoublesIn n seed = P.map castWord64ToDouble (log2ObservedWords <> log2DatapathWords) <> doublesIn 1 2046 n seed

-- | Inputs in segments 43 and 205 of the base 2 logarithm (12 each), see Note
-- [Observed double precision base 2 logarithm coefficients] in
-- "Clash.Cores.Intel.Floating.Internal.BitExactDouble"
log2ObservedWords :: [Word64]
log2ObservedWords =
  [ 0x3ff2bf9ec1bc0a5b, 0x3ff2bde90dab8344, 0x3ff2bbe8a549f000, 0x3ff2bd3bfb5c9400
  , 0x3ff2bf11db7e7639, 0x3ff2babb96a6b24c, 0x3ff2bd615a5a6119, 0x3ff2bdc59f0ec543
  , 0x3ff2be125e72875e, 0x3ff2bea2f4c00000, 0x3ff2be4c3cdcc000, 0x3ff2b157711d8800
  , 0x3ffcd9896d881ce4, 0x3ffcd084a592f4e4, 0x3ffcd0001322a39c, 0x3ffcd00009f254af
  , 0x3ffcd003a751520d, 0x3ffcd0000003eb0f, 0x3ffcd0004fab08ab, 0x3ffcd00007fd3fc4
  , 0x3ffcdc4ecefac056, 0x3ffcd75d1815f8c6, 0x3ffcd00001700448, 0x3ffcd04f54670000 ]

-- | Inputs for which the base 2 logarithm of MAX 10 differs from the one the
-- IP would compute with full multipliers
log2DatapathWords :: [Word64]
log2DatapathWords =
  [ 0x3feac6da0774a08b, 0x3fe70ff75fa866d0, 0x3ff07db8dd3f520b, 0x3ffc3243eae75387
  , 0x3fec0d12e1bfbb0f, 0x3ff0982693683bb3, 0x3fe0bc0bb4b7ec20, 0x3fe7deac5d3a2e65
  , 0x3fe2782f5f47ae22, 0x3ff7cdfeda146f64, 0x3ffd3c32e6724518, 0x3ff59e5a7ebef404
  , 0x3fefeef3e9437607, 0x3fe299934700272f, 0x3ff8e04514a95329, 0x3fe16334aa502531
  , 0x3fe8297247ec26f1, 0x3fed6bff5b142a5c, 0x3fe212659de88f13, 0x3ff4e6e833a5c174
  , 0x3fee8f31efe5fb78, 0x3ff7ac763e20c3e0, 0x3fe613debdc9dc3e, 0x3fe3f388c21586fa
  , 0x3fe76a9f80ae52b7, 0x3ff9acb6b5223b68, 0x3ffe1b388540829c, 0x3fe1ab111a3a0a1c
  , 0x3ffbfe12e8563401, 0x3ff4c22839b357c7, 0x3fedaebc083a128c, 0x3ff66e9b9da9b9f9
  , 0x3ff8c17383fe94bb, 0x3ff5bbc34804b7d7, 0x3feffdc360736f6b, 0x3ff3bc97505ab4ac
  , 0x3ff36a861dbb2f11, 0x3ffb394275a50cd8, 0x3ff3bf2673bc13cd, 0x3ff51d9ca9707937
  , 0x3fe3b676baa37e0f, 0x3ff6aae567db286e, 0x3ff58b2cb6180104, 0x3ff28e6d27953d2b
  , 0x3feb4a58368e0c0c, 0x3ff10b6011728acf, 0x3ff34a51bd7afd4a, 0x3fef7791de60e35b ]

-- | E.exp2, E.exp2With @10
exp2Dut :: Dut Double (Vec 2 Double)
exp2Dut clk rst en i = bundle (unary (E.exp2 simFamily) clk rst en i :> unary (E.exp2With @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE exp2Dut #-}

-- | Inputs of the base 2 exponential: inputs in the table segment whose
-- coefficients were observed from the IP (for which the computed coefficients
-- would give other results), inputs whose results depend on how the IP
-- truncates its products, 'specialDoubleWords', and @n@ random inputs of
-- several kinds: in the range where the result is finite and nonzero, in
-- @[0, 1)@ (where the result only depends on the polynomial), with random
-- exponents, and random bit patterns
exp2DoublesIn :: Int -> Word64 -> [Double]
exp2DoublesIn n seed =
  P.map castWord64ToDouble (exp2ObservedWords <> exp2DatapathWords <> specialDoubleWords <> P.take n (go (randoms seed)))
 where
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : rs) =
    let w = case k `mod` 4 of
          0 -> withExp x (0x3f8 + (k `shiftR` 8) `mod` 0x11)
          1 -> withExp x (0x3f9 + (k `shiftR` 8) `mod` 6) .&. 0x7fffffffffffffff
          2 -> withExp x (0x3c4 + (k `shiftR` 8) `mod` 0x45)
          _ -> x
     in w : go rs
  go _ = []

-- | Inputs in segment 50 of the base 2 exponential, see Note [Observed double
-- precision base 2 exponential coefficients] in
-- "Clash.Cores.Intel.Floating.Internal.BitExactDouble"
exp2ObservedWords :: [Word64]
exp2ObservedWords =
  [ 0x3fe979f9c52d5728, 0x3fe9772837c949e3, 0x3fe9571b8430ee50, 0x3fe97c3c83ee038e
  , 0x3fe97ebb1e1054fc, 0x3fe963aaed3fdf62, 0x3fe979eda99d60ff, 0x3fe97f36ecdd8f54
  , 0x3fe97ec3ca45f3ba, 0x3fe97c4e5a3a423b, 0x3fe95e29b42f592e, 0x3fe94afa0d67b7de
  , 0x3fe97e0a59c41502, 0x3fe932c3be92fcee, 0x3fe97b1974bd0000, 0x3fe95d1489fe7cda
  , 0x3fe910c3daff5d74, 0x3fe97bc733105068, 0x3fe9570b34290a12, 0x3fe97787ec99b400
  , 0x3fe965021c744706, 0x3fe91957b2d26fb4, 0x3fe96fdffb359d08, 0x3fe96a880f6e22a0 ]

-- | Inputs for which the base 2 exponential of MAX 10 differs from the one
-- the IP would compute with full multipliers
exp2DatapathWords :: [Word64]
exp2DatapathWords =
  [ 0x3fe9c2957dd5771c, 0x3fd15f768cf9d322, 0x3f8dc5328624ff40, 0x3fee6ec7bf3f5f49
  , 0x3fedf75557c6ba10, 0x3fd9ed22e66c4f40, 0x3fdec087560c2826, 0x3fe3027d499d5857
  , 0x3fd65e4537f210f0, 0x3fd7732d1d199fa2, 0x3fd43b550a2ff1ba, 0x3fcfc2a1f0f32598
  , 0x3fd2d00470c202ee, 0x3fb0ca0b181e70b0, 0x3fe7728ecb6654c6, 0x3fd4b9e1faca5630
  , 0x3fdabad662ae1f60, 0x3fd1bf968d630f78, 0x3fafeb999acc7020, 0x3fe0c54c215d48af
  , 0x3fd9e1660dfa3590, 0x3fb13ad5523aee40, 0x3fe07659e7f7a463, 0x3fcff21e22af9da4
  , 0x3fd0ee1d485098ec, 0x3fcb361594f8519c, 0x3fb0fb45123983b0, 0x3fe0c91de8b5284c
  , 0x3fd856093bdd1c76, 0x3fea43d536fde648, 0x3fe15fd339eff311, 0x3fec92d471ec88f5
  , 0x3fe27b5fb214f446, 0x3fe6c35813a3a634, 0x3fdf0a944776fa84, 0x3fd702d7615b697a
  , 0x3fdd634269493c84, 0x3f8718a763a4cfc0, 0x3fd5ac3278403ad2, 0x3fef64d1aa3ca45d
  , 0x3fed6c33500ca8aa, 0x3fe2c96093986897, 0x3feaaaf789190fbc, 0x3fb1fddcb105e928
  , 0x3fd5a3ced2561100, 0x3feaf0c8b6807697, 0x3fc2e56d9dc1cd74, 0x3fd2a1c4ed6eee6a ]
