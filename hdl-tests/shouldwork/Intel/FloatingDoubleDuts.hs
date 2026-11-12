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

