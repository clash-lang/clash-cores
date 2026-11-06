{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Circuits and stimuli for "SbRam40"
module SbRam40Types where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.Memory
import Ice40TestCommon

import Language.Haskell.TH (ExpQ)

import qualified Prelude as P

-- | Number of cycles of each test bench
type Cycles = 600

-- | Inputs of 'sbRam40_4k', with raw addresses and data. The circuit takes the
-- bits it needs.
data RamIn = RamIn
  { wClkE :: Bool
  , we :: Bool
  , wAddr :: BitVector 11
  , wData :: BitVector 16
  , mask :: BitVector 16
  , rClkE :: Bool
  , re :: Bool
  , rAddr :: BitVector 11
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | Convert a raw address to an address of a mode
toIndex :: forall n. KnownNat n => BitVector 11 -> Index n
toIndex a = fromIntegral (a .&. (natToNum @n - 1))

-- | 'sbRam40_4k' with both ports in the same domain. Outputs @RDATA@.
ramCircuit ::
  forall dom depthW widthW depthR widthR.
  ( KnownDomain dom
  , KnownNat depthW, KnownNat widthW, KnownNat depthR, KnownNat widthR ) =>
  RamMode depthW widthW ->
  RamMode depthR widthR ->
  RamInit ->
  Clock dom ->
  Signal dom RamIn ->
  Signal dom (BitVector widthR)
ramCircuit wMode rMode initial clk i =
  sbRam40_4k
    wMode
    rMode
    initial
    clk
    (toEnable (wClkE <$> i))
    (toWrite <$> i)
    (mask <$> i)
    clk
    (toEnable (rClkE <$> i))
    (re <$> i)
    (toIndex . rAddr <$> i)
 where
  toWrite x
    | we x = Just (toIndex (wAddr x), resize (wData x))
    | otherwise = Nothing
{-# INLINE ramCircuit #-}

-- | An address in the first 16 words, with random bits selecting the narrow
-- word, so that reads often hit written words.
genAddr :: G (BitVector 11)
genAddr = do
  word <- genBV @4
  sub <- genBV @3
  pure (sub ++# 0 ++# word)

genRamIn :: G RamIn
genRamIn = do
  wClkE' <- chance 85
  we' <- chance 50
  wAddr' <- genAddr
  wData' <- genBV
  mask' <- chance 30 >>= \m -> if m then genBV else pure 0
  rClkE' <- chance 85
  re' <- chance 85
  rAddr' <- genAddr
  pure
    RamIn
      { wClkE = wClkE'
      , we = we'
      , wAddr = wAddr'
      , wData = wData'
      , mask = mask'
      , rClkE = rClkE'
      , re = re'
      , rAddr = rAddr'
      }

ramStimuli :: Int -> [RamIn]
ramStimuli = evalG (sequence (P.replicate (natToNum @Cycles) genRamIn))

-- | Random initial contents
randomInit :: Int -> Vec 256 (BitVector 16)
randomInit = evalG (sequence (repeat genBV))

ramBlobs ::
  forall dom depthW widthW depthR widthR.
  ( KnownDomain dom
  , KnownNat depthW, KnownNat widthW, KnownNat depthR, KnownNat widthR ) =>
  RamMode depthW widthW ->
  RamMode depthR widthR ->
  RamInit ->
  Int ->
  ExpQ
ramBlobs wMode rMode initial seed =
  blobs @dom (ramCircuit wMode rMode initial) (ramStimuli seed)

-- | Initial contents of a 2048x2 memory: word /a/ is @a `mod` 4@
init2048x2 :: RamInit
init2048x2 = ramInit RamMode2048x2 (map (resize . pack) (indicesI @2048))

-- | 'sbRam40_4k' with the write port in 'Dom' and the read port in 'Fast'.
-- Outputs nothing in 'Dom' and @RDATA@ in 'Fast'.
ramCircuitDual :: DdrCircuit RamIn RamIn Bit (BitVector 16)
ramCircuitDual clkW clkR iW iR = (pure 0, rData)
 where
  rData =
    sbRam40_4k
      RamMode256x16
      RamMode256x16
      ramInitZero
      clkW
      (toEnable (wClkE <$> iW))
      (toWrite <$> iW)
      (mask <$> iW)
      clkR
      (toEnable (rClkE <$> iR))
      (re <$> iR)
      (toIndex . rAddr <$> iR)
  toWrite x
    | we x = Just (toIndex (wAddr x), wData x)
    | otherwise = Nothing

ramDualBlobs :: Int -> ExpQ
ramDualBlobs seed = blobsDdr ramCircuitDual (ramStimuli seed) (ramStimuli (seed + 1000) <> ramStimuli (seed + 2000))
