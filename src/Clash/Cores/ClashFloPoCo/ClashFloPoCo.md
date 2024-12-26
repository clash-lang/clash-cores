# Clash library for interacting with FloPoCo
This is the library contains API to interact with FloPoCo to generate
hardware synthesizable floating point arithmetics function.
It can also be used with Float type in haskell
# How to use it
1. Install FloPoCo library
On Linux, user can run to go to FloPoCo source code follow the instruction.
For Window, user must enable Window Subsystem Linux to install FloPoCo as
FloPoCo depends on Sollya which is only supported in Linux.
2. Learn to use FloPoCo
Users should learn and generate their own floating point core by using FloPoCo.
3. Tutorial
For the IEEE-754 single precision floating point, FloPoCo offers 3 arithmetics operations:
addition, fuse-multiply addition, and exponent.
The first thing is to use the function genFloPoCoInfoEntity to generate desire
floating point core for custom usage. This function returns the InfoEntity as the Q Expression
in template haskell, so user has to define its inputs: flopoco path, flopoco arguments,
and flopoco VHDL file in a seperate haskell file. After that, users can make another haskell file
to create InfoEntity for their custom usage. Here is the example of how to generate a custom
floating point addition using FloPoCo and use it with Clash.
In a seperate haskell file:
```haskell
floPoCoPathExample = "home/user/flopoco/build/bin/flopoco"
argsPlusExample = ["frequency=100", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "outputFile=flopocoAdd.vhdl","registerLargeTables=1"]
filePlusExample = "flopocoAdd.vhdl"
```
It's important to have custom name for the generated floating point entity or the lexer function
cannot retrieve the name of the entity.
In a seperate haskell file:
```haskell
infoEnPlusExample = $(genFloPoCoInfoEntity floPoCoPathExample argsPlusExample filePlusExample)
```
The infoEnPlusExample will be defined like this during the compile time:
```haskell
infoEnPlusExample :: InfoEntity
infoEnPlusExample = InfoEntity {
    name = Just "plusFloat",
    freq = Just 100,
    pipedep = Just 2,
    insig = Just ["clk", "X", "Y"],
    outsig = Just ["R"]
}
```
Then users can write their own implementation for floating point addition in Clash and binding it
with the blackbox like in this example:
```haskell
import Clash.Annotations.Primitive (Primitive (..))
import Clash.Backend (Backend)
import Clash.Cores.ClashFloPoCo.InfoEn
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction,
    BlackBoxMeta (..),
    TemplateKind (..),
    emptyBlackBoxMeta,
  )
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( BlackBox (..),
    BlackBoxContext,
    EntityOrComponent (..),
    TemplateFunction (..),
  )
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Control.Monad.State.Lazy (State)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)
import Clash.Cores.ClashFloPoCo.GenTemDSL
import Clash.Prelude

type Nplus = $(getPipeDep infoEnPlusExample)
xp :: SNat Nplus
xp = SNat::SNat Nplus
plusFloatExample
  :: forall n .
  Clock XilinxSystem -- ^Clock signal
  -> DSignal XilinxSystem n Float -- ^Operand input signal
  -> DSignal XilinxSystem n Float -- ^Operand input signal
  -> DSignal XilinxSystem (n + Nplus) Float -- ^Result output signal
plusFloatExample clk a b =
  delayN xp undefined enableGen clk (liftA2 (+) a b)
{-# OPAQUE plusFloatExample #-}
$(genBlackBox infoEnPlusExample)
{-# ANN plusFloatExample (
    let
      primName = show 'plusFloatExample
      tfName = show 'plusFloatBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}
```
For more example, please look at the file FloPoCoExample.hs.
For the VHDL synthesis, user need to create file cabal.project and add these lines.
```
-- This is add for VHDL block generation used by ClashFloPoCo library
source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: 5706eafca799ae04fda0ee7d666a40b6c0e7f22b
  subdir: clash-prelude

source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: 5706eafca799ae04fda0ee7d666a40b6c0e7f22b
  subdir: clash-ghc

source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: 5706eafca799ae04fda0ee7d666a40b6c0e7f22b
  subdir: clash-lib

```
