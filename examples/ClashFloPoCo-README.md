# ClashFloPoCo Example

This directory contains a demonstration of Clash FloPoCo integration for hardware floating-point operations.

## Overview

The example shows how to:
1. Define FloPoCo IP core parameters using `InfoEntity`
2. Generate blackbox functions using Template Haskell
3. Create pipelined floating-point operations
4. Properly handle pipeline delays in Clash circuits

## Files

- **`Example.hs`**: Main module exporting `floPoCoAdder` - a pipelined single-precision FP adder
- **`FloPoCoDemo.hs`**: Standalone example that can be compiled to HDL
- **`FloPoCoAdderTest.hs`**: Test module with testbench (in `hdl-tests/shouldwork/ClashFloPoCo/`)

## Usage

### Building the Library

```bash
cabal build clash-cores
```

### Using in Your Design

```haskell
import Clash.Explicit.Prelude
import Clash.Cores.ClashFloPoCo.Example

myCircuit :: Clock XilinxSystem -> Signal XilinxSystem Float -> Signal XilinxSystem Float -> Signal XilinxSystem Float
myCircuit clk a b =
  let a' = toSignal (fromSignal a :: DSignal XilinxSystem 0 Float)
      b' = toSignal (fromSignal b :: DSignal XilinxSystem 0 Float)
  in toSignal (floPoCoAdder clk a' b')
```

### Generating HDL

To generate VHDL from the demo (requires `clash` executable):

```bash
cabal run clash -- examples/FloPoCoDemo.hs --vhdl
```

This will create VHDL files in the `vhdl/` directory.

## Key Concepts

### InfoEntity

The `InfoEntity` record defines the FloPoCo IP core parameters:

```haskell
floPoCoAdderInfo :: InfoEntity
floPoCoAdderInfo = InfoEntity
  { name = Just "floPoCoAdder"     -- Component name
  , freq = Just 100                -- Target frequency (MHz)
  , pipedep = Just 2               -- Pipeline depth (cycles)
  , insig = Just ["clk", "X", "Y"] -- Input signal names
  , outsig = Just ["R"]            -- Output signal names
  }
```

### Pipeline Depth

The `getPipeDep` Template Haskell function extracts the pipeline depth as a type-level natural:

```haskell
type PipelineDepth = $(getPipeDep floPoCoAdderInfo)  -- PipelineDepth ~ 2
```

This is used to properly delay the output signal to match the hardware pipeline.

### Blackbox Generation

The `genBlackBoxProd` TH splice generates all required blackbox functions:

```haskell
$(genBlackBoxProd floPoCoAdderInfo)
```

This creates:
- `floPoCoAdderBBTF` - BlackBox template function
- `floPoCoAdderTF` - Template function
- `floPoCoAdderBBF` - BlackBox function

### Simulation Model

For simulation, the design uses MPFR (Multiple Precision Floating-Point Reliable) library:

```haskell
mpfrAddFloat :: Float -> Float -> Float
mpfrAddFloat a b = realToFrac $ unsafePerformIO $ mpfrAdd 24 (realToFrac a) (realToFrac b)
```

This provides bit-accurate floating-point operations matching the FloPoCo IP behavior.

## Testing

Run the test suite:

```bash
cd hdl-tests
cabal run cores-hdl-tests -- -p FloPoCoAdderTest
```

Note: This requires a properly configured Clash installation with all dependencies.

## Implementation Details

### Generated Blackbox

The TH code generates a blackbox that:
1. Extracts input signals from the Clash context
2. Creates component instantiation with proper port mappings
3. Handles the FloPoCo IP core interface
4. Returns the result with correct type information

### Pipeline Matching

The `delayN` function ensures the output is delayed to match the hardware pipeline:

```haskell
floPoCoAdder clk a b = delayN pipelineDepth undefined enableGen clk (liftA2 mpfrAddFloat a b)
```

This maintains cycle-accurate simulation behavior.

## Extending the Example

To create other FloPoCo operations (multiply, divide, etc.):

1. Define a new `InfoEntity` with appropriate signals
2. Implement a reference function using MPFR
3. Create the Clash function with proper delays
4. Generate blackboxes using `$(genBlackBoxProd ...)`
5. Annotate with `{-# ANN ... InlineYamlPrimitive ... #-}`

See `FloPoCoExample.hs` for more examples (FMA, exponential, etc.).
