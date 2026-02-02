# MPFR Integration Enhancements

## Summary

Enhanced the MPFR (Multiple Precision Floating-Point Reliable Library) FFI bindings in clash-cores with user-controllable rounding modes and validated struct size assumptions. All tests are now consolidated into a single module with proper Hedgehog property-based testing.

## Changes Made

### 1. Rounding Mode Support (`src/Clash/Cores/ClashFloPoCo/MPFR.hs`)

Added explicit rounding mode variants for all MPFR operations:

- `mpfrAddRnd` - Addition with specified rounding mode
- `mpfrMulRnd` - Multiplication with specified rounding mode  
- `mpfrSubRnd` - Subtraction with specified rounding mode
- `mpfrDivRnd` - Division with specified rounding mode

The original functions (`mpfrAdd`, `mpfrMul`, etc.) remain available and use the default `rndN` (round to nearest) mode for convenience.

**Available Rounding Modes:**
- `rndN` - Round to nearest (ties to even)
- `rndZ` - Round toward zero (truncate)
- `rndU` - Round toward +infinity (ceiling)
- `rndD` - Round toward -infinity (floor)
- `rndA` - Round away from zero

### 2. Struct Size Improvements

**Increased allocation size from 32 to 64 bytes:**
```haskell
withMPFR :: CLong -> (MPFRPtr -> IO a) -> IO a
withMPFR prec action = do
  let structSize = (64 :: Int)  -- Conservative estimate for mpfr_t header
  allocaBytes structSize $ \ptr -> do
    c_mpfr_init2 ptr prec
    result <- action ptr
    c_mpfr_clear ptr
    return result
```

**Rationale:**
- The `mpfr_t` struct typically contains 4 fields (~32 bytes on 64-bit platforms)
- Using 64 bytes provides safety margin across different platforms and configurations
- MPFR manages the actual precision limbs separately via internal allocation

**Added FFI import for size verification:**
```haskell
foreign import ccall "mpfr.h mpfr_custom_get_size"
  c_mpfr_custom_get_size :: CLong -> CSize
```

### 3. Consolidated Test Suite (`test/Test/Cores/ClashFloPoCo/MPFR.hs`)

**Refactored test structure:**
- **Single module**: All MPFR tests consolidated into `Test.Cores.ClashFloPoCo.MPFR`
- **Removed**: `Test.Cores.ClashFloPoCo.MPFRRounding` module (merged into main MPFR tests)
- **Proper generators**: All property tests now use explicit Hedgehog generators
- **28 total tests**: 24 property-based tests + 4 unit tests

**Test Organization:**

1. **Generators** (for property-based testing):
   - `genFiniteDouble` - Finite doubles for basic tests
   - `genSmallDouble` - Small doubles for precision comparisons
   - `genNonZeroDouble` - Non-zero doubles (avoids division by zero)
   - `genPrecision` - Precision values (24-128 bits)
   - `genLowPrecision` - Low precision for rounding tests (24-53 bits)
   - `genStablePrecision` - Higher precision for stability (53-128 bits)
   - `genRoundingMode` - All 5 rounding modes

2. **Property Tests** (using Hedgehog):
   - Addition: commutativity, associativity, identity, matches Haskell Double
   - Multiplication: commutativity, associativity, identity, zero property, matches Double
   - Subtraction, division, logarithm, exponential, sine, cosine: match Haskell Double
   - Absolute value and comparison: match Haskell Double
   - **Rounding modes**: different modes differ, toward zero behavior, consistency, up vs down
   - Precision: parameter has effect

3. **Unit Tests**:
   - Edge cases: NaN, positive infinity, negative infinity, multiply by infinity
   - Struct size validation: 1000-operation stress test, size adequacy check

## Test Results

All 28 MPFR tests pass successfully:
- 24 property-based tests (100 cases each = 2,400 total test cases)
- 4 unit tests for edge cases and struct validation

**Test execution time:** ~0.06-0.2 seconds

## Usage Examples

### Basic usage (default rounding to nearest):
```haskell
result <- mpfrAdd 128 3.14159 2.71828
```

### With explicit rounding mode:
```haskell
-- Round toward zero
resultTrunc <- mpfrDivRnd 64 rndZ 7.0 3.0

-- Round toward +infinity  
resultCeil <- mpfrDivRnd 64 rndU 7.0 3.0

-- Round toward -infinity
resultFloor <- mpfrDivRnd 64 rndD 7.0 3.0
```

## Benefits

1. **User Control**: Applications can now specify rounding behavior for financial, scientific, or interval arithmetic use cases
2. **Safety**: Increased struct size eliminates potential memory corruption issues
3. **Validation**: Comprehensive property-based test suite with explicit generators ensures correctness
4. **Backward Compatibility**: Existing code continues to work with default rounding mode
5. **Maintainability**: Single consolidated test module with proper Hedgehog generators

## Test Refactoring Details

**Before:**
- Tests split across two modules: `MPFR.hs` and `MPFRRounding.hs`
- Mixed unit tests and implicit property tests
- Some tests used inline generators without proper Hedgehog structure

**After:**
- Single module: `Test.Cores.ClashFloPoCo.MPFR`
- All property tests use explicit, named generators
- Clear separation between generators, properties, and unit tests
- Better handling of edge cases (infinity, extreme values) in generators
- Consistent test structure throughout

## Future Work

Potential enhancements:
- Expose `c_mpfr_custom_get_size` as a public API for runtime size queries
- Add rounding mode variants for logarithm, sin, cos, and other transcendental functions
- Consider using `mpfr_custom_get_size` to dynamically determine optimal struct allocation size
- Add more property tests for rounding mode behavior with transcendental functions

## References

- MPFR Documentation: https://www.mpfr.org/
- MPFR Rounding Modes: https://www.mpfr.org/mpfr-current/mpfr.html#Rounding-Modes
- IEEE 754 Rounding: https://en.wikipedia.org/wiki/IEEE_754#Rounding_rules
- Hedgehog Testing: https://hackage.haskell.org/package/hedgehog

