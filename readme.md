# Haskell Advent of Code 2020
My solutions for Advent of Code 2020 in Haskell.  Here's some goals I
set to make the most out of it:

1. No looking up solutions/discussing with others before completion.
2. No use of libraries outside of the [GHC bootstrap
  libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html)
  for the solutions.
3. If time permits, use
  [Criterion](https://hackage.haskell.org/package/criterion) to
  benchmark and improve solutions.
4. No unsafe Haskell.

The reason for (2) is that many online competitive programming sites
that support Haskell (CodeForces, Google Code Jam) do not have
libraries beyond the bootstrap list.  Plus, there's already a wealth
of competitive libraries in there, such as `bytestring`, `text`,
`parsec`, `containers` (which contains maps, sets, int sets, graphs,
sequences) and more.

I might do a full writeup after my semester is over, but here's my
rough procedure on how to tackle the problems.

1. Write the most naive thing that could possibly work.  If it works,
   submit the answers!
2. Focus on algorithmic improvements.  I use a combination of
   techniques:
   - use equational reasoning to fuse folds, traversals
   - use more efficient data structures
3. Focus on empirical improvements.  I make heavy use of Criterion,
   though [AutoBench](https://github.com/mathandley/AutoBench) seems
   interesting.
   - manually inlining helper functions and equational reasoning, tail
     recursion
   - convert `foldr` to `foldl` when possible
   - using strict versions of functions, bang patterns
   - explicit type annotations
   - faster types: `Int` instead of `Integer`, `ByteString` instead of
     `String`, `Sequence` or `Vector` instead of `List`

Of course with (3) one could continue shaving off more and more time,
though these heuristics in practice have given me most of the gains.

## Best benchmarks so far
TODO: automatically generate report
<details>
<summary>CPU details</summary>

```
Architecture:                    x86_64
CPU op-mode(s):                  32-bit, 64-bit
Byte Order:                      Little Endian
Address sizes:                   39 bits physical, 48 bits virtual
CPU(s):                          4
On-line CPU(s) list:             0-3
Thread(s) per core:              2
Core(s) per socket:              2
Socket(s):                       1
NUMA node(s):                    1
Vendor ID:                       GenuineIntel
CPU family:                      6
Model:                           69
Model name:                      Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz
```
</details>

### Day 1
<details>

```
benchmarking day1/part1
time                 448.6 ns   (446.9 ns .. 450.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 447.6 ns   (446.4 ns .. 449.5 ns)
std dev              5.041 ns   (3.535 ns .. 8.317 ns)

benchmarking day1/part2
time                 27.75 μs   (27.65 μs .. 27.84 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 27.75 μs   (27.68 μs .. 27.84 μs)
std dev              264.1 ns   (188.8 ns .. 350.6 ns)
```
</details>

### Day 2
<details>

```
benchmarking day2/part1
time                 102.3 μs   (97.14 μs .. 110.2 μs)
                     0.966 R²   (0.936 R² .. 1.000 R²)
mean                 99.73 μs   (96.97 μs .. 106.5 μs)
std dev              12.75 μs   (1.664 μs .. 21.92 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking day2/part2
time                 66.20 μs   (66.01 μs .. 66.40 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 66.29 μs   (66.07 μs .. 66.69 μs)
std dev              1.007 μs   (637.7 ns .. 1.420 μs)
```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 8.454 μs   (8.410 μs .. 8.513 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.543 μs   (8.460 μs .. 8.679 μs)
std dev              366.2 ns   (228.0 ns .. 547.7 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking day3/part2
time                 39.46 μs   (39.17 μs .. 39.74 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 39.44 μs   (39.20 μs .. 39.89 μs)
std dev              1.087 μs   (728.8 ns .. 1.619 μs)
variance introduced by outliers: 27% (moderately inflated)
```
</details>
