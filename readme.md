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
time                 25.33 μs   (25.21 μs .. 25.49 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 25.39 μs   (25.28 μs .. 25.57 μs)
std dev              472.6 ns   (340.0 ns .. 636.7 ns)
variance introduced by outliers: 16% (moderately inflated)

benchmarking day2/part2
time                 12.15 μs   (12.10 μs .. 12.23 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.24 μs   (12.18 μs .. 12.37 μs)
std dev              305.9 ns   (195.5 ns .. 521.6 ns)
variance introduced by outliers: 27% (moderately inflated)
```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 11.91 μs   (11.88 μs .. 11.93 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.92 μs   (11.88 μs .. 11.98 μs)
std dev              165.8 ns   (113.6 ns .. 264.1 ns)
variance introduced by outliers: 10% (moderately inflated)

benchmarking day3/part2
time                 26.72 μs   (26.12 μs .. 27.31 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 25.96 μs   (25.73 μs .. 26.36 μs)
std dev              986.3 ns   (621.4 ns .. 1.451 μs)
variance introduced by outliers: 44% (moderately inflated)
```
</details>

### Day 4
<details>

```
benchmarking day4/part1
time                 113.1 μs   (111.3 μs .. 114.9 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 112.6 μs   (111.5 μs .. 114.2 μs)
std dev              4.365 μs   (3.053 μs .. 6.519 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking day4/part2
time                 2.191 ms   (2.175 ms .. 2.204 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.238 ms   (2.210 ms .. 2.336 ms)
std dev              160.4 μs   (48.47 μs .. 327.6 μs)
variance introduced by outliers: 51% (severely inflated)
```
</details>
