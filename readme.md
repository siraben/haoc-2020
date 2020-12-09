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
time                 13.41 μs   (13.37 μs .. 13.45 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 13.51 μs   (13.41 μs .. 13.85 μs)
std dev              551.3 ns   (152.7 ns .. 1.126 μs)
variance introduced by outliers: 49% (moderately inflated)

benchmarking day4/part2
time                 2.239 ms   (2.223 ms .. 2.254 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.269 ms   (2.249 ms .. 2.311 ms)
std dev              98.12 μs   (46.41 μs .. 174.5 μs)
variance introduced by outliers: 28% (moderately inflated)
```
</details>

### Day 5
<details>

```
benchmarking day5/part1
time                 16.39 μs   (16.29 μs .. 16.51 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.34 μs   (16.31 μs .. 16.40 μs)
std dev              136.6 ns   (84.33 ns .. 208.5 ns)

benchmarking day5/part2
time                 16.39 μs   (16.31 μs .. 16.51 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.38 μs   (16.33 μs .. 16.45 μs)
std dev              188.9 ns   (109.6 ns .. 276.3 ns)
```
</details>

### Day 6
<details>

```
benchmarking day6/part1
time                 13.47 μs   (13.30 μs .. 13.67 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 14.08 μs   (13.79 μs .. 14.41 μs)
std dev              1.105 μs   (919.3 ns .. 1.464 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking day6/part2
time                 12.00 μs   (11.94 μs .. 12.08 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 12.15 μs   (12.04 μs .. 12.41 μs)
std dev              536.6 ns   (266.7 ns .. 981.4 ns)
variance introduced by outliers: 53% (severely inflated)
```
</details>

### Day 7
<details>

```
benchmarking day7/part1
time                 1.990 ms   (1.946 ms .. 2.052 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.957 ms   (1.929 ms .. 1.988 ms)
std dev              97.12 μs   (75.89 μs .. 139.0 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking day7/part2
time                 532.3 μs   (529.0 μs .. 536.7 μs)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 538.0 μs   (532.5 μs .. 550.4 μs)
std dev              26.07 μs   (15.15 μs .. 44.87 μs)
variance introduced by outliers: 42% (moderately inflated)
```
</details>

### Day 8
<details>

```
benchmarking day8/part1
time                 24.09 μs   (23.95 μs .. 24.27 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 24.22 μs   (24.02 μs .. 24.48 μs)
std dev              781.9 ns   (598.8 ns .. 1.083 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking day8/part2
time                 12.14 ms   (12.03 ms .. 12.25 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 12.07 ms   (11.97 ms .. 12.23 ms)
std dev              322.7 μs   (211.3 μs .. 509.9 μs)
```
</details>

### Day 9
<details>

```
benchmarking day9/part1
time                 385.2 μs   (380.6 μs .. 392.0 μs)
                     0.998 R²   (0.997 R² .. 0.998 R²)
mean                 394.2 μs   (389.0 μs .. 401.0 μs)
std dev              19.72 μs   (16.62 μs .. 24.11 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking day9/part2
time                 1.140 ms   (1.127 ms .. 1.153 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.132 ms   (1.125 ms .. 1.140 ms)
std dev              23.69 μs   (20.28 μs .. 28.34 μs)
```
</details>
