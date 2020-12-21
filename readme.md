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

## Writing Haskell for AoC quickly
AoC is all about solving the problem quickly via any means, so

> a naive (but possibly inefficient) solution that is quick to write
> and produces the answer is better than an optimized one that is slow
> to write

From this principle, it informs my choices on how to write Haskell
quickly for the initial solve.

- put everything in main, variables/expressions used more than once go
  in a `let` expression, use a short, random identifier unless you can
  think of a good one within 3 seconds
- for mapping over lists, use `<$>`, but if you need to filter, use
  `let` expressions or iterate over a cartesian product of lists, list
  comprehensions/monadic syntax is better
- never discard parts of input even if it makes part 1 faster to
  solve, since almost always part 2 will use that information and
  you'll have to adjust the `String -> data` step again
- avoid explicit recursion when possible, or at the least use tail
  recursion with a function named `go`
- print intermediate results as you process data, especially if the
  transformation is complex, so `do { let a = f x; print a; let b = g
  a; ... }` over `do { print (f (g x)) }`
- if using the state monad (should be a last resort), use a record for
  the state

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
time                 179.3 μs   (177.4 μs .. 181.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 178.6 μs   (177.1 μs .. 180.4 μs)
std dev              5.729 μs   (3.968 μs .. 8.236 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking day9/part2
time                 13.13 μs   (13.07 μs .. 13.20 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.13 μs   (13.06 μs .. 13.23 μs)
std dev              266.3 ns   (184.4 ns .. 370.2 ns)
variance introduced by outliers: 19% (moderately inflated)
```
</details>

### Day 10
<details>

```
benchmarking day10/part1
time                 1.043 μs   (1.016 μs .. 1.073 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 1.036 μs   (1.022 μs .. 1.068 μs)
std dev              66.24 ns   (32.12 ns .. 112.3 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking day10/part2
time                 10.77 μs   (10.64 μs .. 10.94 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 10.89 μs   (10.73 μs .. 11.08 μs)
std dev              591.3 ns   (468.3 ns .. 793.8 ns)
variance introduced by outliers: 64% (severely inflated)
```
</details>

### Day 11
<details>

```
benchmarking day11/part1
time                 786.8 ms   (733.2 ms .. NaN s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 807.9 ms   (790.7 ms .. 826.1 ms)
std dev              22.23 ms   (11.00 ms .. 28.15 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking day11/part2
time                 1.617 s    (1.569 s .. 1.713 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.625 s    (1.607 s .. 1.645 s)
std dev              23.63 ms   (8.665 ms .. 31.22 ms)
variance introduced by outliers: 19% (moderately inflated)
```
</details>

### Day 12
<details>

```
benchmarking day12/part1
time                 9.821 μs   (9.699 μs .. 9.981 μs)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 9.858 μs   (9.764 μs .. 10.11 μs)
std dev              457.1 ns   (236.7 ns .. 852.9 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking day12/part2
time                 17.61 μs   (17.48 μs .. 17.73 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.51 μs   (17.43 μs .. 17.59 μs)
std dev              271.1 ns   (223.0 ns .. 338.6 ns)
variance introduced by outliers: 12% (moderately inflated)
```
</details>

### Day 13
<details>

```
benchmarking day13/part1
time                 845.1 ns   (841.6 ns .. 849.8 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 847.3 ns   (843.4 ns .. 856.5 ns)
std dev              18.78 ns   (10.07 ns .. 33.84 ns)
variance introduced by outliers: 28% (moderately inflated)

benchmarking day13/part2
time                 9.215 μs   (9.182 μs .. 9.257 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.208 μs   (9.167 μs .. 9.263 μs)
std dev              158.8 ns   (121.9 ns .. 209.0 ns)
variance introduced by outliers: 15% (moderately inflated)
```
</details>

### Day 14
<details>

```
benchmarking day14/part1
time                 545.2 μs   (540.3 μs .. 552.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 542.5 μs   (540.1 μs .. 546.8 μs)
std dev              10.63 μs   (6.327 μs .. 18.41 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking day14/part2
time                 46.26 ms   (45.08 ms .. 48.46 ms)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 45.57 ms   (45.13 ms .. 46.43 ms)
std dev              1.216 ms   (655.2 μs .. 2.038 ms)
```
</details>

### Day 15
<details>

```
benchmarking day15/part1
time                 320.8 μs   (316.7 μs .. 326.0 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 318.6 μs   (315.7 μs .. 323.1 μs)
std dev              11.70 μs   (8.501 μs .. 16.76 μs)
variance introduced by outliers: 32% (moderately inflated)

./day15 part2  47.38s user 0.35s system 99% cpu 47.735 total
```
</details>

### Day 16
<details>

```
benchmarking day16/part1
time                 32.59 μs   (32.22 μs .. 33.06 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 32.36 μs   (32.22 μs .. 32.62 μs)
std dev              564.0 ns   (345.3 ns .. 856.7 ns)
variance introduced by outliers: 14% (moderately inflated)

benchmarking day16/part2
time                 15.24 ms   (14.91 ms .. 15.65 ms)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 15.14 ms   (14.96 ms .. 15.44 ms)
std dev              510.9 μs   (302.4 μs .. 863.4 μs)
variance introduced by outliers: 11% (moderately inflated)
```
</details>

### Day 18
<details>

```
benchmarking day18/part1
time                 5.435 ms   (5.383 ms .. 5.490 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.466 ms   (5.423 ms .. 5.538 ms)
std dev              167.9 μs   (103.6 μs .. 304.1 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking day18/part2
time                 5.780 ms   (5.728 ms .. 5.841 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.756 ms   (5.722 ms .. 5.805 ms)
std dev              126.0 μs   (89.81 μs .. 179.4 μs)
```

### Day 19
<details>

```
benchmarking day19/part1
time                 7.695 ms   (7.588 ms .. 7.800 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 7.775 ms   (7.649 ms .. 7.933 ms)
std dev              408.1 μs   (314.2 μs .. 543.3 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking day19/part2
time                 70.92 ms   (70.21 ms .. 71.47 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 70.95 ms   (70.49 ms .. 71.45 ms)
std dev              873.7 μs   (599.9 μs .. 1.360 ms)
```

### Day 20
<details>

```
benchmarking day20/part1
time                 290.8 μs   (289.1 μs .. 292.4 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 292.6 μs   (289.8 μs .. 300.0 μs)
std dev              13.22 μs   (3.743 μs .. 28.20 μs)
variance introduced by outliers: 42% (moderately inflated)
```
</details>

### Day 21
<details>

```
benchmarking day21/part1
time                 3.511 ms   (3.457 ms .. 3.564 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 3.502 ms   (3.457 ms .. 3.560 ms)
std dev              164.3 μs   (132.8 μs .. 211.0 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking day21/part2
time                 2.215 ms   (2.178 ms .. 2.269 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 2.242 ms   (2.211 ms .. 2.291 ms)
std dev              132.4 μs   (91.84 μs .. 198.3 μs)
variance introduced by outliers: 42% (moderately inflated)
```
</details>
