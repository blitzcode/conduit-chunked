
# conduit-chunking

### The Problem

http://www.reddit.com/r/haskell/comments/29nvsx/how_to_get_good_performance_when_processing/

### The Solution

Getting decent performance requires verbose, unsafe, low-level code. Anything with my preferred await / yield semantics is really slow, even just the overhead from the monad stack is enough to make it unworkable. Using a high-level language and a library like conduit is kinda pointless if I have to do the work with `mallocByteString` and `pokeByteOff` and can't even be in the conduit monad to transparently yield the finished chunk. At that point, I'm just writing clumsy C code in Haskell. Will have to give this one another look in the future.

### Benchmark

```
benchmarking 1M file size/Speed-of-light
mean: 21.26010 ms, lb 20.58214 ms, ub 21.77533 ms, ci 0.950
std dev: 1.004654 ms, lb 704.6951 us, ub 1.316683 ms, ci 0.950
variance introduced by outliers: 9.300%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with list
collecting 10 samples, 1 iterations each, in estimated 5.224149 s
mean: 532.0543 ms, lb 528.5182 ms, ub 538.2167 ms, ci 0.950
std dev: 7.752959 ms, lb 4.097320 ms, ub 12.28790 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high mild
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with blaze-builder
mean: 238.7372 ms, lb 231.6125 ms, ub 258.7150 ms, ci 0.950
std dev: 19.71395 ms, lb 3.836165 ms, ub 33.21432 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high severe
variance introduced by outliers: 27.938%
variance is moderately inflated by outliers

benchmarking 1M file size/Chunking with raw memory buffer
mean: 22.08381 ms, lb 20.92055 ms, ub 23.36200 ms, ci 0.950
std dev: 2.079799 ms, lb 1.639464 ms, ub 2.673748 ms, ci 0.950
variance introduced by outliers: 28.414%
variance is moderately inflated by outliers

benchmarking 1M file size/Chunking with chunk builder
collecting 10 samples, 1 iterations each, in estimated 24.57181 s
mean: 2.470865 s, lb 2.468428 s, ub 2.474412 s, ci 0.950
std dev: 4.953276 ms, lb 3.091434 ms, ub 7.440388 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high mild
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with state
collecting 10 samples, 1 iterations each, in estimated 22.02394 s
mean: 2.147976 s, lb 2.144977 s, ub 2.155977 s, ci 0.950
std dev: 7.983189 ms, lb 2.276935 ms, ub 13.26398 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high severe
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers
```
