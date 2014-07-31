
# conduit-chunking

### The Problem

http://www.reddit.com/r/haskell/comments/29nvsx/how_to_get_good_performance_when_processing/

### The Solution

Getting decent performance requires verbose, unsafe, low-level code. Anything with my preferred await / yield semantics is really slow, even just the overhead from the monad stack is enough to make it unworkable. Using a high-level language and a library like conduit is kinda pointless if I have to do the work with `mallocByteString` and `pokeByteOff` and can't even be in the conduit monad to transparently yield the finished chunk. At that point, I'm just writing clumsy C code in Haskell. Will have to give this one another look in the future.

#### Update ####

https://www.fpcomplete.com/user/snoyberg/library-documentation/vectorbuilder

The `conduit-combinators` package has been update with a new conduit named `vectorBuilder`, addressing this very problem. The solution is nice and performance is close enough. Solved!

### Benchmark

```
benchmarking 1M file size/Speed-of-light
mean: 21.33466 ms, lb 19.66938 ms, ub 23.58114 ms, ci 0.950
std dev: 3.271833 ms, lb 2.207320 ms, ub 4.711354 ms, ci 0.950
variance introduced by outliers: 48.339%
variance is moderately inflated by outliers

benchmarking 1M file size/Chunking with list
collecting 10 samples, 1 iterations each, in estimated 8.989661 s
mean: 919.7806 ms, lb 917.1447 ms, ub 923.0766 ms, ci 0.950
std dev: 5.015212 ms, lb 3.335758 ms, ub 7.501738 ms, ci 0.950
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with blaze-builder
mean: 339.6164 ms, lb 336.6435 ms, ub 344.2831 ms, ci 0.950
std dev: 6.200981 ms, lb 3.655564 ms, ub 9.273673 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high mild
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with raw memory buffer
mean: 24.95672 ms, lb 20.45549 ms, ub 34.69759 ms, ci 0.950
std dev: 10.78405 ms, lb 4.345717 ms, ub 17.91868 ms, ci 0.950
found 1 outliers among 10 samples (10.0%)
  1 (10.0%) high severe
variance introduced by outliers: 89.322%
variance is severely inflated by outliers

benchmarking 1M file size/Chunking with chunk builder
collecting 10 samples, 1 iterations each, in estimated 26.95480 s
mean: 2.724945 s, lb 2.718691 s, ub 2.730379 s, ci 0.950
std dev: 9.911362 ms, lb 6.669448 ms, ub 15.00682 ms, ci 0.950
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with state
collecting 10 samples, 1 iterations each, in estimated 17.52748 s
mean: 1.716773 s, lb 1.711659 s, ub 1.722977 s, ci 0.950
std dev: 9.605795 ms, lb 7.260816 ms, ub 12.36112 ms, ci 0.950
variance introduced by outliers: 9.000%
variance is slightly inflated by outliers

benchmarking 1M file size/Chunking with vectorBuilder
mean: 43.72639 ms, lb 42.32925 ms, ub 45.59146 ms, ci 0.950
std dev: 2.739349 ms, lb 1.923031 ms, ub 3.703952 ms, ci 0.950
variance introduced by outliers: 18.408%
variance is moderately inflated by outliers
```

