This is the code that goes together with the blog post [Locating performance bottlenecks in large Haskell codebases](https://www.tweag.io/posts/2020-01-30-haskell-profiling.html).
All the profiling commands can be found in the Makefile.

One can generate [flamegraphs](https://github.com/brendangregg/FlameGraph) using the [ghc-prof-flamegraph](https://github.com/fpco/ghc-prof-flamegraph) tool:

```
ghc-prof-flamegraph example.prof
```
