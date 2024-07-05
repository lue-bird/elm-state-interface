module Main exposing (main)

import Benchmark.Runner.Alternative
import Benchmarks


main : Benchmark.Runner.Alternative.Program
main =
    Benchmark.Runner.Alternative.program Benchmarks.benchmarks
