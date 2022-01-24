package io.univalence.crier

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}
import org.openjdk.jmh.infra.Blackhole

class MainBenchmark {
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(value = 2)
  @Warmup(iterations = 2)
  @Measurement(iterations = 2)
  def myBeautifullFunction(blackhole: Blackhole): Unit = blackhole.consume(Main.myBeautifullFunction(2))
}
