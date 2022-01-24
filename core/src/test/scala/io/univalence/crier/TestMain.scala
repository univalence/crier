package io.univalence.crier

import org.scalatest._
import flatspec._
import matchers._

class TestMain extends AnyFlatSpec with should.Matchers {
  "myBeautifullFunction" should "should get 27 from 3" in {
    assert(Main.myBeautifullFunction(3) === 27)
  }
}
