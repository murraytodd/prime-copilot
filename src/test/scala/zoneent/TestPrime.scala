package zoneent

import org.scalatest.funsuite.AnyFunSuite

class TestPrime extends AnyFunSuite {
  
  test("getPrimes(0) should return empty list") {
    assert(Prime.getPrimes(0) == List.empty)
  }

  test("getPrimes(1) should return List(2)") {
    assert(Prime.getPrimes(1) == List(2))
  }

  test("getPrimes should return values in ascending order") {
    val primes = Prime.getPrimes(10)
    assert(primes == primes.sorted)
  }

  test("testCandidates should return false for 4") {
    assert(! Prime.testCandidates(4, List(3, 2))) // note the candidates are in descending order
  }

  test("testCandidates should return true for 5") {
    assert(Prime.testCandidates(5, List(3, 2)))
  }
}
