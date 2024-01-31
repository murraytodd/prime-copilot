package zoneent

/**
  * A simple prime number generator based on the trial division approach
  * 
  * This object contains a lot of optimizations such as prepending 
  * the candidate list that may arguably fall under the category of 
  * "premature optimizations". In other words, it's not a great 
  * pedagogical approach to demonstrating how to write a prime number 
  * generator using functional programming techniques.
  */
object Prime:

  /**
    * Test whether a candidate number is prime, given a list of primes
    * 
    * This function not only tests that the candidate is not divisible 
    * by any of the primes in the list, but it optimizes performance by 
    * skipping the preexistng primes that are greater than the 
    * candidate's square root.
    *
    * @param n the candidate number
    * @param candidates the list of primes that have already been found
    * @return true if the candidate is prime, false otherwise
    */
  def testCandidates(n: Int, candidates: List[Int]): Boolean =
    if (n % 2 == 0) then false
    else
      val cutoff = Math.sqrt(n).toInt
      candidates
        .dropWhile(_ > cutoff)
        .forall(n % _ != 0)

  /**
   * Get a list of the first n primes
   * 
   * @param n the number of primes to return
   * @param primes the list of primes that have already been found
   * @param next the next candidate number to test
   * @return a list of the first n primes
   */
  def getPrimes(n: Int, primes: List[Int] = List(2), next: Int = 3): List[Int] =
    if (primes.length == n) then primes.reverse // primes are in descending order
    else if testCandidates(next, primes) then getPrimes(n, next :: primes, next + 1) 
    else getPrimes(n, primes, next + 1)

  @main def main(): Unit = 
    println(getPrimes(1000).mkString(", "))
    