package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumbersOps {
  implicit class RRichInt(n: Int) { // our rich int
    /**
     * Easy problems
     */

    def isPrime: Boolean = {
      @tailrec
      def isPrimeTailrec(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n))) true
        else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
      }

      if (n == 0 || n == 1 || n == -1) false
      else isPrimeTailrec(2)
    }

    def decompose: List[Int] = {
      assert(n > 0)

      @tailrec
      def decomposeTailrec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
        if (currentDivisor > Math.sqrt(remaining)) remaining :: accumulator
        else if (remaining % currentDivisor == 0) decomposeTailrec(remaining / currentDivisor, currentDivisor, currentDivisor :: accumulator)
        else decomposeTailrec(remaining, currentDivisor + 1, accumulator)
      }

      decomposeTailrec(n, 2, List())
    }
  }

}

object NumberProblems extends App {

import NumbersOps._
  // implicit class RRichInr is available here

  def testIsPrime() = {

    println((1.isPrime)) // newRRichInr(1).isPrime
    println((2.isPrime))
    println((15.isPrime))
    println((2731189.isPrime))
    println((517935871.isPrime))
    println((2003.isPrime))
    println((0.isPrime))
    println(((-2003).isPrime))
  }

  def testDecompose() = {
    println((1.decompose))
    println((2.decompose))
    println((15.decompose))
    println((2731189.decompose))
    println((53611.isPrime))
    println((9661.isPrime))
    println((517935871.decompose))
    println((2003.decompose))
    println((0.decompose))
    println(((-2003).isPrime))
  }

  testDecompose()
  testIsPrime()
}
