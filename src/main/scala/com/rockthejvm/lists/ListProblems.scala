package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
   * Standard functions
   */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
   * Easy problems
   */
   // get element at given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

} // Our list

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  /**
   * Easy problems
   */
  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
  // the size of the list
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = RNil
 }
//  override def headOption: Option[Nothing] = None

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
    override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }
  /**
   * Easy problems
   */
  //  override def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  // get element at a given index
  override def apply(index: Int): T = {
    /*
    [1, 2, 3, 4, 5].apply(2) = applyTailrec([1, 2, 3, 4, 5], 0)
    = applyTailrec([2, 3, 4, 5], 1)
    = applyTailrec([3, 4, 5], 2
    = 3

    Complexity of this algorithm?
    O(min(N, index))
     */
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }
    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  // the size of the list
  override def length: Int = {
    /*
    [1, 2, 3, 4, 5].length = lengthTailrec([1,2, 3, 4, 5], 0
    = lengthTailrec([2, 3, 4, 5], 1)
    = lengthTailrec([3, 4, 5], 2)
    = lengthTailrec([4, 5], 3)
    = lengthTailrec([5], 4)
    = lengthTailrec([], 5)
    = 5

    Complexity: O(N)
     */
    @tailrec
    def lengthTailrec(remainingList: RList[T], accumulator: Int): Int = {
      if (remainingList.isEmpty) accumulator
      else lengthTailrec(remainingList.tail, accumulator + 1)

    }
    lengthTailrec(this, 0)
  }//1 + tail.length

  // reverse this list into a new list
  override def reverse: RList[T] = {
    // Complexity: O(N)
    @tailrec
    def reverseTailRec(remainingList: RList[T], result: RList[T]): RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailRec(remainingList.tail, remainingList.head :: result)
     }

      reverseTailRec(this, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def convertToRlistTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRlistTailrec(remaining.tail, remaining.head :: acc)
    }
    convertToRlistTailrec(iterable, RNil).reverse
  }
}


object ListProblems extends App {

  RNil.::(2) == 2 :: RNil
  val aSmallList = 1 :: 2 :: 3 :: RNil // Rnil.::(3).::(2).::(1)  //::(1, ::(2, ::(3, RNil)))
  val aLargeList = RList.from(1 to 10000)
  println(aSmallList)

  // test get-kth
  println(aSmallList.apply(0))
  println(aSmallList.apply(2))
  println(aLargeList.apply(8735))

  // test length
  println(aSmallList.length)
  println(aLargeList.length)

  // test reverse
  println(aSmallList.reverse)
  println(aLargeList.reverse)


}
