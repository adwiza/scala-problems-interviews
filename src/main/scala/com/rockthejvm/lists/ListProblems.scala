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

  // concatenate another list to one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]

  // the big3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]


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

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = RNil

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
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

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = { // head :: (tail.++(anotherList)) //stack recursive
    /*
    Complexity: O(M + N)
    length of this list = N
    length of the other list = Mß
     */
    def conncatTailrec(remainingList: RList[S], acc: RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else conncatTailrec(remainingList.tail, remainingList.head :: acc)
    }

    conncatTailrec(anotherList, this.reverse).reverse
  }

  // remove an element
  override def removeAt(index: Int): RList[T] = {
    // Complexity: O(N)
    @tailrec
    def removeTailrec(remaining: RList[T], currentIndex: Int, predecessors: RList[T]): RList[T] = {
      if (currentIndex == index) predecessors.reverse ++ remaining.tail
      else if (remaining.isEmpty) predecessors.reverse
      else removeTailrec(remaining.tail, currentIndex + 1, remaining.head :: predecessors)
    }

    if (index < 0) this
    else removeTailrec(this, 0, RNil)
  }

  // big 3
  override def map[S](f: T => S): RList[S] = {
    // Complexity: O(N)
    @tailrec
    def mapTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else mapTailrec(remaining.tail, f(remaining.head) :: accumulator)
    }
    mapTailrec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    // Complexity: O(Z^2)
    @tailrec
    def flatMapTailrec(remaining: RList[T], accumulator:RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else flatMapTailrec(remaining.tail, f(remaining.head).reverse ++ accumulator)
    }
    flatMapTailrec(this, RNil)

  }

  override def filter(predicate: T => Boolean): RList[T] = {
    /*
    Complexity: O(N)
     */
    @tailrec
    def filterTailrec(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator.reverse // twist
      else if (predicate(remaining.head)) filterTailrec(remaining.tail, remaining.head :: accumulator)
      else filterTailrec(remaining.tail, accumulator)
    }
    filterTailrec(this, RNil)
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

  // test concat
  println(aSmallList ++ aLargeList)

  // test removeAt
  println(aLargeList.removeAt(13))

  // map
  println(aLargeList.map(x => 2 * x))

  // flatMap
  val time = System.currentTimeMillis()
  aLargeList.flatMap(x => x :: (2 * x) :: RNil)
  println(System.currentTimeMillis() - time)

  // filter
  println(aLargeList.filter(_ % 2 == 0)) // _ -> x => x


}
