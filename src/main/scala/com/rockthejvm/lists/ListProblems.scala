package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

} // Our list

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

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

  //  override def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
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
}


object ListProblems extends App {

  RNil.::(2) == 2 :: RNil
  val aSmallList = 1 :: 2 :: 3 :: RNil // Rnil.::(3).::(2).::(1)  //::(1, ::(2, ::(3, RNil)))
  println(aSmallList)

  println(aSmallList.apply(0))
  println(aSmallList.apply(2))
  println(aSmallList.apply(90))

}
