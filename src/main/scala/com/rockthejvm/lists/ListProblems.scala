package com.rockthejvm.lists

import com.rockthejvm.lists

import scala.Console.println
import scala.annotation.tailrec
import scala.util.Random

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

  /**
   * Medium difficulty problems
   */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotation  by a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  /**
   * Hard problems
   */

  // sorting the list in the order defined by Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
  def quickSort[S >: T](ordering: Ordering[S]): RList[S]


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

  /**
   * Medium difficulty problems
   */
  override def rle: RList[(Nothing, Int)] = RNil

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil

  // random samples
  override def sample(k: Int): RList[Nothing] = RNil

  /**
   * Hard problems
   */
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil



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

    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if (currentList.isEmpty && elements.isEmpty) accumulator
      else if (currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    betterFlatMap(this, RNil)

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

  /**
   * Medium difficulty problems
   */
  override def rle: RList[(T, Int)] =  {
    // Complexity: O(N)
    @tailrec
    def rleTailrec(remaining: RList[T], currentTuple: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty && currentTuple._2 == 0) accumulator
      else if (remaining.isEmpty) currentTuple :: accumulator
      else if (remaining.head == currentTuple._1) rleTailrec(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else rleTailrec(remaining.tail, (remaining.head, 1), currentTuple :: accumulator)
    }
    rleTailrec(this.tail, (this.head, 1), RNil).reverse
  }

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[T] = {
    // Complexity: O(N * K)
    @tailrec
    def duplicateTailrec(remaining: RList[T], currentElement: T, nDuplicates: Int, accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty && nDuplicates == k) accumulator.reverse
      else if (remaining.isEmpty) duplicateTailrec(remaining, currentElement, nDuplicates + 1, currentElement :: accumulator)
      else if (nDuplicates == k) duplicateTailrec(remaining.tail, remaining.head, 0, accumulator)
      else duplicateTailrec(remaining, currentElement, nDuplicates + 1, currentElement :: accumulator)
    }
    duplicateTailrec(this.tail, this.head, 0, RNil )
  }

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[T] = {
    @tailrec
    def rotateTailrec(remaining: RList[T], rotationsLeft: Int, buffer: RList[T]): RList[T] = {
      if (remaining.isEmpty && rotationsLeft == 0) this
      else if (remaining.isEmpty) rotateTailrec(this, rotationsLeft, RNil)
      else if (rotationsLeft == 0) remaining ++ buffer.reverse
      else rotateTailrec(remaining.tail, rotationsLeft - 1, remaining.head :: buffer)
    }

    rotateTailrec(this, k, RNil)

  }

  // random samples
  override def sample(k: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val maxIndex = this.length
    @tailrec
    def sampleTailrec(nRemaining: Int, accumulator: RList[T]): RList[T] = {
      if (nRemaining == 0) accumulator
      else {
        val index = random.nextInt(maxIndex)
        val newNumber = this(index)
        sampleTailrec(nRemaining - 1, newNumber :: accumulator)
      }
    }

    def sampleElegant: RList[T] =
      RList.from(1 to k).map(_ => random.nextInt(maxIndex)).map(index => this(index))
    if (k < 0) RNil
    else sampleElegant
  }

  /**
   * Hard problems
   */
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    @tailrec
    def insertSortTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, acc))
    }
    insertSortTailrec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def merge(listA: RList[S], listB: RList[S], accumulator: RList[S]): RList[S] = {
      if (listA.isEmpty) accumulator.reverse ++ listB
      else if (listB.isEmpty) accumulator.reverse ++ listA
      else if (ordering.lteq(listA.head, listB.head)) merge(listA.tail, listB, listA.head :: accumulator)
      else merge(listA, listB.tail, listB.head :: accumulator)

    }

    @tailrec
    def mergeSortTailrec(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = {
      if (smallLists.isEmpty) {
        if (bigLists.isEmpty) RNil
        else if (bigLists.tail.isEmpty) bigLists.head
        else mergeSortTailrec(bigLists, RNil)
      } else if (smallLists.tail.isEmpty) {
        if (bigLists.isEmpty) smallLists.head
        else mergeSortTailrec(smallLists.head :: bigLists, RNil)
      }
      else {
        val first = smallLists.head
        val second = smallLists.tail.head
        val merged = merge(first, second, RNil)
        mergeSortTailrec(smallLists.tail.tail, merged :: bigLists)
      }
    }
    mergeSortTailrec(this.map(x => x :: RNil), RNil)
  }

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def partition(list: RList[T], pivot: T, smaller: RList[T], larger: RList[T]): (RList[T], RList[T]) = {
      if (list.isEmpty) (smaller, larger)
      else if (ordering.lteq(list.head, pivot)) partition(list.tail, pivot, list.head :: smaller, larger)
      else partition(list.tail, pivot, smaller, list.head :: larger)
    }

    @tailrec
    def quickSortTailrec(remainingLists: RList[RList[T]], accumulator: RList[RList[T]]): RList[T] = {
      if (remainingLists.isEmpty) accumulator.flatMap(smallList => smallList).reverse
      else if (remainingLists.head.isEmpty) quickSortTailrec(remainingLists.tail, accumulator)
      else if (remainingLists.head.tail.isEmpty) quickSortTailrec(remainingLists.tail, remainingLists.head :: accumulator)
      else {
        val list = remainingLists.head
        val pivot = list.head
        val listToSplit = list.tail
        val (smaller, larger) = partition(listToSplit, pivot, RNil, RNil)
        quickSortTailrec(smaller :: (pivot :: RNil) :: larger :: remainingLists.tail, accumulator)
      }
    }
    quickSortTailrec(this :: RNil, RNil)
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
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions() = {

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

  /**
   * Medium difficulty functions
   */
  def testMediumDifficultyFunctions() = {
    // run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)
    // duplicateEach
    println(aSmallList.duplicateEach(4))
    // rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

    // random samples
    println(aLargeList.sample(10))

    // better flatMap
    println(aSmallList.flatMap(x => x :: (2 * x) :: RNil))
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil)
    println(System.currentTimeMillis() - time)

  }

  def testHardFunctions() = {
    val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
    val ordering = Ordering.fromLessThan[Int](_ < _ )
    val listToSort = aLargeList.sample(10)

    // insertion sort
    println(anUnorderedList.insertionSort(ordering))
    println(listToSort.insertionSort(ordering))
    // merge sort
    println(listToSort.mergeSort(ordering))
    // quick sort
    println(listToSort.quickSort(ordering))

  }

  testHardFunctions()

}
