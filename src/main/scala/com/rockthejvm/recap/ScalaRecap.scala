package com.rockthejvm.recap

import scala.annotation.tailrec

object ScalaRecap extends App {

  /**
   * Basics
   */

  val aCondition = if (true) "happy" else "sad"

  // tail-recursive function: VERY IMPORTANT for this course
  @tailrec
  def factorialTailrec(n: Int, accumulator: Int): Int = {
    if (n <= 1) accumulator
    else
      factorialTailrec(n -1, n * accumulator)
  }

  /**
   * Object-oriented programming
   */
  class Car
  class Supercar extends Car
  // subtype polymorphism
  val c: Car = new Supercar

  // traits are types with abstract (not implemented) methods
  trait SelfDriving {
    def drive: Unit
  }

  // a class can extend one class and mix-in as many traits as you like
  class CarOfTheFuture extends Car with SelfDriving {
    override def drive: Unit = println("I'm driving by myself! Woohoo!")
  }

  // case classes are lightweight data structures with boilerplate e.g. equals/hashCode already implemented
  case class Person(name: String, age: Int) {
    def drives(car: Car) = println(s"$name is driving $car") // <-- s-interpolated string
  }

  // method notation
  val alice = Person("Alice", 23)
  val lamborghini = new Supercar
  alice drives lamborghini // <-- infix notation alice.drives(lamborghini)

  // generics
  class MySet[+T]

  // singletons & companions
  object MySet

  /**
   * Functional programming
   */

    // functions are instances of the FunctionX trait
  val incrementer = new Function1[Int, Int] {
      override def apply(x: Int): Int = x + 1
    }
  val two = incrementer(1) // same as incrementer.apply(1)

  // syntax sugars for functions
  val anotherIncrementer = (x: Int) => x + 1 // same as new Function1[Int, Int] {...}

  // higher-order functions
  val incrementedList = List(1, 2, 3).map(incrementer) // function is passed as argument to the map method
  //                                  ^^^ this is a NEW list

  /**
   * Collections
   */
  // list
  val aList = List(1, 2, 3, 4)

  // sequences = abstract representation of elements in a given order
  val aSeq = Seq(1, 2, 3, 4) // Seq is a trait, so the Seq companion's apply() actually builds a list

  // arrays
  val anArray = Array.ofDim[Int](2, 3) // <- bi-directional in this call

  // sets: every element appears once
  val aSet = Set(1, 1, 2, 3)

  // vectors: efficient Seq implementation
  val aVector = Vector(1, 2, 3, 4)

  // tuples
  val aTuple: (Int, String) = (1, "I love tuples! Yau. Please bring more tuples.")

  // maps
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  /**
   * Pattern matching
   */
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  println(order)
  println(greeting)

  // all patterns

  /**
   * For the hardcore peeps: implicits.
   * Implicits are a beast. We are only going to use implicits for the below use cases.
   * Implicits are hard and OPTIONAL in this course, just for the sake of Scala's expressiveness.
   *
   * The advanced Scala course explains everything about implicits.
   */

  // use case 1: implicit arguments
  implicit val descendingIntOrdering: Ordering[Int] = Ordering.fromLessThan((a, b) => b > a) // comparison object
  val theObviousTruth = descendingIntOrdering.lteq(99, 100)
  //                                          ^^^^ other methods: lt, gt, qteq, etc.

  //implicit orderings are used for sorting methods
  val descendingList = List(1, 2, 3, 4).sorted
  //                                          ^ here the compiler infects the implicit ordering I defined above, as argument

  // use case 2: implicit conversion
  implicit class MyRichInt(number: Int) {
    def isPositive = number > 0
  }

  val oneSign = 1.isPositive
  // equivalent with new MyRichInt(1).isPositive
  // the auto-conversion is done at compile time
  println(oneSign)








}
