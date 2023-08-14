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
  alice drives lamborghini // <-- infix notation


}
