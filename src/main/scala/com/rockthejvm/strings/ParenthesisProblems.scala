package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  def hasValidParentheses(string: String): Boolean = {

    @tailrec
    def validParensTailrec(remaining: String, openParens: Int): Boolean = {
      if (remaining.isEmpty) openParens == 0
      else if (openParens == 0 && remaining.head == ')') false
      else if (remaining.head == '(') validParensTailrec(remaining.tail, openParens + 1)
      else validParensTailrec(remaining.tail, openParens - 1)
    }

    validParensTailrec(string, 0 )
  }

  def testValidParentheses() = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("())"))
    println(hasValidParentheses(")()"))
    println(hasValidParentheses("(()()(()))(((())()))"))
  }

  def generateAllValidParentheses(n: Int): List[String] = {

    @tailrec
    def genParensTailrec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if (nRemainingParens == 0) currentStrings
      else {
        val newStrings = for {
          string <- currentStrings
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }

        genParensTailrec(nRemainingParens - 1, newStrings)
      }
    }

    assert(n >= 0)

    if (n == 0) List()
    else genParensTailrec(n-1, Set("()")).toList
  }

  def testGenParens() = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }

  testGenParens()

}
