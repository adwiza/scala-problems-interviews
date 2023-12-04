package com.rockthejvm.strings

import scala.annotation.tailrec

object StringsProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (remaining.isEmpty) acc
      else if (acc.contains(remaining.head)) {
        val currentChar = remaining.head
        val currentOccurrences = acc(currentChar)
        countCharactersTailrec(remaining.tail, acc + (currentChar -> (currentOccurrences + 1)))
      } else countCharactersTailrec(remaining.tail, acc + (remaining.head -> 1))

    countCharactersTailrec(s, Map())
  }

  def testCountCharacters() = {

    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and functional programming because it's awesome"))
  }

  def checkAnagrams(sa: String, sb: String): Boolean = countCharacters(sa) == countCharacters(sb)
  def checkAnagrams2(sa: String, sb: String): Boolean = sa.sorted ==  sb.sorted

  def testCheckAnagrams() = {
    println(checkAnagrams("desserts", "stressed"))
    println(checkAnagrams("Scala", "Haskell"))
    println (checkAnagrams2("desserts", "stressed"))
    println(checkAnagrams2("Scala", "Haskell"))
  }

  testCheckAnagrams()
}
