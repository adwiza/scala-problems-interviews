package com.rockthejvm.strings

import scala.:+
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

  def justify(text: String, width: Int): String = {
    def createSpaces(n: Int): String = (1 to n).map(_ => " ").mkString("") // creates n spaces

    @tailrec
    def pack(words: List[String], currentRow: List[String], currentCharCount: Int, result: List[List[String]]): List[List[String]] = {
      if (words.isEmpty && currentRow.isEmpty) {
        result
      } else if (words.isEmpty) {
        result :+ currentRow
      } else if (currentRow.isEmpty && words.head.length > width) {
        // split the word into supercalifra-gilistic
        val (partOnThisRow, partOnNextRow) = words.head.splitAt(width - 2) // at width -1 put a '-'
        pack(partOnNextRow :: words.tail, List(), 0, result :+ List(partOnThisRow + "-"))
      } else if (words.head.length + currentCharCount > width) {
        pack(words, List(), 0, result :+ currentRow)
      } else  {
        pack(words.tail, currentRow :+ words.head, currentCharCount + 1 + words.head.length, result)
      }
    }

    def justifyRow(row: List[String]): String = {
      if (row.length == 1) row.head
      else {
        val nSpacesAvailable = width - row.map(_.length).sum
        val nIntervals = row.length - 1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraspaces = nSpacesAvailable % nIntervals
        val regularSpace = createSpaces(nSpacesPerInterval)
        val biggerSpace = createSpaces(nSpacesPerInterval + 1)

        if (nExtraspaces == 0) row.mkString((regularSpace))
        else {
          val nWordsWithBiggerIntervals = nExtraspaces + 1
          val wordsWithBiggerIntervals = row.take(nWordsWithBiggerIntervals)
          val firstPart = wordsWithBiggerIntervals.mkString(biggerSpace)
          val secondPart = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + regularSpace + secondPart
        }
      }
    }

    assert(width > 2)
    // split text into words
    val words = text.split(" ").toList
    // pack the words into rows
    val unjustifiedRows = pack(words, List(), 0, List())
    // justify the rows
    val justifiedRows = unjustifiedRows.init.map(justifyRow) :+ unjustifiedRows.last.mkString(" ")
    justifiedRows.mkString("\n")

  }

  println(justify("Scala is the most amazing language you will ever write any code in", 6))
  println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec id eros a massa tristique tincidunt. Fusce maximus est felis, et dictum lorem faucibus et. Etiam quis libero a urna efficitur fermentum ac quis tellus. Aliquam nec elit ultricies, laoreet nulla eget, hendrerit sapien. Vestibulum vehicula sem mollis pharetra iaculis. Nam vitae purus est. Fusce eu tempor urna, id hendrerit lorem. Etiam vestibulum luctus commodo.\n\nFusce nec ligula dapibus, pellentesque lorem quis, congue est. Suspendisse tempor massa vel ante semper, ac maximus ante vulputate. Sed commodo mauris quis dolor iaculis, ac feugiat enim suscipit. Nunc eget ullamcorper turpis. Phasellus a ullamcorper.", 80))
  println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut ornare lacus at nulla varius, eu mattis metus ullamcorper. Vestibulum semper nulla a dignissim accumsan. In hac habitasse platea dictumst. Quisque rutrum, justo vel interdum faucibus, urna lectus semper elit, id maximus quam leo id ex. Ut sed sagittis elit. Aenean ut vulputate magna. Etiam nisl nibh, pharetra vitae nisi eu, vehicula feugiat mi. Phasellus in porta elit. Maecenas vitae tristique diam, in condimentum justo. Donec porta eget mi nec porta.\n\nIn et metus arcu. Suspendisse lobortis dictum ipsum, a ultrices mauris tincidunt eu. Sed sagittis tortor a libero ultricies, eu consequat est fermentum. Nunc et nulla bibendum, commodo eros at, vestibulum diam. Nullam id tortor blandit, tempor ex in, scelerisque est. Sed dictum varius quam, et tincidunt lectus viverra eget. Aenean elit leo, vulputate consequat porttitor euismod, tincidunt a sem.\n\nProin dignissim ante vel leo porttitor luctus. Aliquam quis neque nulla. Nulla accumsan hendrerit enim non finibus. In sollicitudin, neque non facilisis condimentum, lacus ex pulvinar urna, in dictum nibh eros ut ligula. Etiam sagittis quam a purus dapibus rutrum. Cras eu risus placerat, tempor dolor eget, bibendum nulla. Aliquam erat volutpat. Ut mollis fringilla sapien non porttitor. Proin ultrices.", 100))
}
