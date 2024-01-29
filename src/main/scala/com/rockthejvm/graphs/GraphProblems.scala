package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  /**
   * Easy problems
   */
  // number of nodes this node `node` is associated or (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    if (graph.contains(node)) graph(node).size
    else 0

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.values.count(_.contains(node))

  println(outDegree(socialNetwork, "Alice")) // 3
  println(outDegree(socialNetwork, "David")) // 2

  /**
   * Medium difficulty problems
   */
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    @tailrec
    def isPathTailrec(remaining: List[T], consideredNodes: Set[T]): Boolean = {
      if (remaining.isEmpty) false
      else {
        val node = remaining.head
        if (node == end) true
        else if (consideredNodes.contains(node)) isPathTailrec(remaining.tail, consideredNodes)
        else isPathTailrec(remaining.tail ++ graph(node), consideredNodes + node)
      }
    }

    isPathTailrec(List(start), Set())
  }

  println(isPath(socialNetwork, "Alice", "Mary")) // true
  println(isPath(socialNetwork, "Bob", "Mary")) // false

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    @tailrec
    def findPathTailrec(remaining: List[(T, List[T])], consideredNodes: Set[T]): List[T] = {
      if (remaining.isEmpty) List()
      else {
        val (node, currentPath) = remaining.head
        if (node == end) currentPath.reverse
        else if (consideredNodes.contains(node)) findPathTailrec(remaining.tail, consideredNodes)
        else {
          val neighbors = graph(node)
          val tuples = neighbors.map(n => (n, n :: currentPath))
          findPathTailrec(remaining.tail ++ tuples, consideredNodes + node)
        }
      }
    }
    findPathTailrec(graph(start).map(n => (n, n :: List(start))).toList, Set(start))
  }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)

  println(findPath(socialNetwork, "Charlie", "Mary"))
  println(findPath(socialNetwork, "Alice", "Mary"))
  println(findPath(socialNetwork, "Bob", "Mary"))
  // test cycles
  println(findCycle(socialNetwork, "Alice"))

}
