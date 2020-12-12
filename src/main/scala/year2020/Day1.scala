// package year2020

import scala.io.Source

object Day1 {
  
  def entryProduct(entries: Seq[Int], n: Int): Int = {
    entries.combinations(n).map(x => (x, x.sum)).filter(_._2 == 2020).map(_._1.product).toList.head
  }
  
  def parseEntries(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  def main(args:Array[String]):Unit = {
    val input: String = Source.fromResource("year2020/day1.txt").mkString.trim
    
    println(entryProduct(parseEntries(input), 2))
    println(entryProduct(parseEntries(input), 3))

  }
}