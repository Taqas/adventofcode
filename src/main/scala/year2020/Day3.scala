// package year2020

import scala.io.Source

object Day3 {

  def defineGrid(input:String):Array[Array[Boolean]] = {
    input.linesIterator.map { line =>
      line.map(_ == '.').toArray
    }.toArray
  }

  def countTrees(grid:Array[Array[Boolean]], right:Int, down:Int):Int = {
    val l = for (i <- 0 until grid.length by down) yield {
      val loc = (i * right/down) % grid(0).length
      grid(i)(loc) == false
    } 
    l.count(_ == true)
  }

  def prodCountTrees(grid:Array[Array[Boolean]], steps: Array[(Int,Int)]):Long = {
    val countedTrees = for ((r, d) <- steps) yield {
      countTrees(grid, r, d)
    }
    countedTrees.map(_.toLong).product
  }

  def main(args:Array[String]): Unit = {
    val input: String = Source.fromResource("year2020/day3.txt").mkString.trim

    val multiplySlopes = Array(
      (1,1),
      (3,1),
      (5,1),
      (7,1),
      (1,2)
    )

    val grid: Array[Array[Boolean]] = defineGrid(input)
    
    println(countTrees(grid, 3, 1))
    println(prodCountTrees(grid, multiplySlopes))
    
  }
}