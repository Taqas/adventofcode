package year2020

import scala.io.Source

object Day3 {

  type Grid[A] = Vector[Vector[A]]

  def countTrees(grid: Grid[Boolean], right: Int, down: Int): Int = {
    val l = for (i <- 0 until grid.length by down) yield {
      val loc = (i * right / down) % grid(0).length
      grid(i)(loc) == false
    }
    l.count(_ == true)
  }

  def prodCountTrees(grid: Grid[Boolean], steps: Seq[(Int, Int)]): Long = {
    val countedTrees = for ((r, d) <- steps) yield {
      countTrees(grid, r, d)
    }
    countedTrees.map(_.toLong).product
  }

  lazy val input: String = Source.fromResource("year2020/day3.txt").mkString.trim

  def defineGrid(input: String): Grid[Boolean] =
    input.linesIterator
      .map(_.toVector)
      .toVector
      .map(_.map({
        case '#' => true
        case '.' => false
      }))

  val multiplySlopes = Seq(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  )

  def main(args: Array[String]): Unit = {

    val grid: Grid[Boolean] = defineGrid(input)

    println(countTrees(grid, 3, 1))
    println(prodCountTrees(grid, multiplySlopes))

  }
}
