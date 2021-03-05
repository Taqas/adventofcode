package year2020

import Day1._
import org.scalatest.flatspec.AnyFlatSpec

class Day1Test extends AnyFlatSpec {

  val exampleInput =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin

  behavior of "Example input"

  it should "return 514579 for part 1" in {
    assert(entryProduct(parseEntries(exampleInput), 2) === 514579)
  }

  it should "return 241861950 for part 2" in {
    assert(entryProduct(parseEntries(exampleInput), 3) === 241861950)
  }
}
