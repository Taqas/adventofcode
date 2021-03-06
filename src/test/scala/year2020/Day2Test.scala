package year2020

import Day2._
import org.scalatest.flatspec.AnyFlatSpec

class Day2Test extends AnyFlatSpec {

  val exampleInput: String =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  behavior of "Example input"

  it should "pass tests for part 1" in {
    assert(Part1.isValid(PolicySledRental(1, 3, 'a'), "abcde"))
    assert(!Part1.isValid(PolicySledRental(1, 3, 'b'), "cdefg"))
    assert(Part1.isValid(PolicySledRental(2, 9, 'c'), "ccccccccc"))

    assert(Part1.countValid(parsePasswordPolicies(exampleInput, StoreTypes.SledRental)) == 2)
  }

  it should "pass tests for part 2" in {
    assert(Part2.isValid(PolicyBogoddan(1, 3, 'a'), "abcde"))
    assert(!Part2.isValid(PolicyBogoddan(1, 3, 'b'), "cdefg"))
    assert(!Part2.isValid(PolicyBogoddan(2, 9, 'c'), "ccccccccc"))

    assert(Part2.countValid(parsePasswordPolicies(exampleInput, StoreTypes.Bogoddan)) == 1)
  }
}
