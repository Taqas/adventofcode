package year2020

import year2020.Day2.StoreTypes.StoreType
import scala.io.Source

object Day2 {

  trait Policy

  case class PolicySledRental(
      minOfRange: Int,
      maxOfRange: Int,
      char: Char
  ) extends Policy

  case class PolicyBogoddan(
      firstPos: Int,
      secondPos: Int,
      char: Char
  ) extends Policy

  type Password = String

  sealed trait Part {
    def isValid(policy: Policy, password: Password): Boolean

    def countValid(passwordPolicies: Seq[(Policy, Password)]): Int =
      passwordPolicies.count((isValid _).tupled)
  }

  object Part1 extends Part {
    override def isValid(policy: Policy, password: Password): Boolean = {
      val PolicySledRental(min, max, char) = policy
      val count = password.count(_ == char)
      min <= count && count <= max
    }
  }

  object Part2 extends Part {
    override def isValid(policy: Policy, password: Password): Boolean = {
      val PolicyBogoddan(firstPos, secondPos, char) = policy
      (password(firstPos - 1) == char) ^ (password(secondPos - 1) == char)
    }
  }

  object StoreTypes extends Enumeration {
    type StoreType = Value
    val SledRental, Bogoddan = Value
  }

  private val passwordPolicyRegex = """(\d+)-(\d+) (\w): (\w+)""".r

  def parsePasswordPolicy(s: String, storeType: StoreType): (Policy, Password) =
    (s, storeType) match {
      case (passwordPolicyRegex(min, max, char, password), StoreTypes.SledRental) =>
        (PolicySledRental(min.toInt, max.toInt, char.head), password)
      case (passwordPolicyRegex(firstPos, secondPos, char, password), StoreTypes.Bogoddan) =>
        (PolicyBogoddan(firstPos.toInt, secondPos.toInt, char.head), password)
    }

  def parsePasswordPolicies(input: String, storeType: StoreType): Seq[(Policy, Password)] =
    input.linesIterator.map(parsePasswordPolicy(_, storeType)).toSeq

  lazy val input: String = Source.fromResource("year2020/day2.txt").mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countValid(parsePasswordPolicies(input, StoreTypes.SledRental)))
    println(Part2.countValid(parsePasswordPolicies(input, StoreTypes.Bogoddan)))
  }
}
