// package year2020

import scala.io.Source

object Day2 {
  case class PasswordPolicy (
    minOfRange: Int,
    maxOfRange: Int,
    char: Char
  )

  case class PasswordPolicyBogoddan (
    firstPos: Int,
    secondPos: Int,
    char: Char
  )

  type Password = String

  object ValidPasswordsSledRental {
    def numberInRange(min:Int, max:Int, number:Int):Boolean = {
      min <= number && number <= max
    }

    def countValid(passwords: List[(PasswordPolicy, Password)]): Int = {
      passwords.map { case (policy:PasswordPolicy, pass:Password) =>
        val l = pass.filter(_ == policy.char).length
        numberInRange(policy.minOfRange, policy.maxOfRange, l)
      }.count(_ == true)
    }

    def parsePasswords(input: String): List[(PasswordPolicy, Password)] = {
      input.linesIterator.map { line => 
        val l = line.split(" ")
        val range = l.head.split("-")
        val min = range(0).toInt
        val max = range(1).toInt
        val char = l(1).dropRight(1).toList.head.toChar
        val pass = l(2)
        (PasswordPolicy(min, max, char), pass)
      }.toList
    }
  }

  object ValidPasswordsToboggan {
    def countValid(passwords: List[(PasswordPolicyBogoddan, Password)]): Int = {
      passwords.map { case (pol, pass) =>
        (pass.charAt(pol.firstPos).toString + pass.charAt(pol.secondPos)).count(_ == pol.char) == 1
      }.count(_ == true)
    }

    def parsePasswords(input: String): List[(PasswordPolicyBogoddan, Password)] = {
      input.linesIterator.map { line => 
        val l = line.split(" ")
        val positions = l.head.split("-")
        val firstPos = positions(0).toInt - 1
        val secondPos = positions(1).toInt - 1
        val char = l(1).dropRight(1).toList.head.toChar
        val pass = l(2)
        (PasswordPolicyBogoddan(firstPos, secondPos, char), pass)
      }.toList
    }
  }

  def main(args:Array[String]): Unit = {
    val input: String = Source.fromResource("year2020/day2.txt").mkString.trim
    
    println(ValidPasswordsSledRental.countValid(ValidPasswordsSledRental.parsePasswords(input)))
    println(ValidPasswordsToboggan.countValid(ValidPasswordsToboggan.parsePasswords(input)))
  }
}