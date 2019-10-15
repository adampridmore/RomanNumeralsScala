package RomanNumerals

import org.scalatest.{FunSpec, Matchers}
import RomanNumerals._

class RomanNumeralsSpec extends FunSpec with Matchers {
  val tests: Seq[(Int, String)] = Seq(
    0 -> "",
    1 -> "I",
    2 -> "II",
    3 -> "III",
    4 -> "IV",
    5 -> "V",
    6 -> "VI",
    7 -> "VII",
    8 -> "VIII",
    9 -> "IX",
    10 -> "X",
    50 -> "L",
    90 -> "XC",
    100 -> "C",
    500 -> "D",
    900 -> "CM",
    1000 -> "M",
    1999 -> "MCMXCIX"
  )

  describe("When parsing a number to a roman numeral then") {
    tests.foreach {
      case (number, numerals) => {
        it(s"$number should be parsed to $numerals") {
          toRomanNumerals(number) shouldBe numerals
        }
      }
    }
  }
}