import org.scalatest.{FunSpec, Matchers, WordSpec}

class RomanNumeralsSpec extends FunSpec with Matchers {
  def roman(number: Int): String = {

    val mappings = Seq(
      1 -> "I",
      4 -> "IV",
      5 -> "V",
      9 -> "IX",
      10 -> "X"
    ).reverse

    val mapping = mappings
      .find {
        case (romanValue, _) => number - romanValue >= 0
      }

    mapping match {
      case Some(mapping) => mapping._2 + roman(number - mapping._1)
      case None => ""
    }
  }

  val data: Seq[(Int, String)] = Seq(
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
    10 -> "X"
  )

  describe("When parsing a number to a roman numeral then") {
    data.foreach {
      case (number, numerals) => {
        it(s"$number should be parsed to $numerals") {
          roman(number) shouldBe numerals
        }
      }
    }
  }
}