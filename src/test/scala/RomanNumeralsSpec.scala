import org.scalatest.{FunSpec, Matchers, WordSpec}

class RomanNumeralsSpec extends FunSpec with Matchers {
  def roman(number: Int): String = {

    case class NumeralMap(value: Int, numerals: String)

    val mappings = Seq(
      NumeralMap(1, "I"),
      NumeralMap(4, "IV"),
      NumeralMap(5, "V"),
      NumeralMap(9, "IX"),
      NumeralMap(10, "X")
    ).reverse

    val mapping = mappings
      .find {
        case NumeralMap(value, _) => number - value >= 0
      }

    mapping match {
      case Some(mapping) => mapping.numerals + roman(number - mapping.value)
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