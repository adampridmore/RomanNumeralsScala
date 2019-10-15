package RomanNumerals

case class NumeralMap(value: Int, numerals: String) {
  def isLessOrEqual(x: Int): Boolean = value <= x
}

trait NumeralMapConvert {
  implicit def apply(map: (Int, String)): NumeralMap = map match {
    case (value, numeral) => NumeralMap(value, numeral)
  }
}

object RomanNumerals extends NumeralMapConvert {
  def toRomanNumerals(number: Int): String = {
    val numeralMatch = mappings.find(_.isLessOrEqual(number))

    numeralMatch match {
      case Some(numeralMatch) => numeralMatch.numerals + toRomanNumerals(number - numeralMatch.value)
      case None => ""
    }
  }

  private val mappings = Seq[NumeralMap](
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
    1000 -> "M"
  ).reverse
}