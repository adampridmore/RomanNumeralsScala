package RomanNumerals

case class NumeralMap(value: Int, numerals: String)

object NumeralMap {
  implicit def apply(map: (Int, String)): NumeralMap = NumeralMap(map._1, map._2)
}

object RomanNumerals {

  def toRomanNumerals(number: Int): String = {

    val mappings = Seq[NumeralMap](
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

    val mapping = mappings
      .find {
        case NumeralMap(value, _) => number - value >= 0
      }

    mapping match {
      case Some(mapping) => mapping.numerals + toRomanNumerals(number - mapping.value)
      case None => ""
    }
  }
}