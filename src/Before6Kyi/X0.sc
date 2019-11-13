def digPow(n: Int, p: Int): Int =
  n.toString.zipWithIndex.map { case (d, i) => math.pow(d.asDigit, i+p) }.sum / n match {
    case r if r.isWhole => r.toInt
    case _              => -1
  }
object ExesAndOhs {

  def xo(str: String): Boolean = {
    for (x <- str.toLowerCase.toCharArray) yield {
      x match {
        case 'x' => -1
        case 'o' => 1
        case _ => 0
      }
    }
  }.sum.equals(0)
}
/*
object ExesAndOhs {

  def xo(str: String): Boolean =
    str.count(_.toLower == 'x') == str.count(_.toLower == 'o')
}
 */
ExesAndOhs.xo("xoxoxoxo1234")
"is2 Thi1s T4est 3a"
"Thi1s is2 3a T4est"
object Text {
  def order(str: String): String = str.split(" ").sortBy(_.find(_.isDigit)).mkString(" ")
}
Text.order("is2 Thi1s T4est 3a")
