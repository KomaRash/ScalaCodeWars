object CamelCase {

  def toCamelCase(str: String)=
    str.split(Array('_', '-')).toList.head + str.split(Array('_', '-')).tail.toList.map(_.capitalize).mkString("")
}


/*
Solution
Define the regular-expression patterns you want to extract from your String, placing parentheses around them so you can extract them as “regular-expression groups.” First, define the desired pattern:

val pattern = "([0-9]+) ([A-Za-z]+)".r
Next, extract the regex groups from the target string:

val pattern(count, fruit) = "100 Bananas"
This code extracts the numeric field and the alphabetic field from the given string as two separate variables, count and fruit, as shown in the Scala REPL:

scala> val pattern = "([0-9]+) ([A-Za-z]+)".r
pattern: scala.util.matching.Regex = ([0-9]+) ([A-Za-z]+)

scala> val pattern(count, fruit) = "100 Bananas"
count: String = 100
fruit: String = Bananas
object CamelCase {
  val toCamelCase = "[_-](.)".r.replaceAllIn(_: String, _.group(1).toUpperCase)
}
 */


object Parity {

  def findOutlier(integers: List[Int]): Int = {
    val a = integers.partition(_ % 2 == 0)
    if (a._2.length == 1) a._2.head
    else a._1.head
  }
}

/*
object Parity {

  def findOutlier(integers: List[Int]): Int =
    integers.partition(_%2 == 0) match {
      case (List(outlier), _) => outlier
      case (_, List(outlier)) => outlier
    }
}
 */