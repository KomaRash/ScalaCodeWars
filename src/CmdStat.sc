
object Stat {
  def time(listA: Array[Int])=(listA(0)*3600+listA(1)*60+listA(2))
  def sort(listA: Array[Int],listB: Array[Int])=time(listA)> time (listB)
  def timeToArray(time:Int,r: Int,array: Array[Int]):Array[Int]=r match {
    case 0 =>array.reverse
    case _ =>timeToArray(time%r,r/60,Array(  time/r ) ++ array)
  }
  def medians(s: Array[Int])  =
  {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
  }
  def stat(strg: String)= {
  val result=strg.split(", ").map(_.split('|').map((_.toInt))).
    sortWith(sort)
    val average=timeToArray(result.map(time(_)).sum/result.length,3600,Array())// your code
    val range=timeToArray(result.map(time).max-result.map(time).min,3600,Array())
    val median = timeToArray(medians(result.map(time)),3600,Array())
      val (lower, upper) = result.splitAt(result.size / 2)

    "Range: "+range.map("%02d".format(_)).mkString("|")+
      " Average: "+average.map("%02d".format(_)).mkString("|")+
        " Median: "+median.map("%02d".format(_)).mkString("|")

  }
}
/*
object Stat {
  def stat(strg: String): String = {
    val millis = strg.split(", ").map(_.split("\\|")).map {
      case Array(h, m, s) => (h.toInt * 60 + m.toInt) * 60 + s.toInt
    }.sorted.toList
    val median: (Int, Int) = millis.zip(millis.reverse).toArray.apply((millis.length - 1) / 2)
    val out = Array(millis.max - millis.min, millis.sum / millis.length, (median._1 + median._2) / 2)
      .map(millis => "%02d|%02d|%02d".format(millis / 3600, millis / 60 % 60, millis % 60))
    // Range: 00|31|17 Average: 02|26|18 Median: 02|22|00"
    s"Range: ${out(0)} Average: ${out(1)} Median: ${out(2)}"
  }
}
 */
Stat.stat  ("01|15|59, 1|47|16, 01|17|20, 1|32|34, 2|17|17" )
