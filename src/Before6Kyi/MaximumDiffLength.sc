object Shortest {

  def findShort(str: String): Int = str.split(" ").map(_.length).min
}
object FindTheOddInt {
  def findOdd(xs: Seq[Int])= {
    xs.sortWith((_>_)).groupBy(_.intValue()).toList.filter(_._2.length%2==1).head._1
  }
}
FindTheOddInt.findOdd(List(1,1,2,-2,5,2,4,4,-1,-2,5))
/*
object FindTheOddInt {

  def findOdd(xs: Seq[Int]): Int =
    xs.find(x => xs.count(_ == x) % 2 == 1).get
}
 */
List(1,1,2,-2,5,2,4,4,-1,-2,5).reduce(_ ^ _)
object Scoring {
  def high(s: String): String =s.split(" ").map(_.toCharArray)
    .sortWith(_.map(_.toInt-'a'+1).sum > _.map(_.toInt-'a'+1).sum).head.mkString("")
}
/*
object Scoring {

  def high(s: String): String = s.split(" ").maxBy(_.map(_.toInt - 96).sum)
}
 */
object ProdFib {

  def productFib(prod: Long): Array[Long] = {
    lazy    val fibs:Stream[Long] = 0L #:: 1L #:: (fibs zip fibs.tail).map{ t => t._1 + t._2 }
    val arr= fibs.sliding(2).toStream.filter(_.product >= prod).head.toArray
    if(arr.product== prod) arr ++ Array(1L)
      else arr ++ Array(0L)
    }
  }
/*
object ProdFib {

  def productFib(prod: Long): Array[Long] =
    Stream.iterate((0L, 1L)){case (n1, n2) => (n2, n1 + n2)}
      .filter{case (n1, n2) => n1 * n2 >= prod}
      .map{case (n1, n2) => Array(n1, n2, if (n1 * n2 == prod) 1 else 0)}
      .head
}
 */
ProdFib.productFib(5895)