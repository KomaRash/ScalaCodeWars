object SquaresPerimeter {
  def perimeter(n: BigInt): BigInt ={
    lazy  val fibs:Stream[BigInt] = BigInt(1) #::  BigInt(1) #:: (fibs zip fibs.tail).map{ t => t._1 + t._2 }
    fibs.take((n+1).toInt).sum.*(4)
  }

}
object Suite {

  def going(n: Int) = {
    lazy val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: (fibs.zipWithIndex.tail map { case (int, i) => int * i })
    (math floor math.pow(10.0,6)* fibs.drop(2).take(n).sum.toDouble * (1.0 / fibs.drop(2).take(n).reverse.head.toDouble ))/
      math.pow(10.0,6)

  }
}
object SuiteCodeWars {

  def going(n: Int): Double =
    ((2 to n)
      .scanLeft((2 to n).map(BigDecimal(_)).product) { case (last, i) => last / i }
      .map(d => (1/d).toDouble)
      .sum)
    match { case v => s"$v".take(8).toDouble }
}


/*

object SquaresPerimeter {
  def fib(n: Int): BigInt = {
    def fibTail(n: Int, a: BigInt, b: BigInt): BigInt = n match {
      case 0 => a
      case _ => fibTail(n-1, b, a + b)
    }
    fibTail(n, 0, 1)
  }

  def perimeter(n: BigInt): BigInt = 4*(fib(n.toInt+1+2)-fib(2))
}

 */