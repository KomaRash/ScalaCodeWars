
object SumOfDigits {

  @scala.annotation.tailrec
  def digitalRoot(n: Int): Int = n.toString.toCharArray.map(_.asDigit)
    .sum match {case f:Int =>if(f >9) digitalRoot(f) else f}
}
object Kata {

  def findMissingLetter(chars: Array[Char]) ='a'.to('z').toArray.dropWhile(chars.head==_)
}
Kata.findMissingLetter("sasd".toCharArray)

object Solution {
  def zeros(n: Int): Int =n/5

}
Solution.zeros(16)