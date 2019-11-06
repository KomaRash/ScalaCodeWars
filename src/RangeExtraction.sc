
object Kata {
  def closeEnough(a:Int, b:Int) = (Math.abs(b -a) <= 1)
  def solution(xs: List[Int]) = {
    if (xs.nonEmpty) {
      val ys = xs.reverse
      ys.drop(1).foldLeft(List(List(ys.head))) {
        ((acc, e) => if (closeEnough(e, acc.head.head))
          (e :: acc.head) :: acc.tail
        else
          List(e) :: acc)
      }.map(l => l.length match {
        case 1 => l.head.toString
        case 2 => l.head.toString + "," + l.last.toString
        case _ => l.head.toString + "-" + l.last.toString
      }
      ).mkString(",")
    }
    else
      ""
  }
}

Kata.solution(List(-44, 18, 24, -89, -88, -27, -60, 15, 16, 17, 18, 19, 20, 21, 21, 65, -70, -69, -68, -67, -66, -65, 91,
  -24, -50, -49, -48, -47, -46, -18, -84, -88, 49, 50, 51, 52, 53, 54, 2, 95, 96))
object KataCodeWars {
  def solution(nums: List[Int]): String = {
    nums.foldLeft(List.empty[List[Int]]) {
      case (Nil, x) => List(x)::Nil
      case (xs::xxs, x) =>
        if (xs.head == x-1) (x::xs)::xxs
        else List(x)::xs::xxs
    }.map {
      case x::Nil => s"$x"
      case x::y::Nil => s"$y,$x"
      case x => s"${x.last}-${x.head}"
    }.reverse.mkString(",")
  }
}