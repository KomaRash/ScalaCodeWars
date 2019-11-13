object Codewars {
  def accum(s: String) = {
    s.toCharArray.zipWithIndex.foldLeft(List[String]())((sq, templ) => {
      for (i <- 0 to templ._2) yield {
        i match {
          case 0 => templ._1.toString.toUpperCase
          case _ => templ._1.toString.toLowerCase()
        }
      }
    }.mkString("") ::sq).reverse.mkString("-")

  }
}

Codewars.accum("AvbD")



  object Sol {

    def isSquare(x: Int): Boolean = {
      if(Math.pow(Math.sqrt(x).toInt.toDouble,2)== x)
        true
      else false
      // your code here

    }
  }
Sol.isSquare(4)
/*
Math.sqrt(x).isWhole
 */