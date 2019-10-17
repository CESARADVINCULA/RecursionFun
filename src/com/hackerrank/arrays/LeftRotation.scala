package com.hackerrank.arrays

object LeftRotation {
  /**
   *
   * @Auhtor Cesar Advincula
   * @return Change the position of the elements from NumRotations
   */

  var arrRslt = Array[Int]()

  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    def orderedArrayIns(  arrayDisPar : Array[Int], dPar : Int  ,dParIter : Int ,  resultPar : Array[Int] , upPar : Int ) :  Array[Int] = {
      if(  dParIter <  arrayDisPar.length ) {
        orderedArrayIns(arrayDisPar, dPar,  dParIter + 1, resultPar :+ arrayDisPar(dParIter), upPar )
      }
      else  {
        if(  dParIter ==  arrayDisPar.length  && upPar <= dPar-1 )
        {  orderedArrayIns(arrayDisPar, dPar,  dParIter , resultPar :+ arrayDisPar(upPar), upPar + 1 )   }
        else
        { resultPar  }
      }
    }

    if (d == 0) { a }
    else {orderedArrayIns( a , d, d,
      arrRslt, 0)}

  }

   def main(args: Array[String]): Unit = {
        val arr= Array(1,2,3,4,5)
        val numRotation=2
          for(i <-0 to arr.length-1)
           {  print( rotLeft(arr, numRotation)(i) + " , " ) }

      }
}