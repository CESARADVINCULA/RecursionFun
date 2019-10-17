package com.hackerrank.arrays

object Sum2Dimensional {

  /**
   *
   * @Auhtor Cesar Advincula
   * @return Max num of Array from a Sum of a Pattern Calculation
   */
  def calMaxArray( a : Array[Int] ) : Int = {
    def calMaxArrayIns( aPar : Array[Int] , result : Int) : Int = {
      if(! aPar.isEmpty)  { if(aPar.head > result )  calMaxArrayIns( aPar.tail , aPar.head )
      else                calMaxArrayIns( aPar.tail , result )}
      else                { result }
    }
    calMaxArrayIns(a,a(0))
  }

  var resulArrayIns= Array[Int]()
  // Complete the hourglassSum function below.
  def hourglassSum(arr: Array[Array[Int]]): Int = {
    def calArrayPatterIns( aPar :  Array[Array[Int]], fixedVal : Int, iPar : Int , jPar : Int , resulPar : Array[Int]  ) : Array[Int] = {
      if( iPar < 4 && jPar < 4) {
        calArrayPatterIns( aPar , fixedVal , iPar , jPar+ fixedVal +1 , resulPar :+ (aPar(iPar)(jPar)+aPar(iPar)(jPar+1)+aPar(iPar)(jPar+2)+aPar(iPar+1)(jPar+1)+aPar(iPar+2)(jPar)+aPar(iPar+2)(jPar+1)+aPar(iPar+2)(jPar+2)))
      }
      else {
        if( iPar < 4 && jPar == 4) {
          calArrayPatterIns( aPar , fixedVal , iPar +1 , fixedVal, resulPar )
        }
        else      {  resulPar  }
      }
    }

    for ( i <- 0 to 3){
      for (j <- 0 to 3) {
        print( arr(i)(j)+arr(i)(j+1)+arr(i)(j+2)+arr(i+1)(j+1)+arr(i+2)(j)+arr(i+2)(j+1)+arr(i+2)(j+2) + " ")
      }
      println(" ")
    }
    println("********")
    calMaxArray(calArrayPatterIns(arr,0,0,0,resulArrayIns ))

  }

  def main(args: Array[String]): Unit = {

    val arr = Array.ofDim[Int](6, 6)
    val start = 20
    val end   = 30
    val rnd = new scala.util.Random

    for(i <- 0 to 5)
    {
      for(j <- 0 to 5)  {
        arr(i)(j)=start + rnd.nextInt( (end - start) + 1 )
      }
    }

    println(hourglassSum(arr))


  }


}
