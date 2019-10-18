package com.hackerrank.arrays

object AvgMultidimensionalArrays {

  /**
   * @Author: Cesar Advincula
   * @param a : Array
   * @return Minimun Element of Array
   */
  def compMinArray( a : Array[Int]) : Int = {
    def calMinInsArray( arr : Array[Int],  ini : Int ) : Int ={
        if(! arr.isEmpty){ if( arr.head <=  ini ) { calMinInsArray(arr.tail,  arr.head)  }
                          else { calMinInsArray(arr.tail,  ini)  } }
        else ini
    }
    calMinInsArray(a, a(0))
  }

  /**
   *  @Author: Cesar Advincula
   * @param arr  Array Int
   * @return Sum of Array element
   */
  def compSumElemArray( arr : Array[Int]) : Int ={
    def compInsSumElemArray( a : Array[Int], result : Int ) : Int = {
      if(!a.isEmpty)  { compInsSumElemArray( a.tail, result + a.head )  }
      else      {     result }
    }
    compInsSumElemArray(arr,0)
  }

  /**
   *  @Author: Cesar Advincula
   * @param arr  Array Int
   * @return Sum of Array element
   */
  def compAvgElemArray( arr : Array[Int]) : Int ={
    def compInsSumElemArray( a : Array[Int], result : Int ) : Int = {
      if(!a.isEmpty)  { compInsSumElemArray( a.tail, result + a.head )  }
      else      {     result }
    }
    compInsSumElemArray(arr,0) / arr.length
  }



  var arrAvgEmp = Array[Int]()
  /**
   * @Author: Cesar Advincula
   * @param arr Array of Elements
   * @return Array with Element of the Avg of each Array
   */
  def compAverageArrayOfArray(  arr : Array[Array[Int]] ) : Array[Int] ={
    def compAverageArrayOfArrayIns( a  : Array[Array[Int]] , b : Array[Int] ) : Array[Int] = {
        if( ! a.isEmpty ) { compAverageArrayOfArrayIns(a.tail, b:+  compAvgElemArray(a.head) ) }
        else  b
    }
    compAverageArrayOfArrayIns(arr, arrAvgEmp)
  }


  def main(args: Array[String]): Unit = {
    val arr = Array( Array(1,2,3), Array(2,12,10,12))
    println( compAverageArrayOfArray(arr)(0) )
    println( compAverageArrayOfArray(arr)(1) )
  }

}
