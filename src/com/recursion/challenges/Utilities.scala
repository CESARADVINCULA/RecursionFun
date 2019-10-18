package com.recursion.challenges

object Utilities {

  /**
   * @Author Cesar Advincula
   * @param array
   * @return get Minimum Element from an Array
   */
  def getMinElementArray( array : Array[Int] ) : Int ={
    def getMinElementArrayIns( arrayIns : Array[Int], num : Int) : Int = {
      if(!arrayIns.isEmpty)
        { if( arrayIns.head < num) getMinElementArrayIns( arrayIns.tail, arrayIns.head)
          else num}
      else num
    }
    getMinElementArrayIns(  array, array(0))
  }

  /**
   * @Author Cesar Advincula
   * @param array
   * @return get the Maximum Element from an Array
   */
  def getMaxElementArray( array : Array[Int] ) : Int ={
    def getMinElementArrayIns( arrayIns : Array[Int], num : Int) : Int = {
      if(!arrayIns.isEmpty)
      { if( arrayIns.head > num) getMinElementArrayIns( arrayIns.tail, arrayIns.head)
      else num}
      else num
    }
    getMinElementArrayIns(  array, array(0))
  }

  /**
   * @Author Cesar Advincula
   * @param array
   * @return Sum Elements from an Array
   */
  def sumArray( array : Array[Int] ) : Int = {
    if(!array.isEmpty) { array.head + sumArray(array.tail) }
    else 0
  }

  /**
   * @Author Cesar Advincula
   * @param arrayPar
   * @return get the Average from an Array
   */
  def avgArray( arrayPar : Array[Int] ) : Double = {
    sumArray(arrayPar )/ arrayPar.length
  }

  /**
   * @Author Cesar Advincula
   * @param num
   * @return the Factorial from a Num
   */
  def compFactorial( num : Int ) : Int ={
      def factorialIns( ini : Int, numIns : Int ) : Int ={
        if( ini < numIns) ini * factorialIns(ini+1,numIns)
        else 1
      }
    factorialIns(1,num)
  }

  /**
   * @Author Cesar Advincula
   * @param num
   * @return check if the Number is Prime
   */
  def checkPrimeNumber( num : Int ) : Boolean ={
    if(num==2) { true}
    else {if( num%2 !=0 &&  num %(Math.sqrt(num)) !=0 ) { true}
          else false}
  }



  def main(args: Array[String]): Unit = {

 }



}
