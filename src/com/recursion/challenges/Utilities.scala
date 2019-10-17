package com.recursion.challenges

object Utilities {

  def getMinElementArray( array : Array[Int] ) : Int ={
    def getMinElementArrayIns( arrayIns : Array[Int], num : Int) : Int = {
      if(!arrayIns.isEmpty)
        { if( arrayIns.head < num) getMinElementArrayIns( arrayIns.tail, arrayIns.head)
          else num}
      else num
    }
    getMinElementArrayIns(  array, array(0))
  }

  def getMaxElementArray( array : Array[Int] ) : Int ={
    def getMinElementArrayIns( arrayIns : Array[Int], num : Int) : Int = {
      if(!arrayIns.isEmpty)
      { if( arrayIns.head > num) getMinElementArrayIns( arrayIns.tail, arrayIns.head)
      else num}
      else num
    }
    getMinElementArrayIns(  array, array(0))
  }

  def sumArray( array : Array[Int] ) : Int = {
    if(!array.isEmpty) { array.head + sumArray(array.tail) }
    else 0
  }

  def avgArray( arrayPar : Array[Int] ) : Double = {
    sumArray(arrayPar )/ arrayPar.length
  }

  def compFactorial( num : Int ) : Int ={
      def factorialIns( ini : Int, numIns : Int ) : Int ={
        if( ini < numIns) ini * factorialIns(ini+1,numIns)
        else 1
      }
    factorialIns(1,num)
  }




}
