package com.framework.collections

object FrameCollectionFun {

  def sampleListUnidimensional() : Unit ={
    println("****** List ********")
    val listChk = List(1,2,3,4)
    println("First element of the List  "+ listChk.head)
    println("Last element of the List "+ listChk.tail)
    println("Iterate each Element " )
    listChk.foreach( x => println(x * 2))
    println("Sum of Element inside : " + listChk.sum)
  }

  def sampleListMultiDimensional() : Unit = {
    val listList = List(List(1,2,3,4), List(3,4,5), List(10,-10,3))
    println("Check the First Element : "+ listList.head)
  }

  def sampleArrayUnidimensional() : Unit = {
    println("*********************")
    println("****** Array ********")
    val  arrayInt = Array(1,2,3,4)
    arrayInt.foreach( x => println(x * 3))
  }

  def sampleFrameCollectMap() : Unit = {
    println("****** Map ********")
    val hasMap = Map("x" -> 24, "y" -> 25, "z" -> 26)
    hasMap.foreach(  x => println("This is the Header : "+ x._1
      + " This is the Body :" + x._2))
    println(" hasMap.head " + hasMap.head + " hasMap.head ._1" + hasMap.head._1)
  }


  def sampleDimensionalArray() : Unit = {
        val rows=3
        val cols=4
        val arrayDim = Array.ofDim[Int](rows, cols)

        for( i <- 0 to rows -1) {
          for(j <- 0 to cols -1){
            arrayDim(i)(j) = 1
          }
        }

        println("*******Array Dimensional******")
        for( i <- 0 to rows -1) {
          for(j <- 0 to cols-1){
            print(arrayDim(i)(j)+" ")
          }
          println()
        }
  }

  def sampleArrayOfArray() : Unit = {
    println("*******Array Dimensional******")
    val arrayDim = Array( Array(2,3,4), Array(20,30), Array(3,5,8,10))
    println("First Row "+arrayDim(0)(0) + " "+ arrayDim(0)(1) + " " + arrayDim(0)(2) )
    println("Second Row "+arrayDim(1)(0)+ " "+ arrayDim(1)(1)  )//We cannot put arrayDim(1)(2)
  }

  def main(args: Array[String]): Unit = {
        //List
        sampleListUnidimensional()
        //Array
        sampleListMultiDimensional
        sampleArrayUnidimensional()
        //Maps
        sampleFrameCollectMap()
        /*There are two options to create a Dimensional Array*/
        sampleDimensionalArray()  //Array.ofDim
        sampleArrayOfArray()      //Array of Array

  }

}
