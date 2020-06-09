package com.currying

object CurryingTesting {

  //A Function that return another Function
  def add2(a: Int): Int => Int = (b: Int) => a + b;

  def main(args: Array[String]): Unit = {
    //In the Currying it takes the two Form
    //First the sum is Function that was inialize with the Variable
    println("*******************")
    val sum = add2(10)
    println(sum(5))
    println(sum(10))
    println(sum(10))


    //This is something That I am using the Int and it takes only the result
    println("*******************")
    val  x = add2(10)(20)
    println(x)

  }

}
