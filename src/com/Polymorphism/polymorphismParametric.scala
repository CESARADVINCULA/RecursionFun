package com.Polymorphism



object polymorphismParametric {

  /**
   * Author: Cesar Advincula
   * Polymorphism, is a class can have many difference forms
   * and have a different behaviors in difference classes as well
   * Then scala have 3 mechanisms:
   *    subtype
   *    parametric
   *    ad-hoc
   */

  /**
   * This example Parametric Polymorphism, is to create a dataStructure
   * to Pass It as Parameter
   */

  trait NewDataStructure
  object Nil extends NewDataStructure {
    val isEmpty = true
  }
  class Cons( val tail: NewDataStructure) extends NewDataStructure {
    val isEmpty = false
  }

  def main(args: Array[String]): Unit = {
    println(Nil.isEmpty)
    //Below is the Polymorphism
    val newDataStructure2 = new Cons( Nil)
    println(newDataStructure2.isEmpty)

    val newDataStructure = new Cons( new Cons( new Cons(Nil) ))
    println(newDataStructure.isEmpty)

  }



}
