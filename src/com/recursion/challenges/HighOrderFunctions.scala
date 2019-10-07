package com.recursion.challenges

object HighOrderFunctions {

  /**
   * @Author Cesar Advincula
   * @param f the Function
   * @param a Initial Number of the Range
   * @param b Final Number of the Range
   * @return Calculation of the Sum
   */
  def sumDynamic( f : Int => Int , a : Int , b : Int) : Int = {
   if(a <= b)  {  f(a) + sumDynamic(f,a+1,b)}
    else 0
  }

  /**
   * @Author Cesar
   * @param a
   * @return Calculation of the
   */
  def compCuadrare(  a  : Int  ) : Int = {
    a*a
  }

  /**
   * @Author Cesar Advincula
   * @param num
   * @return Calculation of the Factorial
   */
  def compFactorial( num : Int ) : Int = {
      def calculationFactorial(  a : Int , b : Int): Int = {
        if( a<=b) { a * calculationFactorial(a+1,b)  }
        else 1
      }
    calculationFactorial(1,num)
  }

  def main(args: Array[String]): Unit = {
     println( sumDynamic( compCuadrare, 1,2)) //Dynamic with Functions
     println( sumDynamic( x => x*x, 1,2) )    //Anonymous Functions
     println( compFactorial( 4 ) )    //Factorial
     println( sumDynamic(compFactorial,1,3) ) //Sum Dynamic of Factorials
  }

}
