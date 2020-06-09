package com.recursion.challenges

object Iteractions {

  /**
   * @Author Cesar Advincula
   * @param row
   * @param col
   * @return Calculation of the Pascal Triangle
   */
  def compPascalNum( row : Int, col : Int ) : Int = {
      if( row== col || col ==1  ) { 1 }
      else  { compPascalNum(row-1,col-1)  + compPascalNum(row-1,col)}
  }

  /**
   * @Author Cesar Advincula
   * @param num
   * @return Calculation of the Fibonnaci
   */
  def calFibonnaci(  num : Int) : Int = {
    def calFibIns( iniIter: Int, finIter: Int, result : Int , befoResult : Int) : Int = {
      if( iniIter <= finIter ) {
        print( result + " ")
        calFibIns(iniIter+1,finIter,result+befoResult,result)
      }
      else result
    }
    calFibIns(1,num,0,1)
  }


  def preCompSQRT(  x : Int , y : Double) : Double = {
    x- (y*y)
  }

  /**
   * @Author Cesar Advincula
   * @param target
   * @return calculation of SQRT using Binary Search
   */
  def compSQRT(  target : Int ) : Double = {
    def calculationSQRT( targetArr : Int, fixedPoint : Double , iniIterator : Int, FinIterator: Int, step : Double, flag : Int ) : Double = {
      if (iniIterator <= FinIterator) {
        if (preCompSQRT(targetArr, fixedPoint) == 0) {
          calculationSQRT(targetArr, fixedPoint, iniIterator + 1, FinIterator, step, flag)
        }
        else {
          if (preCompSQRT(targetArr, fixedPoint) < 0 && flag == 1) {
            calculationSQRT(targetArr, fixedPoint - step / 2, iniIterator + 1, FinIterator, step - step / 2, 0)
          }
          else {
            if (preCompSQRT(targetArr, fixedPoint) > 0 && flag == 0) {
              calculationSQRT(targetArr, fixedPoint + step / 2, iniIterator + 1, FinIterator, step + step / 2, 1)
            }
            else {
              if (flag == 0) {
                calculationSQRT(targetArr, fixedPoint - step, iniIterator + 1, FinIterator, step, 1)
              }
              else {
                calculationSQRT(targetArr, fixedPoint + step, iniIterator + 1, FinIterator, step, 1)
              }
            }
          }
        }

      }
      else { fixedPoint }
    }
    calculationSQRT( target, 1, 1,1000,1,0)
  }


  def main(args: Array[String]): Unit = {

    for (i <-1 to 5  )
    {       for(j <-1 to i)
            { print(compPascalNum(i,j) +" ")}
    println()
    }

    println("Fibonnaci : ")
    calFibonnaci(4)

    println()
    println("Compute SQRT : ")
    println(compSQRT(3))
    println(compSQRT(9))
  }

}

