package com.recursion.financial

object CashFlowSolutions {

  /**
   * Computation of the Net Present Value
   *  Author: Cesar Advincula
   * @param cashFlow
   * @param interestRate
   * @return
   */
  def compNVP( cashFlow : Array[Double], interestRate : Double) : Double ={
    def compNVPIns( cashFlow : Array[Double], interestRate : Double, iterator: Int) : Double ={
      if( ! cashFlow.isEmpty ) {
        cashFlow.head / ( Math.pow((1+interestRate), iterator)  ) + compNVPIns(cashFlow.tail, interestRate, iterator +1)
      }
      else {
        0
      }
    }
    compNVPIns(cashFlow, interestRate ,0)
  }

  /**
   *  Search Binary for Interest Rate
   *  Author: Cesar Advincula
   * @param cashFlow
   * @return
   */
  def compIRR(  cashFlow : Array[Double] ) : Double ={
    val iterator = 1
    val limIteration = 1000
    val resultIRR = 3
    val step = 0.1
    val flagBreak = 0
    def insCompIRR( cashFlow : Array[Double], iterator : Int, limIteration : Int,
                    resultIRR : Double , step : Double, flagBreak : Int ) : Double ={

      if( iterator < limIteration) {
        if( compNVP (cashFlow, resultIRR) ==0  ) {
          insCompIRR( cashFlow, limIteration,limIteration, resultIRR, step, flagBreak )
        }
        else {
          if( compNVP (cashFlow, resultIRR)   < 0 && flagBreak ==0 ){
            insCompIRR( cashFlow, iterator + 1,limIteration, resultIRR - step, step, flagBreak )
          }
          else {
            if( compNVP (cashFlow, resultIRR)   > 0 && flagBreak ==0 ) {
              insCompIRR( cashFlow, iterator + 1,limIteration, resultIRR + step/2,  step/2, 1 )
            }
            else {
              if( compNVP (cashFlow, resultIRR) > 0 &&  flagBreak ==1 ){
                insCompIRR( cashFlow, iterator + 1,limIteration, resultIRR + step,  step, flagBreak )
              }
              else {
                insCompIRR( cashFlow, iterator + 1,limIteration, resultIRR - step/2,  step/2, 0 )
              }
            }
          }
        }
      }
      else {
        resultIRR
      }
    }
    insCompIRR( cashFlow, iterator, limIteration, resultIRR, step, flagBreak )
  }



  def main(args: Array[String]): Unit = {
    val cashFlow = Array(-10 ,5 ,7.0)
    println("compIRR " + compIRR(cashFlow) * 100+"%")
  }

}
