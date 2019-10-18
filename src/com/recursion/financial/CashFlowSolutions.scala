package com.recursion.financial

object CashFlowSolutions {

  /**
   * @Author Cesar Advincula
   * @param cashFlow Array with its the Cash Flow
   * @param interestRate
   * @return Calculation of Net Present Value
   */
  def compNPV( cashFlow: Array[Int], interestRate : Double ) : Double ={
    def compNPVIns( cashFlowArr: Array[Int], interestRateArr : Double, iterator : Int ) : Double ={
      if(iterator <= cashFlowArr.length+1) {
        cashFlowArr.head / ( Math.pow( (1+interestRateArr) , iterator )) + compNPVIns( cashFlowArr.tail, interestRateArr , iterator + 1) }
      else {  0  }
    }
    compNPVIns( cashFlow,interestRate,0 )
  }

  /**
   * @Author Cesar Advincula
   * @param cashFlowArr
   * @return calculation of the IRR
   */
  def compIRR(  cashFlowArr : Array[Int] ) : Double = {
   def calculationIRR( cashFlow : Array[Int], fixedPointInterestRate : Double , iniIterator : Int, FinIterator: Int, step : Double, flag : Int ) : Double = {
    if (iniIterator <= FinIterator) {
      if (compNPV(cashFlow, fixedPointInterestRate) == 0) {
        calculationIRR(cashFlow, fixedPointInterestRate, iniIterator + 1, FinIterator, step, flag)
      }
      else {
        if (compNPV(cashFlow, fixedPointInterestRate) < 0 && flag == 1) {
          calculationIRR(cashFlow, fixedPointInterestRate - step / 2, iniIterator + 1, FinIterator, step - step / 2, 0)
        }

        else {
          if (compNPV(cashFlow, fixedPointInterestRate) > 0 && flag == 0) {
            calculationIRR(cashFlow, fixedPointInterestRate + step / 2, iniIterator + 1, FinIterator, step + step / 2, 1)
          }

          else {
            if (flag == 0) {
              calculationIRR(cashFlow, fixedPointInterestRate - step, iniIterator + 1, FinIterator, step, 1)
            }
            else {
              calculationIRR(cashFlow, fixedPointInterestRate + step, iniIterator + 1, FinIterator, step, 1)
            }
          }
        }
      }

    }
    else { fixedPointInterestRate }
  }
    calculationIRR( cashFlowArr, 1, 1,1000,0.1,0)
  }


  def main(args: Array[String]): Unit = {
    val cashFlow = Array(-10,5,7)
    val interestRate = 0.1

    println( " compNPV "+compNPV(cashFlow,interestRate))
    println( " compIRR "+compIRR(cashFlow))
  }

}
