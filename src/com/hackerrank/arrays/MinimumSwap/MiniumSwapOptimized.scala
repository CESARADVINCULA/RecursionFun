package com.hackerrank.arrays.MinimumSwap

object MiniumSwapOptimized {

  /**
   * @Author Cesar Advincula
   * @param arr
   * @param num
   * @return get the Position in the Array getting the
   */
  def getPositionByNumber(arr: Array[Int], num: Int): Int = {
    def getPositionByNumberIns(arrIns: Array[Int], numIns: Int, result: Int): Int = {
      if (arrIns.head != numIns) {
        getPositionByNumberIns(arrIns.tail, numIns, result + 1)
      }
      else {
        result
      }
    }
    getPositionByNumberIns(arr, num, 0)
  }

  /**
   * @Author Cesar Advincula
   * @param arr
   * @return get the Disorder Array
   */
  def getDisorderArray(arr: Array[Int]): Array[Int] = {
    var arrEmpt2 = Array[Int]()
    def getDisorderArrayIns(arrayIns: Array[Int], arrayOrder: Array[Int], iterIni: Int, iterFin: Int, result: Array[Int]): Array[Int] = {
      if (iterIni <= iterFin) {
        if (arrayIns(iterIni) != arrayOrder(iterIni)) {
          getDisorderArrayIns(arrayIns, arrayOrder, iterIni + 1, iterFin, result :+ arrayIns(iterIni))
        }
        else {
          getDisorderArrayIns(arrayIns, arrayOrder, iterIni + 1, iterFin, result)
        }
      }
      else {
        result
      }
    }
    getDisorderArrayIns(arr, arr.sortWith(_ < _), 0, arr.length - 1, arrEmpt2)
  }


  /**
   * @Author Cesar Advincula
   * @param arr
   * @return check the Ordered of an Array
   */
  def checkOrderArray( arr : Array[Int] ) : Boolean = {
    def checkOrderArrayIns(  arrayOriginal : Array[Int], arrayOrdered : Array[Int],  iterIni : Int, iterFin : Int, result : Int ) : Int ={
      if( iterIni <= iterFin )
      { if( arrayOriginal(iterIni) != arrayOrdered(iterIni) )  {
        checkOrderArrayIns( arrayOriginal, arrayOrdered, iterIni +1 , iterFin, result +1 )
      }
      else {
        checkOrderArrayIns( arrayOriginal, arrayOrdered, iterIni +1 , iterFin, result  )
      }
      }
      else {
        result
      }
    }
    if(checkOrderArrayIns( arr, arr.sortWith(_ < _), 0, arr.length-1,0) > 0 ) false
    else true
  }

  /**
   * @Author Cesar Advincula
   * @param arr
   * @return Getting the Minimum of Swaps to get an Ordered Array
   */
  def minimumSwaps(arr: Array[Int]): Int = {
    def countMinChange(arrOriginalM: Array[Int], num: Int): Int = {

      def changePositionArray(arrOriginal: Array[Int]): Array[Int] = {

        def changingPosArray(arrOriginal: Array[Int], arrOrdered: Array[Int], arrDisorder: Array[Int]): Array[Int] = {

          def changePosition(arr: Array[Int], positionIn: Int, positionFi: Int): Array[Int] = {
            val arr2 = arr.clone()
            def changePositionIns(arrIn: Array[Int], arrOut: Array[Int], positionIni: Int, positionFin: Int): Array[Int] = {
              arrOut(positionIni) = arrIn(positionFin)
              arrOut(positionFin) = arrIn(positionIni)
              arrOut
            }
            changePositionIns(arr, arr2, positionIn, positionFi)
          }
          changePosition(arrOriginal
            , getPositionByNumber(arrOrdered, arrDisorder.reduceLeft(_ min _))
            , getPositionByNumber(arrOriginal, arrDisorder.reduceLeft(_ min _) ) )
        }
        changingPosArray(arrOriginal, arrOriginal.sortWith(_ < _), getDisorderArray(arrOriginal))
      }

      if (checkOrderArray(arrOriginalM) == false) {
        countMinChange(changePositionArray(arrOriginalM), num + 1)
      }
      else num
    }
    countMinChange(arr,0 )
  }



  def main(args: Array[String]): Unit = {
    val arr=Array(8,	45,	35,	84,	79,	12,	74,	92,	81,	82,	61,	32,	36,	1,	65,	44,	89,	40,	28,	20,	97,	90,	22,	87,	48,	26,	56,	18,	49,	71,	23,	34,	59,	54,	14,	16,	19,	76,	83,	95,	31,	30,	69,	7,	9,	60,	66,	25,	52,	5,	37,	27,	63,	80,	24,	42,	3,	50,	6,	11,	64,	10,	96,	47,	38,	57,	2,	88,	100,	4,	78,	85,	21,	29,	75,	94,	43,	77,	33,	86,	98,	68,	73,	72,	13,	91,	70,	41,	17,	15,	67,	93,	62,	39,	53,	51,	55,	58,	99,	46)
    println(" countMinChange " + minimumSwaps(arr) )
  }


}
