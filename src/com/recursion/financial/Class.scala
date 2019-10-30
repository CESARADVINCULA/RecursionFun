package com.recursion.financial

class MyCode {

  def parensMatch(chars: List[Char]): Boolean = {
    def f(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) {
        numOpens == 0
      } else {
        val h = chars.head
        val n =
          if (h == '('|| h=='['|| h=='{') numOpens + 1
          else if (h == ')'||h == ']'||h == '}') numOpens - 1
          else numOpens
        if (n >= 0) f(chars.tail, n)
        else false
      }
    }

    f(chars, 0)
  }


  def main(args: Array[String]): Unit = {
    // The function must handle parens (), square brackets [], and curly braces {}.
    println(" balance "+parensMatch("(a[0]+b[2c[6]]) {24 + 53}".toList))
    println(" balance "+parensMatch("[()]{}([])".toList))
    println(" balance "+ !parensMatch("(c]".toList))
    println(" balance "+ parensMatch("{(a[])".toList))
    println(" balance "+parensMatch("([)]".toList))
    println(" balance "+parensMatch(")(".toList))
    println(" balance "+parensMatch("".toList))
  }

}
