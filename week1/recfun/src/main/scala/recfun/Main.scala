package recfun

import scala.collection.mutable.Stack

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      def factorial(n: Integer): Integer = {

        if (n <= 1)
          1
        else
          n * factorial(n-1)
      }

      factorial(r) / (factorial(c) * factorial((r - c)))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      var stack = Stack[Char]()

      def loop(list: List[Char]): Unit = {

        list match {
          case current :: rest if current.equals('(') => {
            stack.push(current)
            loop(rest)
          }
          case current :: rest if current.equals(')') => {

            if (stack.isEmpty) {
              stack.push(current)
              return
            }

            stack.pop()
            loop(rest)
          }
          case _ :: rest => loop(rest)
          case Nil =>
        }

      }

      loop(chars)

      stack.isEmpty
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      var possibilities = 0

      def loop(money: Int, coins: List[Int]): Unit = {

        if (money == 0) {
          possibilities+=1
          return
        } else if (money < 0) {
          return
        }

        coins match {
          case coin :: rest => {
            loop(money - coin, coins)
            loop(money, rest)
          }
          case Nil =>
        }
      }

      loop(money, coins.sorted)
      possibilities
    }
  }
