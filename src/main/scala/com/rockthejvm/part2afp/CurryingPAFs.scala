package com.rockthejvm.part2afp

object CurryingPAFs {

  // currying
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3: Int => Int = superAdder(3) // y = 3 + y
  val eight = add3(5)
  val eight_v2 = superAdder(3)(5)

  // curried methods
  def curriedAdder(x: Int)(y: Int): Int =
    x + y

  // methods != function values
  // Scala allows for transform for one to another (method -> function)
  val add4 = curriedAdder(4) // eta-expansion
  val nine = add4(5) // 9

  def increment(x: Int): Int = x + 1
  val aList = List(1, 2, 3)
  val anIncrementedList = aList.map(increment) // eta-expansion

  // underscores are powerful
  def concatenator(a: String, b: String, c: String): String = a + b + c

  val insertName =
    concatenator(
      "Hello, my name is ",
      _: String,
      ", I'm cool"
    ) // x => concatenator(..., x, ...)

  val danielsGreeting = insertName("Daniel")
  val fillInTheBlanks = concatenator(_: String, "Daniel", _: String)
  val vinhsGreeting = fillInTheBlanks("Hi, ", " how are you?")

  /**
   * Exercises
   * 1. Create as many add7 definitions
   * 2.
   */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  // 1 - obtain an add7 function: x => x + 7 out of these 3 definitions
  def add7 = (x: Int) => simpleAddMethod(x, 7)

  // 2 - process a list of numbers and return their string representations under different formats
  // step 1: create a curried formatting method with a formatting string and a value
  // step 2: process a list of numbers with various formats
  val piWith2Dec = "%4.2f".format(Math.PI) // 3.14

  def main(args: Array[String]): Unit = {
    println(piWith2Dec)
  }
}
