package com.rockthejvm.part4context

import scala.concurrent.{ExecutionContext, Future}

object ContextFunctions {

  val aList      = List(2, 1, 3, 4)
  val sortedList = aList.sorted

  // defs can take using clauses
  def methodWithoutContextArgs(nonContextArg: Int)(
      nonContextArg2: String
  ): String = ???

  def methodWithContextArgs(nonContextArg: Int)(using
      nonContextArg2: String
  ): String = ???

  // eta-expansion
  val functionWithoutContextArgs = methodWithoutContextArgs
  // val func2 = methodWithContextArgs // doesn't work

  // context function
  val functionWithContextArgs: Int => String ?=> String = methodWithContextArgs

  // val someResult = functionWithContextArgs(2)(using "scala")

  /*
    - convert methods with using clauses to function values
    - HOF with function values taking given instances as arguments
   */
  // execution context here
  // decoupling passing execution context where function is DEFINED vs where function is CALLED
  // val incrementAsync: Int => Future[Int] = x => Future(x + 1) // doesn't work

  // works because we defer passing execution context to the caller of the function
  val incrementAsync: ExecutionContext ?=> Int => Future[Int] = x =>
    Future(x + 1)

  def main(args: Array[String]): Unit = {}
}
