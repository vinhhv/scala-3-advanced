package com.rockthejvm.part3async

import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector

object ParallelCollections {

  val aList                = (1 to 1000000).toList
  val anIncrementedList    = aList.map(_ + 1)
  val parList: ParSeq[Int] = aList.par
  val aParallelizedIncrementedList =
    parList.map(_ + 1) // map, flatMap, reduce, filter, etc.

  /*
    Applicable for
    - Seq
    - Vector
    - Array
    - Map
    - Set

    Use-case: faster processing
   */

  // parallel collection build explicitly
  val aParVector = ParVector[Int](1, 2, 3, 4, 5, 6)

  def measure[A](expression: => A): Long = {
    val time = System.currentTimeMillis()
    expression // forcing evaluation
    System.currentTimeMillis() - time
  }

  def compareListTransformation(): Unit = {
    val list = (1 to 10000000).toList
    println("List creation done")

    val listPar = list.par
    println("List par creation done")

    val serialTime = measure(list.map(_ + 1))
    println(s"Serial time: $serialTime")

    val parallelTime = measure(listPar.map(_ + 1))
    println(s"Parallel time: $parallelTime")
  }

  def demoUndefinedOrder(): Unit = {
    val list = (1 to 1000).toList
    val reduction =
      list.reduce(_ - _) // usually bad idea to use non-associative ops
    // [1,2,3].reduce(_ - _) = 1 - 2 - 3 = -4
    // [1,2,3].reduce(_ - _) = 1 - (2 - 3) = 2

    val parallelReduction = list.par.reduce(
      _ - _
    ) // order of operations are undefined, returns different result

    println(s"Sequential reduction: $reduction")
    println(s"Parallel reduction: $parallelReduction")
  }

  // for associative ops, result is deterministic
  def demoDefinedOrder(): Unit = {
    val strings =
      "I love parallel collections but I must be careful".split(" ").toList
    val concatenation    = strings.reduce(_ + " " + _)
    val parConcatenation = strings.par.reduce(_ + " " + _)

    println(s"Sequential concatenation: $concatenation")
    println(s"Parallel concatenation: $parConcatenation")
  }

  // be careful with doing imperative programming on parallel collections
  def demoRaceConditions(): Unit = {
    var sum = 0
    (1 to 1000).toList.foreach(elem => sum += elem)
    println(sum)
  }

  def main(args: Array[String]): Unit = {
    demoRaceConditions()
  }
}
