package com.rockthejvm.part2afp

import scala.annotation.targetName

object Monads {
  def listStory(): Unit = {
    val aList = List(1, 2, 3)
    val listMultiply = for {
      x <- List(1, 2, 3)
      y <- List(4, 5, 6)
    } yield x * y
    val listMultiply_v2 = List(1, 2, 3).flatMap(x => List(4, 5, 6).map(y => x * y))

    val f = (x: Int) => List(x, x + 1)
    val g = (x: Int) => List(x, 2 * x)
    val pure = (x: Int) => List(x) // same as the list "constructor"

    // property 1: left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every, for every f

    // property 2: right identity
    val rightIdentity = aList.flatMap(pure) == aList // for every list

    // property 3: associativity
    /*
      [1,2,3].flatMap(x => [x, x+1]) = [1,2,2,3,3,4]
      [1,2,2,3,3,4].flatMap(x => [x, 2*x]) = [1,2,2,4,2,4,3,6,3,6,4,8]

      [1,2,3].flatMap(x => f(x).flatMap(g))
     */
    val associativity = aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))
  }

  def optionStory(): Unit = {
    val anOption = Option(42)
    val optionString = for {
      lang <- Option("Scala")
      ver <- Option(3)
    } yield s"$lang-$ver"

    // identical
    val optionString_v2 = Option("Scala").flatMap(lang => Option(3).map(ver => s"$lang-$ver"))

    val f = (x: Int) => Option(x + 1)
    val g = (x: Int) => Option(2 * x)
    val pure = (x: Int) => Option(x) // same as Option "constructor"

    // prop: left-identity
    val leftIdentity = pure(42).flatMap(f) == f(42)
    println(leftIdentity)

    // prop: right-identity
    val rightIdentity = anOption.flatMap(pure) == anOption
    println(rightIdentity)

    // prop 3: associativity
    /*
      anOption.flatMap
     */
    val associativity = anOption.flatMap(f).flatMap(g) == anOption.flatMap(x => f(x).flatMap(g))
  }

  // MONADS = chain dependent computations

  // exercise: IS THIS A MONAD?
  // answer: IT IS A MONAD!
  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] =
      IO(() => f(unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(unsafeRun()).unsafeRun())
  }

  def possiblyMonadStory(): Unit = {
    val aPossiblyMonad = IO(42)

    val f = (x: Int) => IO(() => x + 1)
    val g = (x: Int) => IO(() => 2 * x)
    val pure = (x: Int) => IO(x)

    // prop: left-identity
    val leftIdentity = pure(42).flatMap(f) == f(42)
    println(leftIdentity)

    // prop: right-identity
    val rightIdentity = aPossiblyMonad.flatMap(pure) == aPossiblyMonad
    println(rightIdentity)

    // prop: associativity
    val associativity = aPossiblyMonad.flatMap(f).flatMap(g) == aPossiblyMonad.flatMap(x => f(x).flatMap(g))
    println(associativity)

    // none are true because lambdas with same results aren't equal

    val leftIdentity_v2 = pure(42).flatMap(f).unsafeRun() == f(42).unsafeRun()
    val rightIdentity_v2 = aPossiblyMonad.flatMap(pure).unsafeRun() == aPossiblyMonad.unsafeRun()
    val associativity_v2 = aPossiblyMonad.flatMap(f).flatMap(g).unsafeRun() == aPossiblyMonad.flatMap(x => f(x).flatMap(g)).unsafeRun()

    // real tests: values produced + side effects ordering
    println(leftIdentity_v2)
    println(rightIdentity_v2)
    println(associativity_v2)

    val fs = (x: Int) => IO {
      println("incrementing")
      x + 1
    }

    val gs = (x: Int) => IO {
      println("doubling")
      x * 2
    }

    val associativity_v3 =
      aPossiblyMonad.flatMap(fs).flatMap(gs).unsafeRun() == aPossiblyMonad.flatMap(x => fs(x).flatMap(gs)).unsafeRun()
  }


  object IO {
    @targetName("pure")
    def apply[A](value: => A): IO[A] =
      new IO(() => value)
  }

  def possiblyMonadExample(): Unit = {
    val aPossiblyMonad = IO {
      println("printing my first possibly monad")
      // do something
      42
    }

    val anotherPM = IO {
      println("my second PM")
      "Scala"
    }

    // val aResult = aPossiblyMonad.unsafeRun()
    // println(aResult)

    val aForComprehension = for { // computations are described, not executed
      num <- aPossiblyMonad
      lang <- anotherPM
    } yield s"$num-$lang"
  }

  def main(args: Array[String]): Unit = {
    possiblyMonadStory()
    // possiblyMonadExample()
  }
}
