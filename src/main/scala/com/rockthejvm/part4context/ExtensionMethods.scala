package com.rockthejvm.part4context

object ExtensionMethods {

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name, nice to meet you"
  }

  extension (string: String) def greetAsPerson: String = Person(string).greet

  val danielGreeting = "Daniel".greetAsPerson

  // Generic extension methods
  extension [A](list: List[A]) def ends: (A, A) = (list.head, list.last)

  val aList     = List(1, 2, 3, 4)
  val firstLast = aList.ends

  // reason: make APIs very expressive
  // reason 2: enhance CERTAIN types with new capabilities
  // => super-powerful code
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  extension [A](list: List[A])
    def combineAll(using combinator: Semigroup[A]): A =
      list.reduce(combinator.combine)

  given intCombinator: Semigroup[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  val firstSum = aList.combineAll

  // no context given, does not compile
  // val firstSumStrings = List("123").combineAll

  // grouping extensions
  object GroupedExtensions {
    extension [A](list: List[A]) {
      def ends: (A, A) = (list.head, list.last)
      def combineAll(using combinator: Semigroup[A]): A =
        list.reduce(combinator.combine)
    }
  }

  // call extension methods directly
  val firstLast_v2 = ends(aList)

  /**
   * Exercises
   * 1. Add an isPrime method to the Int type
   *    You should be able to write 7.isPrime
   * 2. Add extensions to Tree:
   *    - map(f: A => B): Tree[B]
   *    - forAll(predicate: A => Boolean): Boolean
   *    - sum => sum of all elements of the tree
   */

  // 1
  extension (number: Int) {
    def isPrime: Boolean = {
      // if (number < 0) false
      // else if (number >= 0 && number <= 2) true
      // else {
      //   val end = number / 2
      //   (2 to end).forall(divisor => number % divisor != 0)
      // }

      def isPrimeAux(divisor: Int): Boolean =
        if (divisor > number / 2) true
        else if (number % divisor == 0) false
        else isPrimeAux(divisor + 1)

      assert(number >= 0)
      if (number == 0 || number == 1) false
      else isPrimeAux(2)
    }
  }

  extension [A](tree: Tree[A]) {
    def map[B](f: A => B): Tree[B] = {
      tree match {
        case Leaf(a)             => Leaf(f(a))
        case Branch(left, right) => Branch(left.map(f), right.map(f))
      }
    }

    def forAll(predicate: A => Boolean): Boolean = {
      tree match {
        case Leaf(a) => predicate(a)
        case Branch(left, right) =>
          left.forAll(predicate) && right.forAll(predicate)
      }
    }

    def combineAll(using combinator: Semigroup[A]): A = tree match
      case Leaf(value) => value
      case Branch(left, right) =>
        combinator.combine(left.combineAll, right.combineAll)
  }

  extension (tree: Tree[Int]) {
    def sum: Int = tree match {
      case Leaf(value)         => value
      case Branch(left, right) => left.sum + right.sum
    }
  }

  // "library code" = cannot change
  sealed abstract class Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def main(args: Array[String]): Unit = {
    println(danielGreeting)
    println("2 " + 2.isPrime)
    println("3 " + 3.isPrime)
    println("4 " + 4.isPrime)
    println("7 " + 7.isPrime)
    println("113 " + 113.isPrime)
    println("120 " + 120.isPrime)

    val aTree: Tree[Int] = Branch(Branch(Leaf(3), Leaf(1)), Leaf(10))
    println(aTree.map(_ + 1))
    println(aTree.forAll(_ % 2 == 0)) // false
    println(aTree.sum)
    println(aTree.combineAll)
  }
}
