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
  trait Combinator[A] {
    def combine(x: A, y: A): A
  }

  extension [A](list: List[A])
    def combineAll(using combinator: Combinator[A]): A =
      list.reduce(combinator.combine)

  given intCombinator: Combinator[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  val firstSum = aList.combineAll
  val firstSumStrings = List("123").combineAll

  def main(args: Array[String]): Unit = {
    println(danielGreeting)
  }
}
