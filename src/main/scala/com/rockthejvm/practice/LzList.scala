package com.rockthejvm.practice

import scala.annotation.tailrec

// Write a lazily evaluated, potentially INFINITE linked list
abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  // utilities
  def #::(element: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A]

  // classics
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A] // takes first N elements from this lazy list
  def takeAsList(n: Int): List[A] = take(n).toList
  def toList: List[A] = {
    @tailrec
    def toListAux(remaining: LzList[A], acc: List[A]): List[A] =
      if (remaining.isEmpty) acc.reverse
      else toListAux(remaining.tail, remaining.head :: acc)

    toListAux(this, List())
  } // use this carefully on large lazy lists
}

case class LzEmpty[A]() extends LzList[A] {
  override def isEmpty: Boolean = true

  override def head: A = throw new NoSuchElementException

  override def tail: LzList[A] = throw new NoSuchElementException

  override def #::(element: A): LzList[A] = LzCons(element, this)

  override infix def ++(another: => LzList[A]): LzList[A] = another

  override def foreach(f: A => Unit): Unit = ()

  override def map[B](f: A => B): LzList[B] = LzEmpty()

  override def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty()

  override def filter(predicate: A => Boolean): LzList[A] = this

  override def take(n: Int): LzList[A] =
    if (n == 0) this
    else throw new RuntimeException(s"Cannot take $n elements from an empty lazy list")
}

class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {
  // hint: use call by need
  override def isEmpty: Boolean = false

  override lazy val head: A = hd

  override lazy val tail: LzList[A] = tl

  override def #::(element: A): LzList[A] = new LzCons(element, this)

  override infix def ++(another: => LzList[A]): LzList[A] =
    new LzCons(head, tail ++ another)

  override def foreach(f: A => Unit): Unit = {
    @tailrec
    def foreachTailrec(lzList: LzList[A]): Unit = {
      if (lzList.isEmpty) ()
      else {
        f(lzList.head)
        foreachTailrec(lzList.tail)
      }
    }
    foreachTailrec(this)
  }

  override def map[B](f: A => B): LzList[B] =
    new LzCons[B](f(head), tail.map(f))

  override def flatMap[B](f: A => LzList[B]): LzList[B] =
    f(head) ++ tail.flatMap(f) // breaks lazy evaluation

  override def filter(predicate: A => Boolean): LzList[A] =
    if (predicate(head)) new LzCons(head, tail.filter(predicate)) // preserves lazy eval
    else tail.filter(predicate) // TODO warning

  override def take(n: Int): LzList[A] =
    if (n <= 0) LzEmpty()
    else if (n == 1) new LzCons(head, LzList.empty) // n == 1 to stop on infinite list
    else new LzCons(head, tail.take(n - 1)) // preserves lazy eval
}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()
  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons(start, generate(generator(start))(generator))
  def from[A](list: List[A]): LzList[A] = list.reverse.foldLeft(LzList.empty) { (curr, elem) =>
    new LzCons(elem, curr)
  }

  def apply[A](values: A*) = LzList.from(values.toList)

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else if (n % t == 0) false
      else isPrimeUntil(t - 1)
    isPrimeUntil(n / 2)
  }

  def fibonacci: LzList[BigInt] = {
    def fibo(first: BigInt, second: BigInt): LzList[BigInt] =
      new LzCons[BigInt](first, fibo(second, first + second))
    fibo(1, 2)
  }

  /**

   sieve([2,3,4,5,6,...]) =
   2 #:: sieve(3,4,5,6,...].filter(_ % 2 != 0))
   2 #:: sieve([3,5,7,9...]
   2 #:: #:: 3 #:: sieve([5,7,9,11...].filter(_ % 3 != 0))
   */
  def eratosthenes: LzList[Int] = {
    def isPrime(n: Int) = {
      def isPrimeTailrec(potentialDivisor: Int): Boolean =
        if (potentialDivisor < 2) true
        else if (n % potentialDivisor == 0) false
        else isPrimeTailrec(potentialDivisor - 1)

      isPrimeTailrec(n / 2)
    }
    def sieve(numbers: LzList[Int]): LzList[Int] =
      if (numbers.isEmpty) numbers
      else if (!isPrime(numbers.head)) sieve(numbers.tail)
      else new LzCons[Int](numbers.head, sieve(numbers.tail.filter(_ % numbers.head != 0)))

    val naturalsFrom2 = LzList.generate(2)(_ + 1)
    sieve(naturalsFrom2)
  }
}

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val naturals = LzList.generate(1)(n => n + 1) // INFINITE list of natural numbers
    println(naturals.head)
    println(naturals.tail.head)
    println(naturals.tail.tail.head)

//    val first50k = naturals.take(50000)
//    first50k.foreach(println)
//    val first50kList = first50k.toList
//    println(first50kList)

    // classics
    println(naturals.map(_ * 2).takeAsList(100))
    println(naturals.flatMap(x => LzList(x, x + 1)).takeAsList(100))
    println(naturals.filter(_ <= 10).takeAsList(9))
    // println(naturals.filter(_ <= 10).takeAsList(10)) // crash with SO or infinite recursion

    val combinationsLazy: LzList[String] = for {
      number <- LzList(1, 2, 3)
      string <- LzList("black", "white")
    } yield s"$number-$string"
    println(combinationsLazy.toList)

    /**
        Exercises:
        1. Lazy list of Fib numbers
          1, 2, 3, 5, 8, 13, 21, 34 ...
        2. Infinite list of prime numbers
          - filter with isPrime
          - Eratosthenes' sieve
          [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, ...]
          [2, 3, 5, 7, 9, 11, 13, 15, 17, 19, ...]
          [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, ...]
     */

    val fibs = LzList.generate((1, 2))((x, y) => (y, x + y)).map(_._1)
    println(fibs.takeAsList(10))

    val fibos = LzList.fibonacci
    println(fibos.takeAsList(10))

    val prime_v1 = naturals.filter(LzList.isPrime)
    println(prime_v1.takeAsList(10))

    val primes_v2 = LzList.eratosthenes
    println(primes_v2.takeAsList(10))
  }
}
