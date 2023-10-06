package com.rockthejvm.practice

import scala.annotation.tailrec

sealed abstract class FSet[A] extends (A => Boolean) {
  // main api
  def contains(elem: A): Boolean
  def apply(elem: A): Boolean = contains(elem)

  infix def +(elem: A): FSet[A]
  infix def ++(anotherSet: FSet[A]): FSet[A]

  // classics
  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def foreach(f: A => Unit): Unit

  // utilities
  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]

  // negation = all the elements of type A EXCEPT the elements in this set
  def unary_! : FSet[A] = new PBSet(x => !contains(x))
}

// { x in N | x % 2 == 0 }
// property-based set
class PBSet[A](property: A => Boolean) extends FSet[A] {
  // main api
  def contains(elem: A): Boolean = property(elem)

  infix def +(elem: A): FSet[A] =
    new PBSet(x => x == elem || property(x))

  infix def ++(anotherSet: FSet[A]): FSet[A] =
    new PBSet(x => property(x) || anotherSet(x))

  // classics
  def map[B](f: A => B): FSet[B] = politelyFail()

  def flatMap[B](f: A => FSet[B]): FSet[B] = politelyFail()

  def filter(predicate: A => Boolean): FSet[A] =
    new PBSet(x => property(x) && predicate(x))

  def foreach(f: A => Unit): Unit = politelyFail()

  // utilities
  infix def -(elem: A): FSet[A] =
    filter(x => x != elem)

  infix def --(anotherSet: FSet[A]): FSet[A] =
    filter(!anotherSet)

  infix def &(anotherSet: FSet[A]): FSet[A] =
    filter(anotherSet)

  // extra utilities (internal)
  private def politelyFail() = throw new RuntimeException("Set might not be iterable...")
}

final case class AllInclusiveSet[A]() extends PBSet[A](_ => true)

final case class Empty[A]() extends FSet[A] {
  override def contains(elem: A): Boolean = false

  override infix def +(elem: A): FSet[A] = Cons(elem, Empty())

  override infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet

  override def map[B](f: A => B): FSet[B] = Empty()

  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()

  override def filter(predicate: A => Boolean): FSet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override infix def -(elem: A): FSet[A] = this

  override infix def --(anotherSet: FSet[A]): FSet[A] = this

  override infix def &(anotherSet: FSet[A]): FSet[A] = this
}

final case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {
  override def contains(elem: A): Boolean = {
    head == elem || tail.contains(elem)
//    @tailrec
//    def containsRec(remaining: FSet[A]): Boolean = remaining match {
//      case Empty() => false
//      case Cons(head, tail) =>
//        if (head == elem) true
//        else containsRec(tail)
//    }
//    containsRec(this)
  }

  override infix def +(elem: A): FSet[A] =
    if (contains(elem)) this
    else Cons(elem, this)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = {
    tail ++ anotherSet + head
//    @tailrec
//    def appendRec(remaining: FSet[A], acc: FSet[A]): FSet[A] = remaining match {
//      case Empty() => acc
//      case Cons(head, tail) => appendRec(tail, Cons(head, acc))
//    }
//    appendRec(this, anotherSet)
  }

  override def map[B](f: A => B): FSet[B] = {
    tail.map(f) + f(head)
//    @tailrec
//    def mapRec(remaining: FSet[A], acc: FSet[B]): FSet[B] = remaining match {
//      case Empty() => acc
//      case Cons(head, tail) => mapRec(tail, acc + f(head))
//    }
//    mapRec(this, Empty[B]())
  }

  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): FSet[A] = {
    val filteredTail = tail.filter(predicate)
    if predicate(head) then filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override infix def -(elem: A): FSet[A] =
    // this.filter(_ != elem)
    if (head == elem) tail
    else tail - elem + head

  // [1, 2, 3] -- [2, 3]
  // 1 -> !anotherSet.contains(1)
  // 2 -> !anotherSet.contains(2)
  // 3 -> !anotherSet.contains(3)
  override infix def --(anotherSet: FSet[A]): FSet[A] =
    this.filter(!anotherSet(_))

  override infix def &(anotherSet: FSet[A]): FSet[A] =
    this.filter(anotherSet)

}

object FSet{
  def apply[A](values: A*): FSet[A] = {
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] = {
      if (valuesSeq.isEmpty) acc
      else buildSet(valuesSeq.tail, acc + valuesSeq.head)
    }
    buildSet(values, Empty())
  }
}

object FunctionalSetPlayground {
  def main(args: Array[String]): Unit = {
    val first5 = FSet(1, 2, 3, 4, 5)
    val someNumbers = FSet(4, 5, 6, 7, 8)
    println(first5.contains(5)) // true
    println(first5(6)) // false
    println((first5 + 10).contains(10)) // true
    println(first5.map(_ * 2).contains(10)) // true
    println(first5.map(_ % 2).contains(1)) // true
    println(first5.flatMap(x => FSet(x, x + 1)).contains(7)) // false

    val aSet = Set(1, 2, 3)
    val aList = (1 to 10).toList
    println(aList.filter(aSet))

    println((first5 - 3).contains(3)) // false
    println((first5 -- someNumbers).contains(4)) // false
    println((first5 & someNumbers).contains(4)) // true

    val naturals = new PBSet[Int](_ => true)
    println(naturals.contains(1512421)) // true
    println(!naturals.contains(0)) // false
    println((!naturals + 1 + 2 + 3).contains(3)) // true
    //println(!naturals.map(_ + 1)) // throw
  }
}
