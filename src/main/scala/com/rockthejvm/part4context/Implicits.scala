package com.rockthejvm.part4context

object Implicits {

  // given/using clauses
  // the ability to pass arguments automatically (implicitly) by the compiler
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // implicit vs using
  def combineAll[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
    list.reduce(semigroup.combine)

  // given vs implicit val (no with)
  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }

  val sumOf10 = combineAll((1 to 10).toList)

  // implicit arg -> using clauses
  // implicit val -> given declaration

  /**
   * extension methods
   */
  // Scala 3 - extension methods
  extension (number: Int) def isEven = number % 2 == 0

  val is23Even = 23.isEven

  // Scala 2 - extension methods = implicit class
  implicit class MyRichInteger(number: Int) {
    // extension methods here
    def isEven_v2 = number % 2 == 0
  }
  val questionOfMyLife = 23.isEven_v2

  /**
   * implicit conversion
   */
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name."
  }

  // implicit conversion - SUPER DANGEROUS
  implicit def string2Person(x: String): Person = Person(x)
  val danielSaysHi                              = "Daniel".greet()

  // implicit def => synthesize NEW implicit values
  implicit def semigroupOfOption[A](implicit
      semigroup: Semigroup[A]
  ): Semigroup[Option[A]] = new Semigroup[Option[A]] {
    override def combine(x: Option[A], y: Option[A]): Option[A] = for {
      valueX <- x
      valueY <- y
    } yield semigroup.combine(valueX, valueY)
  }

  // given semigroupOfOption[A](using semigroup: Semigroup[A]): Semigroup[Option[A]] with ...

  // organizing implicits == organizing contextual abstractions
  // import yourPackage.* // also imports implicits

  /*
    Why implicits will be phased out:
    - the implicit keyword has many different meanings
    - conversions are easy to abuse
    - implicits are very hard to track down while debugging
    (givens are also not trivial, but they are explicitly imported)
   */

  /*
    Contextual abstractions:
    - given/using clauses
    - extension methods
    - explicitly declared implicit conversions
   */

  def main(args: Array[String]): Unit = {
    println(sumOf10)
  }
}
