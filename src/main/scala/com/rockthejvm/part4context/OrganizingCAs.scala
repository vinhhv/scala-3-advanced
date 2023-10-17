package com.rockthejvm.part4context

object OrganizingCAs {

  val aList         = List(2, 3, 1, 4)
  val anOrderedList = aList.sorted

  // compiler fetches givens/EMs
  // 1 - local scope
  given reversedOrdering: Ordering[Int] with
    override def compare(x: Int, y: Int): Int = y - x

  // 2 - imported scope
  case class Person(name: String, age: Int)
  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 67)
  )

  object PersonGivens {
    given ageOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int = y.age - x.age
    extension (p: Person)
      def greet(): String = s"Heya, I'm ${p.name}, so glad to meet you."
  }

  // a - import explicitly
  // import PersonGivens.ageOrdering
  // b - import a given for a particular type
  // import PersonGivens.{given Ordering[Person]}
  // c - import all givens
  // import PersonGivens.given

  // warning: PersonGivens.* does NOT also import given instances
  // import PersonGivens.*

  // 3 - companions of all types involved in method signature
  /*
    - Ordering
    - List
    - Person
   */
  // override def sorted[B >: A](using ord: Ordering[B]): List[B]

  object Person {
    given byNameOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int =
        x.name.compareTo(y.name)

    extension (p: Person) def greet(): String = s"Hello, I'm ${p.name}."
  }

  val sortedPersons = persons.sorted

  /*
      Good practice tips
      1) When you have a "default" given (only ONE that makes sense)
         add it in the companion object of the type
      2) When you have MANY possible givens, but ONE that is dominant, add that in
         the companion object and the rest in other objects
      3) Same as above, but NO dominant, add them in separate objects and import explicitly
   */

  // Same principles apply to extension methods as well

  /**
   * Exercises. Create given instances for Ordering[Purchase]
   * - Ordering by total price, descending = 50% of code base
   * - Ordering by unit count, descending = 25% of code base
   * - Ordering by unit price, ascending = 25% of code base
   */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    given byTotalPriceOrdering: Ordering[Purchase] with
      override def compare(x: Purchase, y: Purchase): Int = {
        val xTotalPrice = x.unitPrice * x.nUnits
        val yTotalPrice = y.unitPrice * y.nUnits

        if (xTotalPrice == yTotalPrice) 0
        else if (xTotalPrice < yTotalPrice) -1
        else 1
      }
  }

  object UnitCountOrdering {
    given byUnitCount: Ordering[Purchase] =
      Ordering.fromLessThan((x, y) => y.nUnits > x.nUnits)

  }

  object UnitPriceOrdering {
    given byUnitPrice: Ordering[Purchase] =
      Ordering.fromLessThan((x, y) => y.unitPrice < x.unitPrice)
  }

  val purchases = List(
    Purchase(10, 10),
    Purchase(1, 9),
    Purchase(2, 8)
  )

  def main(args: Array[String]): Unit = {
    println(anOrderedList)
    println(sortedPersons)
    import PersonGivens.* // includes extension methods
    println(Person("Daniel", 99).greet())

    import UnitPriceOrdering.given
    println(purchases.sorted)
  }
}
