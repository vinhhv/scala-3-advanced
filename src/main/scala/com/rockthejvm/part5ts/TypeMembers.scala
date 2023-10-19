package com.rockthejvm.part5ts

object TypeMembers {

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    // val, var, def, class, traits, object
    type AnimalType              // abstract type member
    type BoundedAnimal <: Animal // abstract type member with a type bound
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalAlias  = Cat // type alias
    type NestedOption = List[Option[Option[Int]]]
  }

  // using type members
  val ac                      = new AnimalCollection
  val anAnimal: ac.AnimalType = ???

  // val cat: ac.BoundedAnimal = new Cat // BoundedAnimal might be Dog
  val aDog: ac.SuperBoundedAnimal = new Dog // ok, Dog <: SuperBoundedAnimal
  val aCat: ac.AnimalAlias        = new Cat // ok, Cat == AnimalAlias

  // establish relationships between types
  // alternative to generics
  class LList[T] {
    def add(element: T): LList[T] = ???
  }

  class MyList {
    type T
    def add(element: T): MyList = ???
  }

  // .type
  type CatType = aCat.type
  val newCat: CatType = aCat

  class MoreConcreteAnimalCollection extends AnimalCollection {
    override type AnimalType = Dog
  }

  def main(args: Array[String]): Unit = {}
}
