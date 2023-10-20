package com.rockthejvm.part5ts

import com.rockthejvm.part5ts.FBoundedPolymorphism.FBP.Dog

object FBoundedPolymorphism {

  object Problem {
    trait Animal {
      def breed: List[Animal]
    }

    class Cat extends Animal {
      override def breed: List[Animal] = List(new Cat, new Dog)
    }

    class Dog extends Animal {
      override def breed: List[Animal] = List(new Dog, new Dog)
    }

    // losing type safety
  }

  object NaiveSolution {
    trait Animal {
      def breed: List[Animal]
    }

    class Cat extends Animal {
      override def breed: List[Cat] = List(new Cat, new Cat)
    }

    class Dog extends Animal {
      override def breed: List[Dog] = List(new Dog, new Dog)
    }

    // I have to write the proper type signatures
    // problem: want the compile to make some guarantees
  }

  object FBP {
    // a recursive type, F-bounded polymorphism
    trait Animal[A <: Animal[A]] {
      def breed: List[Animal[A]]
    }

    class Cat extends Animal[Cat] {
      override def breed: List[Animal[Cat]] = List(new Cat, new Cat)
    }

    class Dog extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)
    }

    // mess up FBP
    class Crocodile extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = ??? // list of dogs
    }
  }

  // example: some ORM libraries
  trait Entity[E <: Entity[E]]
  // example: Java sorting library
  class Person extends Comparable[Person] { // FBP
    override def compareTo(o: Person): Int = ???
  }

  // FBP + self types
  object FBPSelf {
    trait Animal[A <: Animal[A]] { self: A =>
      def breed: List[Animal[A]]
    }

    class Cat extends Animal[Cat] { // Cat == Animal[Cat]
      override def breed: List[Animal[Cat]] = List(new Cat, new Cat)
    }

    class Dog extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)
    }

    // class Crocodile extends Animal[Dog] { // not ok, I must also extend Dog
    //   override def breed: List[Animal[Dog]] = ??? // list of dogs
    // }

    // I can go one level deeper
    trait Fish extends Animal[Fish]
    class Cod extends Fish {
      override def breed: List[Animal[Fish]] = List(new Cod, new Cod)
    }

    class Shark extends Fish {
      override def breed: List[Animal[Fish]] = List(new Cod) // wrong
    }

    // solution level 2
    trait FishL2[A <: FishL2[A]] extends Animal[FishL2[A]] { self: A => }
    class Tuna extends FishL2[Tuna] {
      override def breed: List[Animal[FishL2[Tuna]]] = List(new Tuna)
    }

    // class Swordfish extends FishL2[Swordfish] {
    //   override def breed = List(new Tuna) // doesn't work now
    // }

    // problem: code might be hard to read though
  }

  def main(args: Array[String]): Unit = {}
}
