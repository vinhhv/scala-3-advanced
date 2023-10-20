package com.rockthejvm.part5ts

object SelfTypes {

  trait InstrumentalList {
    def play(): Unit
  }

  // self-type: whoever implements Singer MUST also implement InstrumentalList
  // DO NOT confuse this with a lambda
  trait Singer { self: InstrumentalList =>
    // rest of your API
    def sing(): Unit
  }

  class LeadSinger extends Singer with InstrumentalList {
    override def sing(): Unit = ???
    override def play(): Unit = ???
  }

  // not okay, needs to implement InstrumentalList
  // class Vocalist extends Singer {}

  val jamesHetfield = new Singer with InstrumentalList:
    override def sing(): Unit = ???
    override def play(): Unit = ???

  class Guitarist extends InstrumentalList {
    override def play(): Unit = println("some guitar solo")
  }

  // ok - extending Guitarist <: InstrumentalList
  val ericClapton = new Guitarist with Singer {
    override def sing(): Unit = println("layla")
  }

  // self-types vs inheritance
  class A
  class B extends A // B "is an" A

  trait T
  trait S { self: T => } // S "requires a" T

  // self-types for dependency-injection (DI) = "cake pattern"

  abstract class Component {
    // main general API
  }
  class ComponentA extends Component
  class ComponentB extends Component
  class DependentComponent(
      val component: Component
  ) // regular dependency injection

  // cake pattern
  trait ComponentLayer1 {
    // API
    def actionLayer1(x: Int): String
  }
  trait ComponentLayer2 { self: ComponentLayer1 =>
    // some other API
    def actionLayer2(x: String): Int
  }
  trait Application {
    self: ComponentLayer1 with ComponentLayer2 =>
    // your main API
  }

  // example: a photo taking application API in the style of Instagram
  // layer 1 - small components
  trait Picture extends ComponentLayer1
  trait Stats   extends ComponentLayer1

  // layer 2 - compose
  trait ProfilePage extends ComponentLayer2 with Picture
  trait Analytics   extends ComponentLayer2 with Stats

  // layer 3 - application
  trait AnalyticsApp extends Application with Analytics
  // dependencies are specified in layers, like baking a cake
  // when you put the pieces together, you can pick a possible implementation from each layer

  // self-types: hide preserve the "this" instance
  class SingerWithInnerClass {
    self => // self-type with no type requirement, self == this

    class Voice {
      def sing() = this.toString // this == the voice
    }
  }

  // cyclical inheritance does not work
  // class X extends Y
  // class Y extends X

  // cyclical dependencies, this is fine
  trait X { self: Y => }
  trait Y { self: X => }
  trait Z extends X with Y

  def main(args: Array[String]): Unit = {}
}
