package com.rockthejvm.part5ts

object OpaqueTypes {

  object SocialNetwork {
    // some data structures = "domain"
    opaque type Name = String

    object Name {
      def apply(str: String): Name = str
    }

    extension (name: Name) def length: Int = name.length // use String API

    // inside, Name <-> String
    def addFriend(person1: Name, person2: Name): Boolean =
      person1.length == person2.length // use the entire String API
  }

  // outside SocialNetwork, Name and String are NOT related
  import SocialNetwork.*
  // val name: Name = "Daniel" // will not compile
  // why: you don't need (or want) to have access to the entire String API for the Name type

  object Graphics {
    opaque type Color                = Int // in hex
    opaque type ColorFilter <: Color = Int

    val Red: Color   = 0xff000000
    val Green: Color = 0x00ff0000
    val Blue: Color  = 0x0000ff00

    val halfTransparency: ColorFilter = 0x88 // 50%
  }

  import Graphics.*
  case class OverlayFilter(c: Color)

  val fadeLayer = OverlayFilter(halfTransparency) // ColorFilter <: Color

  // how can we create instance of opaque types + how to access their APIs
  // 1 - companion objects
  // 2 - extension methods
  val aName      = Name("Daniel")
  val nameLength = aName.length

  def main(args: Array[String]): Unit = {}
}
