package com.rockthejvm.part4context

object TypeClasses {

  /*
    Small library to serialize some data to a standard format (HTML)
   */
  // V!: the OO way
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/></div>"
  }

  val bob      = User("Bob", 43, "bob2@rockthejvm.com")
  val bob2html = bob.toHtml
  // same for other data structures that we want to serialize

  /*
    Drawbacks:
    - only available for the types WE write
    - can only provide ONE implementation
   */

  // V2: pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(name, age, email) =>
        s"<div>$name ($age yo) <a href=$email/></div>"
      case _ =>
        throw new IllegalArgumentException("data structure not supported")
    }
  }

  /*
    Drawbacks:
    - lost type safety
    - need to modify a SINGLE piece of code every time
    - still ONE implementation
   */

  // V3: type class
  // part 1 - type class definition
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  // part 2 - type class instances for the supported types
  given userSerializer: HTMLSerializer[User] with {
    def serialize(value: User): String = {
      val User(name, age, email) = value
      s"<div>$name ($age yo) <a href=$email/></div>"
    }
  }

  val bob2html_v2 = userSerializer.serialize(bob)

  /*
    Benefits:
    - can define serializers for other types OUTSIDE the "library"
    - multiple serializers for the same type, pick whichever you want
   */
  import java.util.Date
  given dateSerializer: HTMLSerializer[Date] with {
    override def serialize(date: Date): String =
      s"<div>${date.toString}</div>"
  }

  object SomeOtherSerializerFunctionality {
    given partialUserSerializer: HTMLSerializer[User] with {
      override def serialize(user: User): String = s"<div>${user.name}</div>"
    }
  }

  // part 3 - using the type class (user-facing API)
  object HTMLSerializer {
    def serialize[T](value: T)(using serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](using serializer: HTMLSerializer[T]): HTMLSerializer[T] =
      serializer
  }

  val bob2html_v3 = HTMLSerializer.serialize(bob)
  val bob2html_v4 = HTMLSerializer[User].serialize(bob)

  // part 4
  object HTMLSyntax {
    extension [T](value: T)
      def toHTML(using serializer: HTMLSerializer[T]): String =
        serializer.serialize(value)
  }

  import HTMLSyntax.*
  val bob2html_v5 = bob.toHTML

  /*
    Cool!
    - extend functionality to new types that we want to support
    - flexibility to add TC instances in a different place than defintion of TC
    - choose implementations (by importing the right givens)
    - super expressive! (via extension methods)
   */

  def main(args: Array[String]): Unit = {
    println(bob2html)
    println(bob2html_v2)
    println(bob2html_v3)
    println(bob2html_v4)
    println(bob2html_v5)
  }
}
