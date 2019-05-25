package oss.giussi.cappio.impl

import scala.reflect.ClassTag

/*
https://medium.com/anyjunk/union-types-ffde15fd335
 */

object Union {
  type OneOf2[H <: Union] = { type l[T] = OneOf[T, H] }
}

sealed trait Union
// Analagous to HCons and ::
trait :|:[+H, +T <: Union] extends Union
// Analagous to HNil
sealed trait UNil extends Union

trait OneOf[T, U <: Union]
object OneOf {
  // If T is a member of the union U then we know T is ‘one of’ U, by our definition of what ‘OneOf’ means
  implicit def fromMemberProof[U <: Union, T](implicit s: T MemberOf U): T OneOf U = null
  // If we have evidence OneOf[T, Ev] it means T is a member of Ev, and if we know Ev
  // is a subunion of Target, then we can deduce T is also one of Target
  implicit def nestedOneOfProof[T, Ev <: Union, Target <: Union](
                                                                  implicit ev: T OneOf Ev,
                                                                  sub: Ev SubUnionOf Target
                                                                ): T OneOf Target = null
}


sealed trait MemberOf[T, U <: Union]
object MemberOf {
  // If H is the first type in an Union U, we trivially know that H is in U
  implicit def baseCase[H, U <: Union]: MemberOf[H, H :|: U] = null
  // If T is in the union U, we know T is in the Union H:|:U for any H
  implicit def recursiveCase[H, U <: Union, T](implicit member: T MemberOf U): T MemberOf (H :|: U) = null
}

trait SubUnionOf[U <: Union, T <: Union]
object SubUnionOf {
  // If U1 is a member of U, then U1 :|: UNil is most definitely a subunion of U
  implicit def fromMemberCase[U1, U <: Union](implicit s: U1 MemberOf U): SubUnionOf[U1 :|: UNil, U] = null
  // If T1 is a member of U, and T is a subunion of U, then T1 :|: T is a subunion of U
  implicit def recursiveCase[U <: Union, T1, T <: Union](
                                                          implicit mem: T1 MemberOf U,
                                                          sub: T SubUnionOf U
                                                        ): (T1 :|: T) SubUnionOf U = null
}

/*
trait Pro[A,B] {
  def p[T](a: T)(implicit clazz: ClassTag[T], ev: OneOf[T, A :|: B :|: UNil]): T = a match {
    case i: A if clazz.runtimeClass. => i
    case b: B => b
  }
}

 */

object Prueba extends App {

  type Un[T] = OneOf[T, Int :|: Boolean :|: String :|: UNil]

  def foo[T](union: T)(implicit unionEv: Un[T]): String = union match {
    case i: Int => i.toString
    case b: Boolean => b.toString
    case s: String => s
  }


  println(foo(1))
  println(foo(true))
}

// https://github.com/krzemin/octopus
// https://blog.softwaremill.com/are-you-sure-your-anyvals-dont-instantiate-5822c04875d3