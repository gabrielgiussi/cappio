package oss.giussi

package object cappio {

  type ¬[A] = A => Nothing

  type v[T, U] = ¬[¬[T] with ¬[U]]

  type ¬¬[A] = ¬[¬[A]]
  type |-|[T, U] = { type λ[X] = ¬¬[X] <:< (T v U) }


  case class Tick()

  case class ProcessId(id: Int)

  sealed trait ProcessStatus

  case object Up extends ProcessStatus

  case object Down extends ProcessStatus

  case class Instance(name: String)

  type NoState = Unit

  type NoRequest = Unit

}