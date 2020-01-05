package oss.giussi

package object cappio {

  /*
  type ¬[A] = A => Nothing

  type v[T, U] = ¬[¬[T] with ¬[U]]

  type ¬¬[A] = ¬[¬[A]]
  type |-|[T, U] = { type λ[X] = ¬¬[X] <:< (T v U) }
   */

  //case class Tick()

  case class ProcessId(id: Int) {
    override def toString: String = s"P$id"
  }

  case class Processes(ids: Set[ProcessId]) {
    val all = ids.toList.sortBy(_.id)
  }

  sealed trait ProcessStatus

  case object Up extends ProcessStatus

  case object Down extends ProcessStatus

  object Instance {
    def ANY = Instance("any")
  }

  case class Instance(name: String)

  case class NoState(name: String)

  type NoRequest = Unit

}
