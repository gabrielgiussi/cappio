package oss.ggiussi

package object cappio {

  type ProcessID = Int

  case class InstanceID(id: String)

  case class Processes(p: Set[ProcessID]) {
    def neighbors(id: ProcessID) = p - id // TODO salvo que use topologias donde no todos los nodos esten conectados con todos los otros nodos.
  }

}
