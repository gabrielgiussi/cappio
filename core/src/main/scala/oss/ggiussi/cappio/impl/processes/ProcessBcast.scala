package oss.ggiussi.cappio.impl.processes
import oss.ggiussi.cappio.core.LinkProtocol.Message
import oss.ggiussi.cappio.{InstanceID, ProcessID, Processes}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol.{BrkBcast, BrkBcastHeader, BrkDeliver, BrkDeliverHeader}
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash


object ProcessBcast {
  val instance = InstanceID("process-bcast")
}

case class ProcessBcast(id: ProcessID)(implicit processes: Processes) extends Automaton[ProcessState] {

  import Instances._



  val neighbors = processes.neighbors(id)

  override val sig: ActionSignature = {
    val in: Set[ActionHeader] = processes.p.map(BrkDeliverHeader(_,id,BCAST)) ++ Set(Crash(id, ProcessBcast.instance))
    val out: Set[ActionHeader] = Set(BrkBcastHeader(id,Instances.BCAST))
    val int: Set[ActionHeader] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps2(sig, {
    case _:BrkBcast => Effect.precondition(_.isInstanceOf[Up])
    case BrkDeliver(_,Message(payload,_)) => Effect(_.isInstanceOf[Up], _ deliver payload) // TODO tal vez esta mal q no se pueda ejecutar la accion, sino que se deberi ejecutar y no hacer nada. Ver el paper q algo de esto dice.
    case _: Crash => Effect(_.isInstanceOf[Up], _ => Down)
  })

}