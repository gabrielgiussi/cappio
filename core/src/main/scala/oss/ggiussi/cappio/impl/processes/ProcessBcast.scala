package oss.ggiussi.cappio.impl.processes

import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core._
import oss.ggiussi.cappio.impl.Instances
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol.{BrkBcast, BrkDeliver}
import oss.ggiussi.cappio.impl.links.Message
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash


case class ProcessBcast(id: ProcessID, neighbors: Set[ProcessID])(implicit payloads: Payloads) extends Automaton[ProcessState] {

  import Instances._

  val processID = InstanceID("process-bcast") // TODO

  override val sig: ActionSignature = {
    val messages = neighbors.flatMap(p => payloads.messages(id, p))
    val in: Set[Action] = {
      val delivers: Set[Action] = messages.map(m => BrkDeliver(m.id.from, m.id.to,BCAST, m))
      delivers + Crash(id, processID)
    }
    val out: Set[Action] = (for (p <- payloads.payloads; s <- 0 until payloads.steps) yield (p, s)).map { case (p, s) => BrkBcast(id,Instances.BCAST, p, s) }
    val int: Set[Action] = Set.empty
    ActionSignature(in = in, out = out, int = int)
  }
  override val steps: Steps.Steps[ProcessState] = Steps.steps {
    case BrkBcast(`id`, `BCAST`, payload, _) if payloads enabled payload => Effect.precondition(_.isInstanceOf[Up])
    case BrkDeliver(_, `id`, `BCAST`, Message(payload, _)) => Effect(_.isInstanceOf[Up], _ deliver payload) // TODO tal vez esta mal q no se pueda ejecutar la accion, sino que se deberi ejecutar y no hacer nada. Ver el paper q algo de esto dice.
    case Crash(`id`, `processID`) => Effect(_.isInstanceOf[Up], _ => Down)
  }

}
