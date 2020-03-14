package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLModule, PLSend}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.{ProcessId, ProcessRequest, Scheduler}

object DemoLevel {

  import oss.giussi.cappio.ui.core.LevelConditions._

  type ModLevel = AppMod2[String,PLModule[String]]

  val selection = ActionSelection.fromToPayloadRequest("Enviar"){ case (from,to,payload) => PLSend.external(from,to,payload) } _

  val conditions: Conditions[ModLevel] = List((0,"C"), (1, "A"), (2, "B"))
    .map { case (id,expected) => processState[String, ModLevel](expected,_.state.getOrElse("-"),implicitly)(ProcessId(id)) } :+ noPendingMessages[ModLevel](_.module.state.module.state.sent)

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val app = AppState.app2[String,PLModule[String]](PerfectLink[String](timeout), (state, ind) => Some(ind.packet.payload))
    Scheduler.init(all,_ => app)
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.packet.payload
    override val reqTypes = List(
      selection,
      ActionSelection.crash
    )

    // TODO deberia incorporar imagenes con cada uno de los componentes que esto y describiendo ?
    override val shortDescription = div(
      p(
        "Este nivel utiliza las abstracciones más básicas para poder comunicar procesos punto a punto, por lo tanto no vamos a encontrar ninguna novedad ya que se trata de " +
          "un modo de comunicación básico, sin embargo nos va a servir para entender cómo funciona esta herramienta didáctica"
      )
    )

    override def title: String = "Demo"

    override def predefined: Set[PredefinedAction[PLSend[String]]] = Set(
      PredefinedAction(Index(1), ProcessId(0), ProcessRequest(ProcessId(0), PLSend.external(ProcessId(0),ProcessId(1),"A"),true)),
      PredefinedAction(Index(3), ProcessId(1), ProcessRequest(ProcessId(1), PLSend.external(ProcessId(1),ProcessId(2),"B"),true)),
      PredefinedAction(Index(5), ProcessId(2), ProcessRequest(ProcessId(2), PLSend.external(ProcessId(2),ProcessId(0),"C"),true))
    )

  }

  val good = DemoLevel(conditions) _
}
