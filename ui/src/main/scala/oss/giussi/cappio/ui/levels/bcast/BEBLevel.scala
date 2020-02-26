package oss.giussi.cappio.ui.levels.bcast


import com.raquo.laminar.api.L._
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebApp, BebBcast}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.BEBLevel.ModLevel
import oss.giussi.cappio.ui.levels.{AbstractLevel, PredefinedAction, Snapshot}

object BEBLevel {

  type ModLevel = BebApp[String]

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.app[String](all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.ui.core.LevelConditions._

  val ok = BEBLevel(
    "Best effort 1",
    div(
      h1("Usando Best Effort Broadcast"),
      p(
        "En este nivel vamos a ver la abstracción de broadcast. Es usada para diseminar información entre un conjunto de procesos y cada implementación difiere en las " +
          "garantías de entrega que proveen. En otras palabras, permite a un proceso mandar, a través de una única operación, un mensaje a todos los procesos del sistema " +
          "incluyéndose."
      ),
      p(
          "La forma más simple de broadcast es llamada Best Effort Broadcast y utiliza un Perfect Link para enviar el mensaje a cada uno de los procesos."
      ),
      p(
        "A continuacion se muestra una la estructura de capas de esta abstraccion y los mensajes que se intercambian entre capas."
      ),
      img(
        src := "img/cappio/bcast/beb.svg"
      ),
      h1("Objetivos"),
      p(
        "El objetivo de este nivel es asegurar que todos los procesos reciban todos los mensajes enviados por el proceso 0 en el mismo orden"
      )
    ),
    List(0,1,2).map(ProcessId).map(processState[List[String],ModLevel](List("A", "B", "C"),_.state.getOrElse(List.empty),implicitly))
  ) _

  val ko = {
    BEBLevel(
      "Best effort 2",
      div(
        h1("Rompiendo Best Effort Broadcast"),
        p(
        "En el nivel anterior vimos cómo podemos usar broadcast para enviar un mensaje a todos los procesos del sistema. Pero ¿podemos estar seguros de que " +
          "el mensaje se va a entregar a todos los procesos en todos los casos?. Best Effort Broadcast sólo asegura la entrega a todos los procesos correctos " +
          "en caso de que el procesos que inicio el broadcast no falle." +
          "En este nivel el objetivo es ver un escenario donde Best Effort Broadcast no es suficiente."
        ),
        h1("Objetivos"),
        p(
          "El objetivo de este nivel es hallar un escenario donde uno de los procesos no recibe todos los mensajes, haciendo que los procesos terminen con distintos estados"
        )
      ),
      List((1,List("A", "B")),(2,List("A", "B", "C"))).map {
        case (id,expected) => processState[List[String], ModLevel](expected, _.state.getOrElse(List.empty), implicitly)(ProcessId(id))
      } :+ noPendingMessages[ModLevel](_.module.state.module.state.module.state.sent)
    ) _
  }

}

// TODO replace this class by an apply method?
case class BEBLevel(title: String, shortDescription: Div, cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int) extends AbstractLevel[ModLevel](BEBLevel.scheduler(nProcesses,timeout), cond) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override val indicationPayload = ind => ind.payload.msg.toString

  override def requestPayload(req: BebBcast[String]): (String, String) = ("bcast",req.payload.msg)

  override def predefined: Set[PredefinedAction[Req]] = Set(
    PredefinedAction(Index(1),ProcessId(0),ProcessRequest.predefined(ProcessId(0), BebBcast("A"))),
    PredefinedAction(Index(3),ProcessId(0),ProcessRequest.predefined(ProcessId(0), BebBcast("B"))),
    PredefinedAction(Index(5),ProcessId(0),ProcessRequest.predefined(ProcessId(0), BebBcast("C")))
  )
}
