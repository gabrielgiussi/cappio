package oss.giussi.cappio.ui.levels.bcast


import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebApp, BebBcast}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.BEBLevel.ModLevel
import oss.giussi.cappio.ui.levels.{AbstractLevel, Snapshot}
import oss.giussi.cappio._

object BEBLevel {

  type ModLevel = BebApp[String]

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.app[String](all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.Conditions._
  import oss.giussi.cappio.ui.core.LevelConditions._

  val simple = BEBLevel(
    div(
      p(
        "En este nivel vamos a ver la abstracción de broadcast. Es usada para diseminar información entre un conjunto de procesos y cada implementación difiere en las" +
          "garantías de entrega que proveen. En otras palabras, permite a un proceso mandar, a través de una única operación, un mensaje a todos los procesos del sistema" +
          "incluyéndose." +
          "La forma más simple de broadcast es llamada Best Effort Broadcast y utiliza un Perfect Link."
      ),
      img(
        src := "img/cappio/bcast/beb.svg"
      ),
      p(
        "El objetivo de este nivel es enviar un mensaje a todos los procesos usando broadcast."
      )
    ),
    List(
      ALL_UP[ModLevel],
      condition("Entregar [A] a todos", "El estado de todos los procesos debería contener el mensaje [A]", states[ModLevel](s => if (s.values.forall(_.value.contains("A"))) Successful else Error("Not all processes delivered 'A'")))
    )
  ) _

  val broken = {
    val sent = (s: Snapshot[ModLevel]) => s.step.scheduler.processes.filter(_._2.status == Up).values.flatMap(_.stack.state.module.state.module.state.module.state.sent).toSet
    val delivered = (s: Snapshot[ModLevel]) => s.step.scheduler.network.alreadyDelivered
    val c = (s: Snapshot[ModLevel]) => {
      val se = sent(s)
      val de = delivered(s)
      if ((se -- de).isEmpty) Successful else Error("You still have messages to deliver")
    }
    val p2 = ProcessId(2)
    BEBLevel(
      div(
        "En el nivel anterior vimos cómo podemos usar broadcast para enviar un mensaje a todos los procesos del sistema. Pero ¿podemos estar seguros de que" +
          "el mensaje se va a entregar a todos los procesos en todos los escenarios?. Best Effort Broadcast sólo asegura la entrega a todos los procesos correctos" +
          "en caso de que el procesos que inicio el broadcast no falle." +
          "En este nivel el objetivo es ver un escenario donde Best Effort Broadcast no es suficiente."
      ),
      List(
        condition("Process 2 UP", "Process 2 must be UP", process(p2)(p => if (p.status == Up) Successful else Error("Process 2 crashed"))),
        condition("Process 2 state must be 'A'", "", state[ModLevel](p2)(p => if (p.value.contains("A")) Successful else Error("Process 2 state is not 'A'"))),
        condition("Other processes state must be 'C'", "", states[ModLevel](p => if (p.filterKeys(_ != p2).values.map(_.value).forall(_.contains("C"))) Successful else Error("There is at least one process with an incorrect state"))),
        condition("Shouldn't be pending messages to deliver", "", c)
      )
    ) _
  }

}

// TODO replace this class by an apply method?
case class BEBLevel(shortDescription: Div, cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int) extends AbstractLevel[ModLevel](BEBLevel.scheduler(nProcesses,timeout), cond) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override val indicationPayload = ind => ind.payload.msg.toString

}
