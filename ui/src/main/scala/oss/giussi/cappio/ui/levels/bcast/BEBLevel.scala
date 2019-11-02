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

  val simple = BEBLevel(List(
    ALL_UP[ModLevel],
    condition("Deliver to all", "The state of all processes should be A",states[ModLevel](s => if (s.values.forall(_.value.contains("A"))) Successful else Error("Not all processes delivered 'A'")))
  )) _

  val broken = {
    val sent = (s: Snapshot[ModLevel]) => s.step.scheduler.processes.filter(_._2.status == Up).values.flatMap(_.stack.state.module.state.module.state.module.state.sent).toSet
    val delivered = (s: Snapshot[ModLevel]) => s.step.scheduler.network.alreadyDelivered
    val c = (s: Snapshot[ModLevel]) => {
      val se = sent(s)
      val de = delivered(s)
      if ((se -- de).isEmpty) Successful else Error("You still have messages to deliver")
    }
    val p2 = ProcessId(2)
    BEBLevel(List(
      condition("Process 2 UP", "Process 2 must be UP", process(p2)(p => if (p.status == Up) Successful else Error("Process 2 crashed"))),
      condition("Process 2 state must be 'A'", "", state[ModLevel](p2)(p => if (p.value.contains("A")) Successful else Error("Process 2 state is not 'A'"))),
      condition("Other processes state must be 'C'", "", states[ModLevel](p => if (p.filterKeys(_ != p2).values.map(_.value).forall(_.contains("C"))) Successful else Error("There is at least one process with an incorrect state"))),
      condition("Shouldn't be pending messages to deliver", "", c)
    )) _
  }

}

// TODO replace this class by an apply method?
case class BEBLevel(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int) extends AbstractLevel[ModLevel](BEBLevel.scheduler(nProcesses,timeout), cond) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override val indicationPayload = ind => ind.payload.msg.toString

  override val shortDescription = div(
    "Este es el nivel de beb"
  )

}
