package oss.giussi.cappio.ui.levels.bcast


import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebApp, BebBcast}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.BEBLevel.ModLevel
import oss.giussi.cappio.ui.levels.{AbstractLevel, Snapshot}
import oss.giussi.cappio.{Process, ProcessId, Scheduler, Up}

object BEBLevel {

  type ModLevel = BebApp[String]

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.app[String](all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.Conditions._
  import oss.giussi.cappio.ui.core.LevelConditions._

  // TODO porque no infiere ningun tipo cuando armo la lista de conditions?
  val simple = BEBLevel(List(
    processes(ALL_UP[ModLevel]),
    states[ModLevel](condition[Map[ProcessId,ModLevel#State]]("Deliver to all", "", s => if (s.values.forall(_.value.contains("A"))) None else Some("Not all processes delivered 'A'")))
  )) _

  val broken = {
    val sent = (s: Snapshot[ModLevel]) => s.step.scheduler.processes.filter(_._2.status == Up).values.flatMap(_.stack.state.module.state.module.state.module.state.sent).toSet
    val delivered = (s: Snapshot[ModLevel]) => s.step.scheduler.network.alreadyDelivered
    val c = (s: Snapshot[ModLevel]) => {
      val se = sent(s)
      val de = delivered(s)
      if ((se -- de).isEmpty) None else Some("You still have messages to deliver")
    }
    val p2 = ProcessId(2)
    BEBLevel(List(
      process(p2)(condition[Process[ModLevel]]("Process 2 UP", "Process 2 must be UP", p => if (p.status == Up) None else Some("Process 2 crashed"))),
      state[ModLevel](p2)(condition[ModLevel#State]("Process 2 state must be 'A'", "", p => if (p.value.contains("A")) None else Some("Process 2 state is not 'A'"))),
      states[ModLevel](condition[Map[ProcessId, ModLevel#State]]("Other processes state must be 'C'", "", p => if (p.filterKeys(_ != p2).values.map(_.value).forall(_.contains("C"))) None else Some("There is at least one process with an incorrect state"))),
      condition[Snapshot[ModLevel]]("Shouldn't be pending messages to deliver", "", c)
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

}
