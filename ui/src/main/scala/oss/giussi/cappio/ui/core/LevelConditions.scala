package oss.giussi.cappio.ui.core

import oss.giussi.cappio.Conditions.{IndicationValidation, NetworkValidation, ProcessesValidation}
import oss.giussi.cappio.ui.levels.Snapshot
import oss.giussi.cappio.{Mod, Scheduler}

object LevelConditions {

  def processes[M <: Mod](v: ProcessesValidation[M])(snapshot: Snapshot[M]) = v(snapshot.step.scheduler.processes.values.toSet)

  def network[M <: Mod](v: NetworkValidation[M#Payload])(snapshot: Snapshot[M]) = v(snapshot.step.scheduler.network)

  def indications[M <: Mod](v: IndicationValidation[M#Ind])(snapshot: Snapshot[M]) = v(snapshot.indications)

}
