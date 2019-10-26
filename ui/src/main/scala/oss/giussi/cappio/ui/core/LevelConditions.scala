package oss.giussi.cappio.ui.core

import oss.giussi.cappio
import oss.giussi.cappio.Conditions.{IndicationValidation, NetworkValidation, ProcessValidation, ProcessesValidation, StateValidation, StatesValidation}
import oss.giussi.cappio.ui.levels.Snapshot
import oss.giussi.cappio.{Mod, ProcessId, Scheduler}

object LevelConditions {

  private object Projections {

    def processes[M <: Mod] = (s: Snapshot[M]) => s.step.scheduler.processes.values.toSet

    def process[M <: Mod](id: ProcessId) = processes[M].andThen(_.find(_.id == id).get)

    def network[M <: Mod] = (s: Snapshot[M]) => s.step.scheduler.network

    def indications[M <: Mod] = (s: Snapshot[M]) => s.indications

    def states[M <: Mod] = processes[M].andThen(_.map(p => p.id -> p.stack.state).toMap)

    def state[M <: Mod](id: ProcessId) = states[M].andThen(_(id))
  }

  def process[M <: Mod](id: ProcessId)(v: ProcessValidation[M]) = Projections.process(id).andThen(v)

  def processes[M <: Mod](v: ProcessesValidation[M]) = Projections.processes.andThen(v)

  def network[M <: Mod](v: NetworkValidation[M#Payload]) = Projections.network.andThen(v)

  def indications[M <: Mod](v: IndicationValidation[M#Ind]) = Projections.indications.andThen(v)

  def states[M <: Mod](v: StatesValidation[M]) = Projections.states.andThen(v)

  def state[M <: Mod](id: ProcessId)(v: StateValidation[M]) = Projections.state(id).andThen(v)


}