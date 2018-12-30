package oss.ggiussi.cappio.impl

import oss.ggiussi.cappio.core.Action

object Triggers {

  def init(): Triggers = Triggers(Set.empty)

}

case class Triggers(triggered: Set[Action]) {

  // TODO improve naming
  def wasTriggered(action: Action): Boolean = triggered contains action

  def trigger(action: Action): Triggers = trigger(Set(action))

  def trigger(actions: Set[Action]): Triggers = copy(triggered = triggered ++ actions)

  def markAsTriggered(action: Action): Triggers = if (wasTriggered(action)) copy(triggered = triggered - action) else throw new IllegalStateException(s"Action $action wasn't triggerd")

}
