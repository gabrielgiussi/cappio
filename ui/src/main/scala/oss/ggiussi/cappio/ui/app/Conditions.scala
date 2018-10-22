package oss.ggiussi.cappio.ui.app

import oss.ggiussi.cappio.core.Composer._
import oss.ggiussi.cappio.core.StateCondition
import oss.ggiussi.cappio.impl.processes.{ProcessState, Up}

// TODO move to core
object Conditions {

  def allProcessesUp[S](f: S => List[ProcessState]) =  StateCondition("All processes should be Up", (s: S) => f(s).forall {
    case Up(_) => true
    case _ => false
  })

  implicit def asList[S](t: (S,S)): List[S] = List(t._1,t._2)
  implicit def asList[S](t: STuple3[S]): List[S] = List(t._1,t._2,t._3)
  implicit def asList[S](t: STuple4[S]): List[S] = List(t._1,t._2,t._3,t._4)
  implicit def asList[S](t: STuple5[S]): List[S] = List(t._1,t._2,t._3,t._4,t._5)

}
