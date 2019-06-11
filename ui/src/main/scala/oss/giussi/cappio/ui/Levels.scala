package oss.giussi.cappio.ui

import oss.giussi.cappio.{ProcessId, Processes}

object Levels {

  val level1 = Level(1, Processes(Set(ProcessId(0),ProcessId(1),ProcessId(2))))

  val level2 = Level(2, Processes((0 to 10).map(ProcessId).toSet))

  val levels = List(level1,level2)

}

case class Level(x: Int, processes: Processes)