package oss.giussi.cappio.ui

import oss.giussi.cappio.{ProcessId, Processes}
import oss.giussi.cappio.ui.SampleMain.Point
import oss.giussi.cappio.ui.core.Index

case class GridConf(labelWidth: Int, roundWidth: Double, roundHeight: Double, arrowHeadSize: Double, processes: Processes) {

  val order = processes.ids.toSeq.sortBy(_.id).zipWithIndex

  val map = order.toMap
  // No deberia usar el id del process directamente, sino un mapa del propio grid conf donde asigne posiciones a cada process id.
  // para que no depende de si empiezo por el 0 o por el 1
  def p(index: Index, process: ProcessId): Point = Point((index.i * roundWidth).toInt, (map(process) + 1) * roundHeight.toInt)

  def py(processId: ProcessId) = p(Index(0),processId).y
}
