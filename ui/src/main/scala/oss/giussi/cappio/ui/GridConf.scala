package oss.giussi.cappio.ui

import oss.giussi.cappio.{ProcessId, Processes}
import oss.giussi.cappio.ui.core.Index

case class Point(x: Double, y: Double)

case class GridConfImpl(labelWidth: Int, roundWidth: Double, arrowHeadSize: Double, p: Processes) extends GridConf {

  override val roundHeight = roundWidth * 2

  val sorted = p.ids.toList.sortBy(_.id)

  private val indexes = sorted.zipWithIndex.toMap

  override def point(index: Index, process: ProcessId): Point = Point((index.i * roundWidth).toInt, y(process))

  override def y(process: ProcessId): Double = (indexes(process) + 1) * roundHeight

  override def crossSize: Double = roundWidth * 0.5

  override def processes: List[ProcessId] = sorted
}

sealed trait GridConf {

  def labelWidth: Int

  def roundWidth: Double

  def roundHeight: Double

  def arrowHeadSize: Double

  def crossSize: Double

  def point(index: Index, process: ProcessId): Point

  def y(process: ProcessId): Double

  def processes: List[ProcessId]

}