package oss.giussi.cappio.ui

import oss.giussi.cappio.{ProcessId, Processes}
import oss.giussi.cappio.ui.core.Index

case class Point(x: Double, y: Double) {

  def adjustX(adjust: Double) = copy(x + adjust, y)

}

case class GridConfImpl(baseRoundWidth: Double, p: Processes) extends GridConf {

  val widthAdjustment = 10

  override val roundWidth = baseRoundWidth + ((p.all.size - 1) * widthAdjustment)

  override val roundHeight = roundWidth * 1.2

  override val arrowHeadSize: Double = roundWidth * 0.15

  private val indexes = processes.zipWithIndex.toMap

  override def point(index: Index, process: ProcessId): Point = Point(x(index,process).toInt + (roundWidth / 2),y(process))

  override def y(process: ProcessId): Double = (indexes(process) + 1) * roundHeight

  override def x(index: Index, process: ProcessId): Double = (index.i * roundWidth) + (process.id * widthAdjustment)

  override def crossSize: Double = roundWidth * 0.2

  override def pointSize: Double = roundWidth * 0.2

  override def processes: List[ProcessId] = p.all

}

sealed trait GridConf {

  def roundWidth: Double

  def roundHeight: Double

  def arrowHeadSize: Double

  def crossSize: Double

  def point(index: Index, process: ProcessId): Point

  def y(process: ProcessId): Double

  def x(index: Index, process: ProcessId): Double

  def processes: List[ProcessId]

  def pointSize: Double


}