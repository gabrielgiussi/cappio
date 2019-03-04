package oss.ggiussi.cappio.ui

import oss.ggiussi.cappio.ui.app.App.LevelPage

import scala.scalajs.js.annotation.JSExport


package object levels {

  @JSExport
  val levels = List(
    Level1.a
    //Level2.level,
    //Level3.level
  ).zipWithIndex
    //.map { case (level,i) => (level,i + 1)}
    .map { case (level,i) => (LevelPage(i + 1), level)}
}
