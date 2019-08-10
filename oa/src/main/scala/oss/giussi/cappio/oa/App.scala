package oss.giussi.cappio.oa

import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.{Event, document}
import oss.giussi.cappio.ui.levels.{LevelId, LevelPassed, Levels}
import oss.giussi.scalorm.Scalorm
import oss.giussi.scalorm.model.Passed

import scala.util.Try

object App {

  var initalized = false
  var finished = false

  def finishScorm(event: Event): Unit = {
    if (initalized && !finished) {
      Scalorm.commit()
      Scalorm.finish()
    }
  }

  def main(args: Array[String]): Unit = try {
    if (Scalorm.initialize()) {
      initalized = true
      dom.window.onunload = finishScorm
      dom.window.onbeforeunload = finishScorm
      println(Scalorm.getStudentName)
      val level = LevelId(Try(Scalorm.getLaunchData.toInt).getOrElse(1) - 1)
      documentEvents.onDomContentLoaded.foreach { _ =>
        def getElementById(id: String, clean: Boolean = false) = {
          val container = document.getElementById(id)
          if (clean) container.textContent = ""
          container
        }

        render(getElementById("main-container", true), Levels.INDEXED_LEVELS(level).s.render)


      }(unsafeWindowOwner)
      Levels.$pendingLevels.foreach(_(level) match {
        case LevelPassed =>
          Scalorm.setLessonStatus(Passed)
          Scalorm.commit()
        case _ =>
      })(unsafeWindowOwner)
    }
    else {
      println("No API found")
    }
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      throw e
  }

}