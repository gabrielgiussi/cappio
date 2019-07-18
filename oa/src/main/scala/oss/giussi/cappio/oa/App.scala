package oss.giussi.cappio.oa

import oss.giussi.scalorm.{Scalorm, Scorm}

object App {

  def main(args: Array[String]): Unit = {
    if (Scalorm.initialize()){
      println(Scalorm.getStudentName)
      //println(Level.level.map(_ * 2))
    }
    else {
      println("No API found")
    }
  }

}
