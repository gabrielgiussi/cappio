package oss.ggiussi.cappio.ui.app2

import org.scalajs.dom.document
import oss.ggiussi.cappio.ui.app2.GridComponent.{GridConf, GridProps}

object App2 {

  def main(args: Array[String]): Unit = {

    GridComponent.Component(GridProps(Set((1,Some(2)), (2,None), (3,None), (4,Some(4))), GridConf(50, 50, 40, 6, 30))).renderIntoDOM(document.getElementById("playground"))
  }

}
