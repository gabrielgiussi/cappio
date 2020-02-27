package oss.giussi.cappio.ui.levels

import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.api.L._
import org.scalajs.dom.html

object Introduction {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1("Introduccion"),
    p(
      ""
    ),
    p(

    ),
  )

  val cappio = div(

  )
}

object CappIO {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1(
      "CappIO"
    ),
    p(
      "El libro define algoritmos en pseudocódigo que reflejan un modelo reactivo en el que los componentes del mismo proceso se comunican mediante " +
        "el intercambio de eventos: un algoritmo\nse describe como un conjunto de controladores de eventos. Estos reaccionan a los eventos entrantes y " +
        "posiblemente disparan nuevos eventos.\nCada componente proporciona una interfaz en forma de eventos que el componente acepta y produce. " +
        "Las abstracciones de programación distribuida generalmente están formadas\npor una colección de componentes, al menos uno para cada proceso, " +
        "que tienen el objetivo de satisfacer algunas propiedades comunes.\nLos componentes pueden ser compuestos para construir software stacks. " +
        "En cada proceso, un componente representa una capa específica del stack. La capa de aplicación está en la\nparte superior de la pila, " +
        "mientras que la capa de red suele estar en la parte inferior. Las capas de las abstracciones de programación distribuida que consideraremos " +
        "están\ntípicamente en el medio. Los componentes dentro de la misma pila se comunican a través del intercambio de eventos."
    ),
    p(
      "Según este modelo, cada componente se construye como una máquina de estado cuyas transiciones se desencadenan por la recepción de eventos. " +
        "Los eventos pueden llevar información\ncomo un mensaje de datos o información de group membership.\nCada evento es procesado a través de un " +
        "handler dedicado por el proceso (es decir, por el componente correspondiente). Un handler se formula en términos de una secuencia de " +
        "instrucciones.\nEl procesamiento de un evento puede dar como resultado la creación de nuevos eventos. Cada evento desencadenado por " +
        "un componente del mismo proceso eventualmente se procesa, si el proceso es\ncorrecto. Los eventos del mismo componente se procesan " +
        "en el orden en que se activaron. Este orden first in first out (FIFO) solo se aplica en eventos intercambiados\nentre componentes " +
        "locales de un stack."
    )

  )

}
