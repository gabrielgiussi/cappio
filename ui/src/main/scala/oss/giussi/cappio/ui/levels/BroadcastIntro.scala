package oss.giussi.cappio.ui.levels

import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.api.L._
import org.scalajs.dom.html

object BroadcastIntro {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1(
      "Broadcast"
    ),
    h3(
      "Se conoce como broadcast al mecanismo de comunicación utilizado por un proceso para enviar informacion a todos los procesos del sistema, incluyendose."
    ),
    p(
      "Si bien este tipo de comunicacion parece ser extremadamente simple y tal vez pensemos en que no es necesario una abstraccion sino que podemos resolverlo " +
        "simplemente usando el ya visto Perfect Link, veremos que hay varias implementaciones de esta abstracción, las cuales varían en cuanto a las garantias " +
        "que proveen, como veremos más adelante."
    )

  )

  val beb: ReactiveHtmlElement[html.Div] = div(
    h1("Best effor broadcast"),
    p(
      "La forma más simple de broadcast es llamada Best Effort Broadcast y utiliza un Perfect Link para enviar el mensaje a cada uno de los procesos."
    ),
    p(
      "A continuacion se muestra una la estructura de capas de esta abstraccion y los mensajes que se intercambian entre capas."
    ),
    img(
      src := "img/cappio/bcast/beb.svg"
    ),
    h1("Propiedades"),
    p(
      ul(
        li(
          "BEB1: Validez: Si un proceso correcto hace broadcast de un mensaje m, entonces m se entrega eventualmente a todos los procesos correctos."
        ),
        li(
          "BEB2: No duplicación: Ninguún mensaje es entregado más de una vez."
        ),
        li(
          "BEB3: No creación: Si un proceso entrega un mensaje m enviado por s, entonces s previamente hizo broadcast de m."
        )
      )
    ),
  )

}
