package oss.giussi.cappio.ui.levels

import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.api.L._
import org.scalajs.dom.html

object BroadcastIntro {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1(
      "Broadcast"
    ),
    p(
      "Se conoce como broadcast al mecanismo de comunicación utilizado por un proceso para enviar informacion a todos los procesos del sistema, incluyendose. En lugar de " +
        "la de tipo punto a punto que vimos anteriormente con las abstracciones de links, acá hablamos de comunicación de grupo."
    ),
    p(
      "Si bien este tipo de comunicacion parece ser extremadamente simple y tal vez pensemos en que no es necesario una abstraccion sino que podemos resolverlo " +
        "simplemente usando el ya visto Perfect Link, veremos que hay varias implementaciones de esta abstracción, las cuales varían en cuanto a las garantias " +
        "que proveen, como veremos más adelante."
    ),
    h3("¿Para qué sirve en el mundo real?"),
    p(
      "Para poder entender cómo podemos usar esta abstracción vamos a plantear su uso en el marco de una aplicación para almacenar álbumes de fotos. Para esto vamos a poder " +
        "enviar requests para crear un álbum, agregar o eliminar una foto de un álbum o eliminar todas las fotos. Este es un ejemplo muy acotado donde dejamos fuera otra abstracción " +
        "clave para el almacenamiento que los autores describen como memoria compartida, pero su análisis queda fuera del alcance de esta herramienta."
    ),
    p(
    "Debido a que somos muy optimistas con respecto al éxito de nuestra aplicación y esperamos un gran número de usuarios concurrentes decidimos diseñarla como una aplicación distribuida " +
      "lo que nos permite tener N instancias manejando pedidos para crear álbumes y agregar fotos."
    ),
    p("Dado que una solicitud puede llegar a cualquier instancia, necesitamos que todas ellas se enteren cuando un álbum es creado, es decir cuando una instancia recibe una solicitud " +
      "debe enviarla a todos los procesos del grupo, por lo tanto decidimos usar una abstracción de broadcast. En los siguientes niveles veremos cómo las distintas garantías provistas por " +
      "las distintas implementaciones impactan en nuestra aplicación."
    )

  )

  val beb: ReactiveHtmlElement[html.Div] = div(
    h1("Best effort broadcast"),
    p(
      "La forma más simple de broadcast es llamada Best Effort Broadcast y utiliza un Perfect Link para enviar el mensaje a cada uno de los procesos."
    ),
    p(
      "A continuacion se muestra una la estructura de capas de esta abstraccion y los mensajes que se intercambian entre capas."
    ),
    img(
      src := "img/cappio/bcast/beb.svg"
    ),
    h2("Interface"),
    img(
      widthAttr := 800,
      src := "img/cappio/interfaces/beb.png"
    ),
    h2("Algoritmo"),
    img(
      widthAttr := 600,
      src := "img/cappio/modules/beb.png"
    )
    /*
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
     */
  )

}
