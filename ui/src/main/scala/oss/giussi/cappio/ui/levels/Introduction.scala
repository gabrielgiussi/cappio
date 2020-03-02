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
    )

  )

}

object Book {

  val source = div(
    h1("En busca de un vocabulario común para sistemas distribuidos"),
    p(
      "Comprender e implementar sistemas distribuidos resulta una tarea difícil y la principal razón es que los problemas que intenta resolver son complejos. " +
        "Pero sumado a esto existe otro problema y radica en la falta de un vocabulario común que nos permita a nosotros, seamos científicos de computación o " +
        "desarrolladores de software, que estemos en la industria o en la campo de investigación científica, poder comunicarnos con nuestros pares o incluso leer " +
        "la documentación para entender las garantías que provee una base de datos o una cola de mensajes. Por ejemplo, cuando decimos consistencia, nos referimos a " +
        "la C del teorema CAP, a la C de ACID en el contexto de las bases de datos o la consistencia de las replicas en sistemas replicados asincrónicamente (Kleppmann)."
    ),
    p(
      "La falta de vocabulario no se explica porque los conceptos no estén correctamente definidos sino a la falta de divulgación y al mal uso de ciertos términos por " +
        " parte de la industria. CappIO es una pequeña herramienta didáctiva para aportar a la divulgación de este vocabulario y para esto propone una serie de niveles " +
        "en los cuales el usuario deberá interactuar con un conjunto de procesos que cooperan usando un algoritmo en común para alcanzar un objetivo definido. Esta " +
        "herramienta se basa en el excelente trabajo realizado por Christian Cachin, Rachid Guerraoui y Luis Rodrigues en \"Introdudcción a la programación distribuida " +
        "confiable y segura.\" Este libro es acerca de \"comprender las dificultades que subyacen a los problemas de consenso, total order broadcast y atommic commitment " +
        "y desarrollar abstracciones que encapsulen estos problemas.\"" // TODO End-to-end argument pag 7: donde ponerlo? ponerlo?
    ),
    h2("Introdudcción a la programación distribuida confiable y segura"),
    h5("Modelo de composición"),
    p(
      "En el libro se utiliza pseudocódigo para describer los algoritmos. El pseudocódigo refleja un modelo informático reactivo en el que los componentes del mismo " +
        "proceso se comunican mediante el intercambio de eventos: un algoritmo se describe como un conjunto de controladores de eventos. Estos reaccionan a los eventos " +
        "entrantes y posiblemente desencadenan nuevos eventos. " +
        "Se describen APIs y algoritmos utilizando un modelo de composición asíncrono basado en eventos. Cada proceso aloja un conjunto de componentes de software, " +
        "llamados módulos en nuestro contexto. Cada componente se identifica por un nombre y se caracteriza por un conjunto de propiedades. El componente proporciona una " +
        "interfaz en forma de eventos que el componente acepta y produce a cambio. Las abstracciones de programación distribuida generalmente están formadas por una " +
        "colección de componentes, al menos uno para cada proceso, que están destinados a satisfacer algunas propiedades comunes."
    ),
    h5("Pilas de componentes"),
    p(
      "Los componentes se pueden integrar para construir pilas de software. En cada proceso, un componente representa una capa específica en la pila. La capa de " +
        "aplicación está en la parte superior de la pila, mientras que la capa de red esta en la parte inferior. Las capas de las abstracciones de programación distribuida que " +
        "consideraremos están típicamente en el medio. Los componentes dentro de la misma pila se comunican a través del intercambio de eventos, como se ilustra en la siguiente imagen"
    ),
    p(
      "TODO figura 1.1"
    ),
    h5("Eventos"),
    p(
      "Cada evento es procesado a través de un controlador dedicado por el proceso (es decir, por el componente correspondiente). Un controlador se formula en términos de una secuencia " +
        "de instrucciones introducidas por el evento, que describe el evento, seguido de un pseudocódigo con instrucciones que se ejecutarán. El procesamiento de un evento puede dar " +
        "como resultado la creación de nuevos eventos y la activación de componentes iguales o diferentes. Cada evento desencadenado por un componente del mismo proceso se procesa " +
        "eventualmente, si el proceso es correcto. Los eventos del mismo componente se procesan en el orden en que se activaron. Este orden de primero en entrar, primero en salir (FIFO) " +
        "solo se aplica en eventos intercambiados entre componentes locales en una pila determinada. El modelo supone que cada proceso ejecuta el código desencadenado por eventos de una " +
        "manera mutuamente excluyente. Esto significa que el mismo proceso no maneja dos eventos simultáneamente. Una vez que finaliza el manejo de un evento, el proceso sigue comprobando " +
        "si se activa cualquier otro evento. Se supone que esta verificación periódica es justa y se logra de manera implícita: no es visible en el pseudocódigo."
    ),
    p("La manera desacoplada y asíncrona de interactuar entre componentes coincide muy bien con los requisitos de las aplicaciones distribuidas: por ejemplo, nuevos procesos pueden unirse " +
      "o abandonar el sistema distribuido en cualquier momento y un proceso debe estar listo para manejar los cambios de membresía y la recepción de mensajes en en cualquier momento. Por lo " +
      "tanto, el orden en que se observarán los eventos concurrentes no se puede definir a priori; esto es precisamente lo que se captura a través de nuestro modelo de componentes."),
    p(
      "TODO figura pagina 10, o agregar estilo para pseudocodigo"
    ),
    h5("Interfaz de programación"),
    p(
      "Las API de los componentes incluyen dos tipos de eventos, solicitudes e indicaciones:",
      ul(
        li(
          "Solicitudes (entradas): Un componente utiliza los eventos de solicitud para invocar un servicio en otro componente o para indicar una condición a otro componente. Por ejemplo, la capa " +
            "de aplicación podría desencadenar un evento de solicitud en un componente a cargo de transmitir un mensaje con cierta garantía de confiabilidad a los procesos en un grupo, " +
            "o proponer un valor que decidirá el grupo. Una solicitud también puede llevar información de señalización, por ejemplo, cuando el componente ha enviado previamente algunos " +
            "datos a la capa de aplicación y la solicitud confirma que la capa de aplicación ha procesado los datos. Desde la perspectiva del componente que maneja el evento, los eventos " +
            "de solicitud son entradas."
        ),
        li("Indicaciones (salidas): Los eventos de indicación son utilizados por un componente para entregar información o para señalar una condición a otro componente. Teniendo en cuenta el ejemplo " +
          "de difusión dado anteriormente, en cada proceso que es un destino del mensaje, el componente a cargo de implementar la primitiva de transmisión actual generalmente realizará un " +
          "procesamiento para garantizar la garantía de confiabilidad correspondiente, y luego usará un evento de indicación para entregar el mensaje a la capa de aplicación. Del mismo modo, " +
          "la decisión sobre un valor se indicará con dicho evento. Un evento de indicación también puede desempeñar el papel de una confirmación, por ejemplo, cuando el componente responsable " +
          "de la transmisión indica a la capa de aplicación que el mensaje fue efectivamente transmitido. Desde la perspectiva del componente que desencadena el evento, los eventos de indicación " +
          "son salidas")
      ),
      p(
        "TODO imagen 1.2 pag 12"
      ),
      // TODO aca puedo agregar el ejemplo de la pag 12 (a typical execution ...)
      // pag 13 explica modulos, interfaces y propiedades con un ejemplo: Job Handler
      h3(""),
      h5(""),
      // TODO aca puedo explicar alguna clase con un ejemplo, y explicar la relacion entre esto y el system model. Ejemplo: fail-stop solo puede darse en sync model porque tenes la abstraction PerfectFailureDetector
      h5("Clases de algoritmos"),
      p(
        "A la hora de desarrollar un algoritmo distribuido, las fallas que pueden ocurrir (o que deseamos considerar a la hora de desarrollar el algoritmo), el entorno, los parámetros del sistema y " +
          "otras opciones de diseño, determinan la clase de dicho algoritmo. El libro define las siguientes 6 clases: ",
        ul(
          li(
            "fail-stop: los procesos pueden fallar al fallar, pero todos los otros procesos pueden detectarlos de manera confiable"
          ),
          li(
            "fail-silent: la terminación de procesos nunca pueden detectarse de manera confiable"
          ),
          li(
            "fail-noisy: los procesos pueden fallar pero se pueden detectar dichas fallas, aunque no siempre de manera precisa (la precisión es eventual)"
          ),
          li(
            "fail-recovery: los procesos pueden fallar y luego recuperarse y aún participar en el algoritmo"
          ),
          li(
            // TODO explicar porque se conoce como bizantine, poniendo una referencia al paper two generals problem.
            "fail-arbitrary: los procesos pueden desviarse arbitrariamente de la especificación del protocolo y actuar de manera maliciosa y adversaria. Esta clase también se conoce como algoritmos bizantinos."
          ),
          li(
            "randomized: donde además de las clases presentadas hasta ahora, los procesos pueden tomar decisiones probabilísticas utilizando una fuente de aleatoriedad."
          ),
        ),
        p(
          "En CappIO solo vamos a ver algoritmos de clase fail-silent y fail-noisy."
        )
      ),

    )
    /*
    Motivation
    Inherent distribution (esto capaz puede ir en la intro a ds)
    Distribution as an artifact (aca hay algunas cosas que van en la intro)
    The end to end argument (aca explica la motivacion del modelo de composicion
    Composition model + Software stack
    Programming interface (requests, indication)
    Modules y event handler (o lo explica en el punto anterior?)
    Classes of algoritms (i need this to say which class we are going to cover)
    basic abstractions (processes, messages, process failures, links, link failures, time - system models)
    safety and liveness (lo tengo que explicar si voy a listar las propiedades de los algoritmos, tengo q epxlicar porque solo vamos a tratar con safety)
     */
  )
}
