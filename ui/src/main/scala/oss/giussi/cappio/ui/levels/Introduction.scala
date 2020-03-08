package oss.giussi.cappio.ui.levels

import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.api.L._
import org.scalajs.dom.html

object Introduction {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1("Introducción"),
    p(
      ""
    ),
    p(

    ),
  )

}

object CappIO {

  val source: ReactiveHtmlElement[html.Div] = div(
    h1(
      "CappIO"
    ),
    h2("¿Por qué CappIO?"),
    p("Los algoritmos distribuidos pueden ser muy complejos, sobre todo si consideramos no sólo el problema teórico, por ejemplo alcanzar consenso entre procesos distribuidos que se comunican " +
      "sobre una red que puede perder o reordenar paquetes, sino también restricciones de performance, que generalmente se miden en cantidad de mensajes y de pasos requeridos, para que " +
      "la solución pueda tener usos prácticos en la industria, de nada sirve un algoritmo de consenso que funciona perfectamente pero demora horas en alcanzar un resultado."),
    p("Además de la complejidad intrínseca de estos algoritmos, las soluciones se encuentran muchas veces descritas en bibliografía producida por el campo de investigación que puede resultar " +
      "difícil de comprender para desarrolladores e ingenieros. Por todo esto se desarrolló esta herramienta con el fin de mostrar de manera gráfica como trabajan estos algoritmos, pudiendo ver" +
      "los mensajes que intercambian los procesos y cómo este afecta su estado a través del tiempo, utilizando una representación informal que se suele ver con frecuencia en libros y papers."),
    p("Para comprender los elementos básicos de los sistemas distribuidos vamos a analizar algoritmos de broadcast poniendo especial énfasis en el orden de entrega de los mensajes."), // TODO poner que system model vamos a usar, pero si antes los explique

    /*
    h2("¿Cómo funciona CappIO?"),
    p(
      "Cada nivel empieza desde el paso 0 y nuestra tarea es elegir las acciones que se van a ejecutar en cada proceso en cada paso hasta cumplir con los objetivos " +
        "propuestos por el nivel que se mostrarán en la lista de objetivos como explicamos anteriormente."
    ),
    h4("¿Qué es un paso?"),
    p(
      "El libro plantea"
    ),
     */
    div(
      h2("Entendiendo la interfaz gráfica"),
      p(
        "Esta se compone de 4 elementos",
        ul(
          li(
            h3("El diagrama de intercambio de mensajes"),
            p(
              "Este es el elemento principal y muestra las líneas de tiempo de todos los procesos del sistema, para los niveles generalmente " +
                "usaremos 3 procesos ya que son suficientes para entender los algoritmos sin tener una gran cantidad de mensajes con los cuales lidiar. A medida que interactuemos " +
                "con la herramienta enviando requests, descartando mensajes o terminando la ejecución de procesos, estos eventos se irán dibujando en este diagrama para que podamos " +
                "tener una vista completa de alto nivel de cómo interactuan los procesos."
            ),
            img(
              widthAttr := 400,
              src := "img/cappio/demo/diagram.png"
            ),
          ),
          li(
            h3("La lista de objetivos"),
            p(
              "Acá se listan todos los objetivos que se deben cumplir para pasar al siguiente nivel. El icono en rojo muestra que el objetivo todavía no se " +
                "cumplió y que se necesitan más interacciones para lograrlo, por ejemplo uno de los objetivos de este nivel evalua el estado interno de los procesos que se actualiza " +
                "cada vez que recibe un mensaje de otro proceso. En este caso nos referimos al estado de la capa que se encuentra más arriba que representa la aplicación que utiliza las " +
                "abstracciones discutidas en el libro."
            ),
            img(
              widthAttr := 400,
              src := "img/cappio/demo/conditions.png"
            ),
          ),
          li(
            h3("La selección de acciones"),
            p(
              "Este componente se puede encontrar en dos estados",
            ),
            ul(
              li(
                h5("Selección de requests"),
                p(
                  "En este estado podemos elegir terminar la ejecución de un proceso y además veremos las acciones predeterminadas que se enviaran en este paso."
                ),
                img(
                  widthAttr := 400,
                  src := "img/cappio/demo/requests.png"
                ),
              ),
              li(
                h5("Selección de paquetes"),
                p(
                  "En este estados podemos ver cuales son los paquetes disponibles en la red para ser entregados a sus respectivos destinatarios. Si no hay ningún paquete " +
                    "disponible simplemente se mostrara una leyenda indicando esto, de lo contrario se listaran todos los paquetes junto con dos iconos, uno para seleccionar " +
                    "la entrega del paquete y otro para indicar que debe ser descartado por la red. Es importante notar que mientras haya paquetes disponibles, debemos decidir si " +
                    "queremos entregarlo o descartarlo, además solo podremos entregar 1 paquete por vez a un mismo proceso, esto es para que podamos observar los cambios en el estado del" +
                    "proceso que se reflejarán en el componente correspondiente. "
                ),
                img(
                  widthAttr := 400,
                  src := "img/cappio/demo/network.png"
                ),
              ),
            ),
          ),
          li(
            h3("El estado interno de los procesos"),
            p(
              "Esta parte muestra la estructura de capas que componen el proceso, incluyendo el estado interno de cada abstracción. " +
                "La capa superior representa el código de aplicación, es decir el código que escribiríamos nosotros como desarrolladores y que se valdría de componentes " +
                "que nos proverían de abstracciones de alto nivel como broadcast, consenso, failure detectors, etc."
            ),
            img(
              widthAttr := 400,
              src := "img/cappio/demo/state.png"
            ),
          ),
        )
      ),
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
        "confiable y segura.\" Este libro es acerca de comprender las dificultades que subyacen a los problemas de consenso, total order broadcast y atommic commitment " +
        "y desarrollar abstracciones que encapsulen estos problemas." // TODO End-to-end argument pag 7: donde ponerlo? ponerlo?
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
      h3("DEFINIR TITULO"),
      h5("Abstracciones básicas"),
      p(
        "Estamos interesados en abstracciones y algoritmos que sean relevantes para una amplia gama de entornos distribuidos. Para lograr este objetivo, necesitamos capturar las " +
          "características fundamentales de varios sistemas distribuidos en algunas abstracciones básicas, además de las cuales luego podemos definir otras abstracciones de " +
          "programación distribuidas más elaboradas y genéricas. Vamos a presentar las abstracciones básicas que utilizamos para modelar un sistema distribuido compuesto " +
          "por entidades activas que realizan cálculos y se comunican mediante el intercambio de mensajes. Dos tipos de abstracciones serán de importancia primordial: las que " +
          "representan procesos y las que representan enlaces de comunicación."
      ),
      h5(
        "Procesos"
      ),
      p(
        "Abstraemos a las unidades que pueden realizar cálculos en un sistema distribuido a través de la noción de un proceso. Consideramos que el sistema está compuesto de N procesos " +
          "diferentes. Si bien en la realidad esto puede no ser asi, este modelo asume que el conjunto es estático y no cambia, y cada proceso conoce las identidades de todos los procesos. " +
          "Todos los procesos del sistema ejecutan el mismo algoritmo local. La suma de estas copias constituye el algoritmo distribuido real. No asumimos ningún relación particular de nuestra " +
          "noción abstracta de proceso a los procesadores o subprocesos reales de una máquina de computadora o sistema operativo específico. Los procesos se comunican mediante el " +
          "intercambio de mensajes y los mensajes se identifican de manera única, por ejemplo, por su proceso de remitente original utilizando un número de secuencia o un reloj local, " +
          "junto con el identificador del proceso. En otras palabras, suponemos que todos los mensajes que algún algoritmo distribuido alguna vez intercambia son únicos. Los mensajes " +
          "son intercambiados por los procesos a través de enlaces de comunicación."
      ),
      h5("Fallas en los procesos"),
      p(
        "Un proceso ejecuta el algoritmo distribuido asignado a él a través del conjunto de componentes que implementan el algoritmo dentro de ese proceso. Cada vez que " +
          "el proceso no se comporta de acuerdo con el algoritmo decimos que se produce un error. Nuestra unidad de falla es el proceso. Cuando el proceso falla, todos sus componentes fallan al " +
          "mismo tiempo, no hay fallas parciales dentro de un mismo proceso. Las abstracciones de proceso difieren según la naturaleza de las fallas. Las posibles fallas van " +
          "desde un 'crash', donde un proceso simplemente se detiene para ejecutar cualquier paso, sobre una omisión para tomar algunos pasos, un bloqueo con recuperación posterior, hasta " +
          "un comportamiento arbitrario e incluso adversario."
      ),
      h5("Crash-stop"),
      p("La forma más sencilla de fallar en un proceso es cuando el proceso deja de ejecutar pasos. El proceso ejecuta su algoritmo correctamente, incluido el intercambio de mensajes con otros " +
        "procesos, hasta cierto tiempo t, después de lo cual deja de ejecutar cualquier cálculo local y no envía ningún mensaje a otros procesos. En otras palabras, el proceso se para en el " +
        "momento t y nunca se recupera después de ese tiempo. Llamamos a esto una 'crash fault', y hablamos de una abstracción del proceso de tipo 'crash-stop'. Con esta abstracción, un proceso " +
        "es incorrecto si se para en algún momento durante la ejecución. Se dice que es correcto si nunca se para y ejecuta un número infinito de pasos." +
        "Al considerar la abstracción del proceso crash-stop, se supone que un proceso ejecuta su algoritmo correctamente, pero puede pararse en algún momento; después de que un proceso se " +
        "para, nunca se recupera, es decir, no vuelve a realizar ningún paso. Obviamente, en la práctica, los procesos que fallan pueden reiniciarse " +
        "y, por lo tanto, pueden recuperarse y generalmente es deseable que lo hagan. Pero con la abstracción de detención de bloqueo, un proceso recuperado ya no es parte del sistema."
      ),
      p("En esta herramienta solo veremos implementaciones en base a abstracciones de proceso de tipo crash-stop"),
      /*  TODO Aca habla de majority! tal vez lo pueda poner mas adelante porque tengo un algoritmo q lo usa (pag 25)
      Discutimos dos " +
        "ramificaciones de la abstracción de bloqueo de parada.\nEs habitual diseñar algoritmos que implementen una abstracción de programación distribuida dada, por ejemplo, algún tipo de acuerdo, " +
        "siempre que solo un número limitado de procesos sean defectuosos, lo que podría ser una minoría de los procesos o todos los procesos hasta uno. Asumir un límite en el número de procesos " +
        "defectuosos en la forma de un parámetro f significa que cualquier número de procesos hasta f puede fallar, pero no que los procesos f realmente exhiban tales fallas en cada ejecución. " +
        "La relación entre el número f de procesos potencialmente defectuosos y el número total N de procesos en el sistema generalmente se llama resiliencia.\n
        Es importante entender aquí que " +
        "tal suposición no significa que se supone que el hardware subyacente a estos procesos funcione correctamente para siempre. De hecho, la suposición significa que en cada ejecución de un
        algoritmo que se basa en esa abstracción, es muy poco probable que más de f de procesos se bloqueen durante la vida útil de esa misma ejecución. Un ingeniero que elija un algoritmo de este
        tipo para una aplicación determinada debe confiar en que los elementos elegidos subyacentes a la arquitectura de software y hardware hacen que esa suposición sea plausible.
        En general, también es una buena práctica, cuando se diseñan algoritmos que implementan una abstracción distribuida dada bajo ciertos supuestos, determinar con precisión qué propiedades de la
         abstracción se conservan y cuáles se pueden violar cuando no se cumple un subconjunto específico de los supuestos, por ejemplo , cuando fallan más de f procesos.\n
       */
      h5(
        "Enlaces de comunicación (links)"
      ),
      p(
        "La abstracción de un enlace se utiliza para representar los componentes de red del sistema distribuido. Cada par de procesos está conectado por un enlace bidireccional, " +
          "una topología que proporciona conectividad completa entre los procesos. En la práctica, diferentes topologías pueden implementar esta abstracción, posiblemente utilizando " +
          "algoritmos de enrutamiento."
      ),
      h5(
        "Fallas en los enlaces de comunicación"
      ),
      p(
        "En un sistema distribuido, es posible que los mensajes se pierdan al transitar por la red. Sin embargo, es razonable suponer que la probabilidad de que un mensaje llegue a su destino " +
          "no es cero porque es muy poco probable que todos los mensajes intercambiados entre dos procesos se pierdan sistemáticamente a menos que haya una falla grave de la red " +
          "(como una partición en la red). Una manera simple de superar la falta de fiabilidad inherente de la red es seguir retransmitiendo mensajes hasta que lleguen a sus destinos."
      ),
      // TODO talk about failure detectors and system models!(see page 20)

      // TODO abstracciones de links (fair loss, stubborn, perfect link)
      // TODO On the Link Abstractions (pag 43)


      /* Como no esta implementado directamente asi en Cappio tal vez es mejor explicarlo en el nivel
      h3(
        "Algoritmos distribuidos"
      ),
      p(
        ""
      ),
       */

      // TODO aca puedo explicar alguna clase con un ejemplo, y explicar la relacion entre esto y el system model. Ejemplo: fail-stop solo puede darse en sync model porque tenes la abstraction PerfectFailureDetector
      h5("Clases de algoritmos"), // TODO pag 63 explica la combinacion entre system model y abstracciones. vale la pena agregarlo?
      p(
        "A la hora de desarrollar un algoritmo distribuido, las fallas que pueden ocurrir (o que deseamos considerar a la hora de desarrollar el algoritmo), el entorno, los parámetros del sistema y " +
          "otras opciones de diseño, determinan la clase de dicho algoritmo. El libro define las siguientes 6 clases: ",
        ul(
          li(
            "fail-stop: los procesos pueden fallar al detener su ejecución (crash fault), pero todos los otros procesos pueden detectarlos de manera confiable"
          ),
          li(
            "fail-silent: las fallas de procesos nunca pueden detectarse de manera confiable"
          ),
          li(
            "fail-noisy: los procesos pueden fallar pero se pueden detectar dichas fallas, aunque no siempre de manera precisa (la precisión es eventual)"
          ),
          li(
            "fail-recovery: los procesos pueden fallar y luego recuperarse y aún participar en el algoritmo"
          ),
          li(
            // TODO explicar porque se conoce como bizantine, poniendo una referencia al paper two generals problem. O en la tesis?
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


  val links = div(
    h1("Abstracciones de links"),
    p(
      "A continuación, presentamos tres abstracciones de enlaces diferentes, implementadas para la abstracción del proceso de crash-stop. La diferencia entre " +
        "dichas abstracciones radica en las garantías de confiabilidad, siendo algunas más seguras que otras. Todos son abstracciones de enlaces punto a punto, " +
        "es decir, soportan la comunicación entre pares de procesos."
    ),
    p("Definimos las propiedades de cada una de nuestras abstracciones de enlaces utilizando dos tipos de eventos: un evento de solicitud ⟨Enviar⟩ para enviar " +
      "un mensaje y un evento de indicación ⟨Entregar⟩ que entrega un mensaje. Preferimos el término entregar sobre el término más general recibir para enfatizar " +
      "que estamos hablando de una abstracción de enlace específica que se implementará en la red. Por lo general, se recibe un mensaje en un puerto determinado " +
      "de la red y se almacena en algún búfer, y luego se ejecuta algún algoritmo para asegurarse de que se cumplan las propiedades de la abstracción de enlace " +
      "requerida, antes de que el mensaje se entregue realmente. Un proceso invoca la solicitud de envío de una abstracción de enlace para solicitar el envío de un " +
      "mensaje utilizando esa abstracción. Cuando el proceso invoca la solicitud, decimos que el proceso envía el mensaje. Depende de la abstracción del enlace " +
      "transmitir el mensaje al proceso de destino, de acuerdo con la especificación real de la abstracción. La indicación de entrega es activada por el algoritmo " +
      "que implementa la abstracción en un proceso de destino. Cuando este evento ocurre en un proceso p para un mensaje m, decimos que p entrega m."),
    h5("Interfaces, propiedades e implementaciones"),
    p(
      "Antes de explicar las abstracciones de links debemos entender cómo son formuladas por los autores. Una abstracción específica esta compuesta por una interfaz (o módulo) " +
        "que define las solicitudes y las indicaciones (es decir la manera de comunicarse con otras abstracciones) y un conjunto de propiedades que provee. " +
        "Para una misma interfaz podemos tener un número infinito de algoritmos que la implementen, lo que nos permite cambiar de implementación manteniendo las garantías (siempre " +
        "y cuando el algoritmo sea correcto). Estos algoritmos son definidos en pseudocódigo usando manejadores de eventos."
    ),
    h5("Fair-loss link"),
    p(
      "Esta abstracción es la variante más débil en términos de las garantías de confiabilidad. " +
        "Captura la idea básica de que los mensajes se pueden perder pero la probabilidad de que un mensaje se entregue es distinta de 0, esto quiere decir que algunos mensajes se " +
        "entregaran y otros se perderán. Esta abstracción se asemeja al protocolo UDP."
    ),
    h6(
      "Interfaz y propiedades"
    ),
    img(
      widthAttr := 400,
      src := "img/cappio/interfaces/fairloss.png"
    ),
    p("Este es la única abstracción que no define una implementación ya que básicamente modela a la red"),
    h5("Stubborn Link"),
    p("Esta abstracción utiliza un mecanismos de retransmisión en el proceso remitente, cuando se utilizan fair-loss links, para " +
      "asegurarse de que sus mensajes sean finalmente entregados por el proceso de destino. Esto hace que cada mensaje enviado a través del enlace se " +
      "entregue en el receptor un número ilimitado de veces."),
    h6(
      "Interfaz y propiedades"
    ),
    img(
      widthAttr := 400,
      src := "img/cappio/interfaces/stubborn.png"
    ),
    h6(
      "Algoritmo"
    ),
    img(
      widthAttr := 400,
      src := "img/cappio/modules/stubborn.png"
    ),
    h5("Perfect Link"),
    p("Usando fair-loss links, depende del proceso de destino verificar si un mensaje ya se ha entregado o no. Agregar mecanismos para detectar y suprimir duplicados de mensajes, " +
      "además de mecanismos para la retransmisión de mensajes, nos permite construir una primitiva de nivel aún más alto conocida como perfect links o reliable links. " +
      "Esta abstracción se asemeja al protocolo TCP."),
    h6(
      "Interfaz y propiedades"
    ),
    img(
      widthAttr := 400,
      src := "img/cappio/interfaces/perfect.png"
    ),
    h6(
      "Algoritmo"
    ),
    img(
      widthAttr := 400,
      src := "img/cappio/modules/perfect.png"
    ),
    p(
      "En todos los algoritmos que veremos vamos a utilizar Perfect Links, sin embargo definir tres abstracciones de links distintas nos permiten ver las diferentes garantías que se " +
        "pueden proveer y como podemos lograr una mayor confiabilidad a partir de abstracciones de menor confiabilidad mediante composición."
    )
  )
}
