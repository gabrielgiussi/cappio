package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLModule, PLSend}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.{ProcessId, ProcessRequest, Scheduler}

object DemoLevel {

  import oss.giussi.cappio.ui.core.LevelConditions._

  type ModLevel = AppMod2[String,PLModule[String]]

  val selection = ActionSelection.fromToPayloadRequest("Enviar"){ case (from,to,payload) => PLSend.external(from,to,payload) } _

  val conditions: Conditions[ModLevel] = List(
    processState[String, ModLevel]("A",_.state.getOrElse("-"),implicitly)(ProcessId(0)) // TODO
  )

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val app = AppState.app2[String,PLModule[String]](PerfectLink[String](timeout), (state, ind) => Some(ind.packet.payload))
    Scheduler.init(all,_ => app)
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.packet.payload
    override val reqTypes = List(
      selection,
      ActionSelection.crash
    )

    // TODO deberia incorporar imagenes con cada uno de los componentes que esto y describiendo ?
    override val shortDescription = div(
      p(
        "Este nivel utiliza las abstracciones más básicas para poder comunicar procesos punto a punto, por lo tanto no vamos a encontrar ninguna novedad ya que se trata de " +
          "un modo de comunicación básico, sin embargo nos va a servir para entender cómo funciona esta herramienta didáctica"
      ),
      h1("Entendiendo la interfaz gráfica"),
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
      h1("Cómo funciona CappIO"),
      p(
        "TOOD"
      )
    )

    override def title: String = "Demo"

    override def predefined: Set[PredefinedAction[PLSend[String]]] = Set(
      PredefinedAction(Index(1), ProcessId(0), ProcessRequest(ProcessId(0), PLSend.external(ProcessId(0),ProcessId(1),"A"),true)),
      PredefinedAction(Index(3), ProcessId(1), ProcessRequest(ProcessId(1), PLSend.external(ProcessId(1),ProcessId(2),"B"),true)),
      PredefinedAction(Index(5), ProcessId(0), ProcessRequest(ProcessId(0), PLSend.external(ProcessId(0),ProcessId(1),"C"),true))
    )

  }

  val good = DemoLevel(conditions) _
}
