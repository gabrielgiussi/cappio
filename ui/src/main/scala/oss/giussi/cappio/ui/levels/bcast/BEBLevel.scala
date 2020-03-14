package oss.giussi.cappio.ui.levels.bcast


import com.raquo.laminar.api.L._
import oss.giussi.cappio._
import oss.giussi.cappio.impl.PhotosApp
import oss.giussi.cappio.impl.PhotosApp._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.core.Index
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.BEBLevel.ModLevel
import oss.giussi.cappio.ui.levels.{AbstractLevel, PredefinedAction}

object BEBLevel {

  type ModLevel = AlbumBeb

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,PhotosApp.bestEffort(all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.ui.core.LevelConditions._

  val ok = BEBLevel(
    "Best effort 1",
    div(
      h1("Usando Best Effort Broadcast"),
      p(
        "El objetivo de este nivel es asegurar que todos los procesos reciban todos los mensajes enviados por el proceso 0 en el mismo orden"
      )
    ),
    List(0,1,2).map(ProcessId).map(albumState[Albums,ModLevel](Album("Mis fotos", Set("Vacaciones", "Graduacion"))))
  ) _

  val ko = {
    BEBLevel(
      "Best effort 2",
      div(
        h1("Rompiendo Best Effort Broadcast"),
        p(
        "En el nivel anterior vimos cómo podemos usar broadcast para enviar un mensaje a todos los procesos del sistema. Pero ¿podemos estar seguros de que " +
          "el mensaje se va a entregar a todos los procesos en todos los casos?. Best Effort Broadcast sólo asegura la entrega a todos los procesos correctos " +
          "en caso de que el procesos que inicio el broadcast no falle. Es mas, si lo pensamos detenidamente el Best Effort Broadcast no nos da más garantías que " +
          "las que podríamos obtener usando un Perfect Link, pero para el modelo de capas propuesto por los autores resulta muy útil porque provee una interfaz " +
          "que puede ser utilizada por capas superiores si se requiere enviar un mensaje a todos los procesos y esto nos permite cambiar la implementacion de " +
          "broadcast si necesitamos otras garantías." +
          "En este nivel el objetivo es ver un escenario donde Best Effort Broadcast no es suficiente."
        ),
        h1("Objetivos"),
        p(
          "El objetivo de este nivel es hallar un escenario donde uno de los procesos no recibe todos los mensajes, haciendo que los procesos terminen con distintos estados"
        )
      ),
      List((1,Album("Mis fotos",Set("Graduacion"))),(2,Album("Mis fotos", Set("Vacaciones", "Graduacion")))).map {
        case (id,expected) => albumState[Albums, ModLevel](expected)(ProcessId(id))
      } :+ noPendingMessages[ModLevel](_.module.state.module.state.module.state.sent)
    ) _
  }

}

// TODO replace this class by an apply method?
case class BEBLevel(title: String, shortDescription: Div, cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int) extends AbstractLevel[ModLevel](BEBLevel.scheduler(nProcesses,timeout), cond) {

  override def predefined: Set[PredefinedAction[Req]] = Set(
    PredefinedAction(Index(1),ProcessId(0),ProcessRequest.predefined(ProcessId(0), CreateAlbum("Mis fotos"))),
    PredefinedAction(Index(3),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Vacaciones"))),
    PredefinedAction(Index(5),ProcessId(0),ProcessRequest.predefined(ProcessId(0), AddPhoto("Mis fotos", "Graduacion")))
  )

  override def code: Div = div(
    h2("Interface"),
    img(
      src := "img/cappio/interfaces/beb.png"
    ),
    h2("Broadcast básico"),
    img(
      src := "img/cappio/modules/beb.png"
    )
  )

  override val indicationPayload: PhotosApp.AlbumOpResult => String = {
    case AlbumCreated(name) => s"album [$name] creado"
    case PhotoCreated(album, photo) => s"foto $photo agregada a $album"
    case PhotoRemoved(album, photo) => s"foto $photo eliminada de $album"
  }
  override val reqTypes: List[Inputs[Req]] = List()
}
