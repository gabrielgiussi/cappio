package oss.giussi.cappio.ui

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.net.StubbornLink
import oss.giussi.cappio.impl.net.StubbornLink.{SLDeliver, SLSend}
import oss.giussi.cappio.ui.core._
import oss.giussi.cappio._

object Levels {

  val processes = (0 to 2).map { id =>
    Process(ProcessId(id), StubbornLink.init(20))
  }.toList

  def level(i: Int) = LevelFactory(i, () => LevelImpl(i, Processes(Set(ProcessId(0), ProcessId(1), ProcessId(2))), Scheduler.init(processes), s => SLSend(Packet(0,1,s,Instance("")))
  ,(ind: SLDeliver, i: Index) => Indication(ind.packet.to,i,ind.packet.payload.toString), (r: SLSend,i: Index) => Request(r.packet.from,i,r.packet.payload.toString)))

  // FIXME esto es por ahora para que siempre genere un nuevo nivel porque las signal ya van a haber cambiado el valor initial
  val levels: List[LevelFactory] = List(level(1), level(2))

}


case class LevelFactory(x: Int, f: () => Level)

trait Level {

  val x: Int

  val processes: Processes

  val $actions: EventStream[List[Action]]

  def diagram: Div = Diagram(processes, $actions)

  def actionSelection: Div

  def states: Div

  def conditions: Div

}

case class LevelImpl[R, S, I](x: Int, processes: Processes, scheduler: Scheduler[R, S, I], f: String => R, toi: (I,Index) => Indication, tor: (R,Index) => Request) extends AbstractLevel(x, processes, scheduler) {

  //override val $states = new EventBus[List[ProcessState]].events.toSignal(processes.ids.map(id => ProcessState(id,,Up)).toList)
  override def toRequest(s: String): R = f(s)

  override def toIndication(ind: I, i:Index): Indication = toi(ind,i)

  override def toRequest(req: R, i: Index): Request = tor(req,i)
}

abstract class AbstractLevel[R, S, I](x: Int, processes: Processes, scheduler: Scheduler[R, S, I]) extends Level {

  case class ProcessState(id: ProcessId, state: S, status: ProcessStatus)

  val $next = new EventBus[Either[RequestBatch[R],DeliverBatch]]

  val $actionsBus = new EventBus[List[Action]]

  case class Snapshot(index: Int, actions: List[Action], step: Step[R,S,I])

  def toRequest(req: R,i: Index): Request

  def toIndication(ind: I,i: Index): Indication

  val $snapshots = $next.events.fold[Snapshot](Snapshot(0,List.empty,WaitingRequest(TickScheduler(scheduler)))){
    // TODO remove duplicated code
    case (Snapshot(current,actions,wr@WaitingRequest(_)),Left(req)) =>
      val index = Index(current)
      val (sent,ind,wd) = wr.request(req)
      val sends = sent.map { case FLLSend(Packet(id,payload,from,to,_)) => Undelivered(from,to,id,payload.toString,index) }
      val requests = req.requests.values.map(toRequest(_,index))
      val indications = ind.map(toIndication(_,Index(current)))
      Snapshot(current + 1,actions ++ indications ++ requests ++ sends,wd)
    case (Snapshot(current,actions,wd@WaitingDeliver(_)),Right(del)) =>
      val index = Index(current)
      val (sent,ind,wr) = wd.deliver(del)
      val sends = sent.map { case FLLSend(Packet(id,payload,from,to,_)) => Undelivered(from,to,id,payload.toString,index) }
      val delivers = del.ops.values.flatMap {
        case Left(FLLDeliver(Packet(id,payload,from,to,_))) => Some(Delivered(from,to,id,payload.toString,actions.collect {
          case Undelivered(_,_,`id`,_,s) => s
        }.head,index)) // FIXME esto es solo para pruebas porque es un asco
        case _ => None
      }
      val indications = ind.map(toIndication(_,index))
      val filtered = actions.filter {
        case Undelivered(_,_,id,_,_) if delivers.map(_.uuid).toList.contains(id) => false
        case _ => true
      }
      Snapshot(current + 1, filtered ++ indications ++ delivers ++ sends,wr)
    case (s,_) => s // TODO log error in console
  }
  val $steps = $snapshots.map(_.step)

  override val $actions = $snapshots.map(_.actions).changes // TODO puedo devolver una signal directamente total al principio no va a tener actions

  def toRequest(s: String): R

  override def actionSelection: Div = {
    div(
      child <-- $steps.map {
        case WaitingRequest(sch) => ActionSelection.reqBatchInput(processes,$next.writer.contramap[RequestBatch[R]](r => Left(r)),(obs: Observer[Option[R]]) => (i: String) => input(
          `type` := "text",
          // TODO how to throttle key press?
          inContext(thisNode => onChange.mapTo(Option(thisNode.ref.value).filterNot(_.isEmpty).map(toRequest)) --> obs),
        ))
        case WaitingDeliver(sch) => ActionSelection.networkInput(sch.scheduler.network.inTransit(), $next.writer.contramap(r => Right(r)))
      }
    )
  }

  def renderState(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]) = div(
    cls := "col-md-4 mb-4",
    div(
      cls := "card",
      div(
        cls := "card-body",
        renderStateI(id, initial, $states)
      )
    )
  )

  def renderStateI(id: ProcessId, initial: ProcessState, $states: Signal[ProcessState]): Modifier[Div] = id.toString

  override def states: Div = div(
    cls := "row wow fadeIn",
    // children <-- $states.split(_.id)(renderState) TODO
  )

  override def conditions: Div = div()


}