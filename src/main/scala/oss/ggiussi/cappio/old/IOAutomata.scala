package oss.ggiussi.cappio.old

import scala.collection.immutable.Queue

object Process {
  def apply(n: Int, i: Int, f: Vector[Int] => Int): Process[Int] = Process(i, Vector.fill(n)(None), f)

}

// TODO enum or trait?
object ActionType extends Enumeration {

  type ActionType = Value
  val Input, Output, Internal = Value
}

trait Action

case class Init[V](to: Int, v: V) extends Action

case class Deliver[V](from: Int, to: Int, v: V) extends Action

case class Send[V](from: Int, to: Int, v: V) extends Action

case class Decide[V](to: Int, v: V) extends Action

trait Transition

trait IOAutomata {

  private val defaultTransition = (_: Action) => this

  def transition: PartialFunction[Action, IOAutomata]

  final def next(action: Action) = transition.applyOrElse(action, defaultTransition)

}

case class PreconditionException(automata: IOAutomata, action: Action) extends RuntimeException {
  override lazy val getMessage: String = s"Automata: ${automata.toString} doesn't apply the precondition for ${action.toString}"
}

case class Process[S](id: Int, state: Vector[Option[S]], f: Vector[S] => S) extends IOAutomata {

  private def init(v: S) = copy(state = state.updated(id, Some(v)))

  private def deliver(v: S, j: Int) = copy(state = state.updated(j, Some(v)))

  override lazy val transition = {
    case Init(`id`, v: S)          =>
      init(v)
    case a@Send(`id`, _, v: S)     =>
      if (Some(v) == state(id)) this // Effect: None
      else throw PreconditionException(this, a)
    case a@Decide(`id`, v: S)      =>
      /*
      The slide says:
      - Precondition: for all 1 <= j <= n: val(j) <> null
      - v = f(val(1),...,val(n)) // TODO If this isn't an effect, it must be a precondition !!!!!!
      - Effect: None
       */
      if ((state.forall(_.isDefined)) && (f(state.flatten) == v)) this // Effect: None
      else throw PreconditionException(this, a)
    case Deliver(from, `id`, v: S) =>
      deliver(v, from)

  }
}

object Channel {
  def apply(from: Int, to: Int): Channel[Int] = new Channel[Int](from, to, Queue.empty[Int])
}

case class Channel[S](from: Int, to: Int, state: Queue[S]) extends IOAutomata {

  def send(v: S) = copy(state = state.enqueue(v))

  def deliver(v: S) = copy(state = state.dequeue._2)

  override lazy val transition = {
    case Send(`from`, `to`, v: S)      =>
      send(v)
    case a@Deliver(`from`, `to`, v: S) =>
      if (state.headOption == Some(v)) deliver(v)
      else throw PreconditionException(this, a)
  }
}

case class Composition(automatas: Seq[IOAutomata]) extends IOAutomata {

  override def transition(): PartialFunction[Action, IOAutomata] = {
    case action => copy(automatas = automatas.map(_.next(action)))
  }

}

object Prueba2 extends App {

  val reduce = (vector: Vector[Int]) => vector.fold(0)(_ + _)

  val process0 = Process(2, 0, reduce)
  val process1 = Process(2, 1, reduce)
  val channel01 = Channel(0, 1)
  val channel10 = Channel(1, 0)

  val automata: IOAutomata = Composition(Seq(process0, process1, channel01, channel10))

  // Como hago una Automata que cuando yo le diga Init, "cargue" una accion para el proximo step.
  // La accion de afuera siempre debiera ser Step(n: Int)
  val steps: Seq[Action] = Seq(
    Init(1, 10), // environment
    Init(0, 20),
    Send(0, 1, 20),
    Send(1, 0, 10),
    Deliver(1, 0, 10),
    Deliver(0, 1, 20),
    Decide(0, 30),
    Decide(1, 30)
  )

  val result = steps.foldLeft(automata)(_ next _)

  println(result)


}