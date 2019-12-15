package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.CRDTTypes.Operation
import oss.giussi.cappio.crdt.pure.StabilityProtocol.TCStable

import scala.language.higherKinds
import scala.util._

/**
 * Typeclass to be implemented by CRDTs if they shall be managed by [[CRDTService]]
 *
 * @tparam A CRDT type
 * @tparam B CRDT value type
 */
trait CRDTServiceOps[A, B] {

  /**
   * Default CRDT instance.
   */
  def zero: A

  /**
   * Returns the CRDT value (for example, the entries of an OR-Set)
   */
  final def value(crdt: A): B = eval(crdt)

  def eval(crdt: A): B

  /**
   * Must return `true` if CRDT checks preconditions. Should be overridden to return
   * `false` if CRDT does not check preconditions, as this will significantly increase
   * write throughput.
   */
  def precondition: Boolean = true

  /**
   * Update phase 1 ("atSource"). Prepares an operation for phase 2.
   */
  def prepare(crdt: A, operation: Operation): Try[Option[Operation]] = Success(Some(operation))

  /**
   * Update phase 2 ("downstream").
   */
  def effect(crdt: A, op: Operation, vt: VectorTime, systemTimestamp: Long = 0L, creator: String = ""): A

  /**
   * This mechanism allows to discard stable operations, not only timestamps, if they have no
   * impact on the semantics of the data type. For some data types like RWSet
   * some operations become useless once other future operations become stable"
   *
   * @param crdt a crdt
   * @param stable the TCStable delivered by the TCSB middleware
   * @return the crdt after being applied causal stabilization. By default it returns the same crdt unmodified
   */
  def stable(crdt: A, stable: TCStable) = crdt

}

object CRDTService {

  /**
   * Persistent event with update operation.
   *
   * @param operation update operation.
   */
  case class ValueUpdated(operation: Operation)

}

/**
 * A generic, replicated CRDT service that manages a map of CRDTs identified by name.
 * Replication is based on the replicated event `log` that preserves causal ordering
 * of events.
 *
 * @tparam A CRDT type
 * @tparam B CRDT value type
 */
trait CRDTService[A, B] {

  import CRDTService._

  /**
   * CRDT service id.
   */
  def serviceId: String

  /**
   * CRDT service operations.
   */
  def ops: CRDTServiceOps[A, B]


  /**
   * Returns the current value of the CRDT identified by `id`.
   */
  def value(id: String): B

  protected def op(id: String, operation: Operation): B

  private case class OnChange(crdt: A, operation: Option[Operation])

  private case class OnStable(crdt: A, stable: TCStable)

  /*
  private class CRDTActor(crdtId: String, override val eventLog: ActorRef, stabilityConf: Option[StabilityConf]) extends EventsourcedActor {
    override val id =
      s"${serviceId}_${crdtId}"

    override val aggregateId =
      Some(s"${ops.zero.getClass.getSimpleName}_${crdtId}")

    var crdt: A =
      ops.zero

    var lastTCStable: Option[TCStable] = None
    var rtm: Option[RTM] = stabilityConf.map(RTM.apply)

    override def stateSync: Boolean = ops.precondition

    override def onCommand = {
      case Get(`crdtId`) =>
        sender() ! GetReply(crdtId, ops.value(crdt))
      case Update(`crdtId`, operation) =>
        ops.prepare(crdt, operation) match {
          case Success(Some(op)) =>
            persist(ValueUpdated(op)) {
              case Success(evt) =>
                sender() ! UpdateReply(crdtId, ops.value(crdt))
              case Failure(err) =>
                sender() ! Status.Failure(err)
            }
          case Success(None) =>
            sender() ! UpdateReply(crdtId, ops.value(crdt))
          case Failure(err) =>
            sender() ! Status.Failure(err)
        }
      case Save(`crdtId`) =>
        save(crdt) {
          case Success(md) =>
            sender() ! SaveReply(crdtId, md)
          case Failure(err) =>
            sender() ! Status.Failure(err)
        }
    }

    override def onEvent = {
      case evt @ ValueUpdated(operation) =>
        crdt = ops.effect(crdt, operation, lastVectorTimestamp, lastSystemTimestamp, lastEmitterId)
        updateStability(lastProcessId, lastVectorTimestamp)
        context.parent ! OnChange(crdt, Some(operation))
    }

    def updateStability(processId: String, vt: VectorTime) = {
      rtm = rtm.map(_.update(processId, vt))
      rtm.flatMap(_.stable()).filterNot(s => lastTCStable.fold(false)(_ equiv s)).foreach { tcstable =>
        lastTCStable = Some(tcstable)
        crdt = ops.stable(crdt, tcstable)
        context.parent ! OnStable(crdt, tcstable)
      }
    }

    override def onSnapshot = {
      case snapshot =>
        crdt = snapshot.asInstanceOf[A]
        context.parent ! OnChange(crdt, None)
    }

  }

   */

  private[crdt] def onStable(crdt: A, stable: TCStable): Unit = ()
}