package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.VectorTime
import oss.giussi.cappio.crdt.pure.StabilityProtocol.TCStable

import scala.language.higherKinds
import scala.util._

/**
 * Typeclass to be implemented by CRDTs if they shall be managed by [[CRDTService]]
 *
 * @tparam A CRDT type
 * @tparam B CRDT value type
 * @tparam Op CRDT value type
 */
trait CRDTServiceOps[A, B, Op] {

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
  def prepare(crdt: A, operation: Op): Try[Option[Op]] = Success(Some(operation))

  /**
   * Update phase 2 ("downstream").
   */
  def effect(crdt: A, op: Op, vt: VectorTime, systemTimestamp: Long = 0L, creator: String = ""): A // TODO remove unused args

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
