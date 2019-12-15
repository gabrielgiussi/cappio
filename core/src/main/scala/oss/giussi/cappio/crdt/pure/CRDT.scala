package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.pure.CRDTTypes.Operation

object CRDT {

  def apply[A](state: A): CRDT[A] = CRDT(POLog(), state)

  def zero: CRDT[Seq[Operation]] = CRDT(POLog(), Seq.empty)
}

/**
 * A pure op-based CRDT wich state is splitted in two componentes. A map of timestamps to operations (the POLog) and
 * a plain set of stable operations or a specialized implementation (the state)
 * P(O) × (T ֒→ O)
 *
 * @param polog The POLog contains only the set of timestamped operations
 * @param state The state of the CRDT that contains stable operations (non-timestamped) or a "specialized
 *              implementation according to the domain" e.g., a bitmap for dense sets of integers in an AWSet
 * @tparam B state type
 */
case class CRDT[B](polog: POLog[Operation], state: B)