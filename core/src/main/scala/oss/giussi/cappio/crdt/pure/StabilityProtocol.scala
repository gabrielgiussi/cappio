package oss.giussi.cappio.crdt.pure

import oss.giussi.cappio.crdt.VectorTime

object StabilityProtocol {

  case class StabilityConf(localPartition: String, partitions: Set[String])

  object RTM {
    def apply(conf: StabilityConf): RTM = RTM(conf.localPartition, (conf.partitions - conf.localPartition).map(_ -> VectorTime.Zero).toMap)
  }

  case class RTM(private val localPartition: String, private val timestamps: Map[String, VectorTime]) {

    def update(partition: String, timestamp: VectorTime): RTM = Option(partition).filterNot(_ equals localPartition).flatMap(timestamps.get) match {
      case Some(oldTimestamp) => copy(timestamps = timestamps + (partition -> oldTimestamp.merge(timestamp)))
      case _                  => this
    }

    def stable(): Option[TCStable] = {
      val processIds = timestamps.values.flatMap(_.value.keys).toSet
      Option(TCStable(VectorTime(processIds.map(processId => (processId, timestamps.values.map(_.localTime(processId)).reduce[Long](Math.min))).toMap))).filterNot(_.isZero)
    }
  }

  case class TCStable(private val stableVector: VectorTime) {
    def isStable(that: VectorTime): Boolean = that <= stableVector

    def isZero: Boolean = stableVector.value.forall(_._2 equals 0L)

    def equiv(that: TCStable): Boolean = stableVector.equiv(that.stableVector)
  }

}
