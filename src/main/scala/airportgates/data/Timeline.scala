package airportgates.data

import scala.collection.immutable.{SortedMap => Map}
import spire.implicits._

/**
 * Created by Piotr on 2015-04-12.
 */
class Timeline(val states: Map[Time, AirportState]) {

  // this method tries to return a valid airport state.
  // if there's no such time mark on the timeline, we return the
  // nearest past state. if no past states exist, None is returned
  def getAirportState(t: Time): Option[AirportState] = states get t match {
    case state@Some(_) => state
    case None =>
      val smaller = for {
        p <- states if p._1 < t
      } yield p._2
      smaller.lastOption
  }

  def apply(t: Time): Option[AirportState] = getAirportState(t)

  // will overwrite any state under time t
  def addAirportState(t: Time, state: AirportState): Timeline = new Timeline(states + (t -> state))
}

object Timeline { self : Timeline =>
  def empty: Timeline = new Timeline(Map.empty[Time, AirportState])
}