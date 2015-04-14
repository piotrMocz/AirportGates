package airportgates.data

import spire.math.Interval

class Flight(val timeAtGate: Interval[Time],
             val origin: Section,
             val destination: Section,
             val plane: Plane,
             val groundHandler: GroundHandler) {

  def timeBounds: Interval[Time] = {
    val offset = plane.sizeCategory match {
      case Small => 0.1
      case Medium => 0.2
      case Large => 0.3
    }
    Interval(Time(-offset), Time(offset))
  }
}

  sealed trait Section

  case object Schengen extends Section

  case object EU extends Section

  case object NonEU extends Section


  case class GroundHandler(handler: String)