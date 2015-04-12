package airportgates.data

/**
 * Created by Piotr on 2015-04-12.
 */

class AirportState(val gates: Map[GateID, Gate],
                   val assignments: Map[Flight, Gate]) {

  // assign flight to gate
  def assign(flight: Flight, gate: Gate): Either[GateAssignmentError, AirportState] = {
    if (assignments.contains(flight)) Left(GateAlreadyAssigned)
    else reassign(flight, gate)
  }

  // same as assign, only it doesn't check if the flight is already assigned:
  def reassign(flight: Flight, gate: Gate): Either[GateAssignmentError, AirportState] = {
    if (!gate.canAssignFlight(flight)) Left(GateCannotHandle)
    else if (!gates.contains(gate.id)) Left(GateNotFound)
    else if (!isTimeIntervalRight(flight, gate.id)) Left(GateTimeConflict)
    else Right(new AirportState(gates, assignments + (flight -> gate)))
  }
  
  def getGateById(id: GateID): Option[Gate] = gates get id

  // checks if the times don't overlap
  private def isTimeIntervalRight(flight: Flight, gateID: GateID): Boolean = {
    val interval = flight.timeAtGate + flight.timeBounds
    val flights = assignedToGate(gateID)
    flights.find(_.timeAtGate.intersects(interval)) match {
      case Some(_) => false
      case None    => true
    }
  }

  def assignedToGate(id: GateID): List[Flight] = {
    assignments.filter(_._2.id == id).keys.toList
  }
}

sealed trait GateAssignmentError
case object GateAlreadyAssigned extends GateAssignmentError
case object GateCannotHandle extends GateAssignmentError
case object GateNotFound extends GateAssignmentError
case object GateTimeConflict extends GateAssignmentError