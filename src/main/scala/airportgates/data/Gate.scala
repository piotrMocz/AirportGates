package airportgates.data

/**
 * Created by Piotr on 2015-04-11.
 */

class Gate(val id: GateID,
           val possibleRegions: Set[Section],
           val possibleSizeCategories: Set[Category],
           val possibleGroundHandlers: Set[GroundHandler]) {

  def canAssignFlight(flight: Flight): Boolean = possibleRegions.contains(flight.destination) &&
                                                 possibleRegions.contains(flight.origin) &&
                                                 possibleGroundHandlers.contains(flight.groundHandler)

}
