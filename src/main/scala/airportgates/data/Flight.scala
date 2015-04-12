package airportgates.data

import spire.math.{Interval, Rational, atan, pi}
import spire.implicits._

/**
 * Created by Piotr on 2015-04-11.
 */

class Flight(val timeAtGate: Interval[Time],
             val origin: Section,
             val destination: Section,
             val plane: Plane,
             val groundHandler: GroundHandler) {}


sealed trait Section
case object Schengen extends Section
case object EU extends Section
case object NonEU extends Section


case class GroundHandler(handler: String)