package airportgates.data

import spire.implicits._
import spire.math._
import spire.algebra.{Order, Trig}

/**
 * Created by Piotr on 2015-04-12.
 */

class Time(val t: Double)

object Time {
  def apply(t: Double) = new Time(t)

  def idleTimeCost(time: Time): Double = 1000 * (atan(0.21 * (5 - time.t) ) + pi / 2)

  implicit def orderingTime: Ordering[Time] = new Ordering[Time] {
    override def compare(x: Time, y: Time): GateID = x compare y
  }

  implicit def trigTime: Trig[Time] = new Trig[Time] {
    override def e: Time = Time(spire.math.e)

    override def acos(a: Time): Time = Time(spire.math.acos(a.t))

    override def atan(a: Time): Time = Time(spire.math.atan(a.t))

    override def tanh(x: Time): Time = Time(spire.math.tanh(x.t))

    override def log(a: Time): Time = Time(spire.math.log(a.t))

    override def cosh(x: Time): Time = Time(spire.math.cosh(x.t))

    override def tan(a: Time): Time = Time(spire.math.tan(a.t))

    override def cos(a: Time): Time = Time(spire.math.cos(a.t))

    override def exp(a: Time): Time = Time(spire.math.exp(a.t))

    override def expm1(a: Time): Time = Time(spire.math.expm1(a.t))

    override def asin(a: Time): Time = Time(spire.math.asin(a.t))

    override def pi: Time = Time(spire.math.pi)

    override def log1p(a: Time): Time = Time(spire.math.log1p(a.t))

    override def sin(a: Time): Time = Time(spire.math.sin(a.t))

    override def toRadians(a: Time): Time = Time(spire.math.toRadians(a.t))

    override def atan2(y: Time, x: Time): Time = Time(spire.math.atan2(y.t, x.t))

    override def toDegrees(a: Time): Time = Time(spire.math.toDegrees(a.t))

    override def sinh(x: Time): Time = Time(spire.math.sinh(x.t))
  }

  implicit def numTime: Numeric[Time] = new Numeric[Time] {
    override def times(x: Time, y: Time): Time = Time(x.t * y.t)

    override def toType[B](a: Time)(implicit ev: ConvertableTo[B]): B = ev.fromRational(a.t)

    override def toReal(a: Time): Real = toType[Real](a)

    override def toAlgebraic(a: Time): Algebraic = toType[Algebraic](a)

    override def toBigInt(a: Time): BigInt = toType[BigInt](a)

    override def toRational(a: Time): Rational = a.t

    override def toFloat(a: Time): Float = a.t.toFloat

    override def toInt(a: Time): Int = a.t.toInt

    override def toBigDecimal(a: Time): BigDecimal = a.t.toBigDecimal()

    override def toShort(a: Time): Short = a.t.toShort

    override def toLong(a: Time): Long = a.t.toLong

    override def toByte(a: Time): Byte = a.t.toByte

    override def toNumber(a: Time): Number = toType[Number](a)

    override def toString(a: Time): String = "Time(" + a.t.toString + ")"

    override def compare(x: Time, y: Time): Int = x.t.compare(y.t)

    override def negate(x: Time): Time = ???  // you really shouldn't negate time :)

    override def abs(a: Time): Time = a

    override def signum(a: Time): Int = if (a.t == 0) 0 else 1

    override def zero: Time = Time(0.0)

    override def div(x: Time, y: Time): Time = Time(x.t / y.t)

    override def one: Time = Time(1.0)

    override def toDouble(a: Time): Double = a.t

    override def round(a: Time): Time = Time(a.t.round.toDouble)

    override def floor(a: Time): Time = Time(a.t.floor)

    override def ceil(a: Time): Time = Time(a.t.ceil)

    override def isWhole(a: Time): Boolean = a.t.isWhole()

    override def fromShort(n: Short): Time = fromType[Short](n)

    override def fromBigInt(n: BigInt): Time = fromType[BigInt](n)

    override def fromAlgebraic(n: Algebraic): Time = fromType[Algebraic](n)

    override def fromByte(n: Byte): Time = fromType[Byte](n)

    override def fromDouble(n: Double): Time = fromType[Double](n)

    override def fromReal(n: Real): Time = fromType[Real](n)

    override def fromRational(n: Rational): Time = Time(n.toDouble)

    override def fromType[B](b: B)(implicit ev: ConvertableFrom[B]): Time = Time(b.toDouble())

    override def fromFloat(n: Float): Time = fromType[Float](n)

    override def fromBigDecimal(n: BigDecimal): Time = fromType[BigDecimal](n)

    override def fromLong(n: Long): Time = fromType[Long](n)

    override def plus(x: Time, y: Time): Time = Time(x.t + y.t)

    override def fpow(a: Time, b: Time): Time = Time(spire.math.pow(a.t, b.t))

    override def nroot(a: Time, n: Int): Time = ???
  }


}