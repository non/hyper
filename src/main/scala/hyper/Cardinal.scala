package hyper

import spire.algebra.{ Order, Rig }
import spire.macros.Auto

/**
 * Implementation of the Cardinal Numbers.
 *
 * The idea is that these numbers can be used to describe the size of
 * a set, even if that set is infinite.
 *
 * https://en.wikipedia.org/wiki/Cardinal_number
 *
 * Constructivists beware! This implementation assumes not only the
 * Axiom of Choice, but also the Continuum Hypothesis. The practical
 * effect of this is that 2^ℵ(k) = ℵ(k+1).
 */
sealed abstract class Cardinal extends Product with Serializable { lhs =>

  import Cardinal.{ Finite, Aleph }

  def fold[A](fin: BigInt => A, inf: BigInt => A): A =
    this match {
      case Finite(n) => fin(n)
      case Aleph(i) => inf(i)
    }

  def isZero: Boolean = fold(_.signum == 0, _ => false)
  def isOne: Boolean = fold(_ == 1, _ => false)

  def compare(rhs: Cardinal): Int =
    (lhs, rhs) match {
      case (Finite(m), Finite(n)) => m compare n
      case (Aleph(i), Aleph(j)) => i compare j
      case (Aleph(_), _) => 1
      case (_, Aleph(_)) => -1
    }

  def <(rhs: Cardinal): Boolean = (lhs compare rhs) < 0
  def <=(rhs: Cardinal): Boolean = (lhs compare rhs) <= 0
  def >(rhs: Cardinal): Boolean = (lhs compare rhs) > 0
  def >=(rhs: Cardinal): Boolean = (lhs compare rhs) >= 0

  def min(rhs: Cardinal): Cardinal =
    if ((lhs compare rhs) <= 0) lhs else rhs

  def max(rhs: Cardinal): Cardinal =
    if ((lhs compare rhs) <= 0) rhs else lhs

  def +(rhs: Cardinal): Cardinal =
    (lhs, rhs) match {
      case (Finite(m), Finite(n)) => Finite(m + n)
      case _ => lhs max rhs
    }

  def *(rhs: Cardinal): Cardinal =
    if (lhs.isZero || rhs.isZero) Cardinal.zero
    else (lhs, rhs) match {
      case (Finite(m), Finite(n)) => Finite(m * n)
      case _ => lhs max rhs
    }

  def **(rhs: Cardinal): Cardinal =
    if (rhs.isZero) Cardinal.one
    else if (lhs.isZero) Cardinal.zero
    else if (lhs.isOne) Cardinal.one
    else if (rhs.isOne) lhs
    else (lhs, rhs) match {
      case (Finite(m), Finite(n)) => Finite(spire.math.pow(m, n))
      case (Finite(_), Aleph(i)) => Aleph(i + 1)
      case (Aleph(_), Finite(_)) => lhs
      case (Aleph(i), Aleph(j)) => Aleph((i max j) + 1)
    }
}

object Cardinal {

  case class Finite(n: BigInt) extends Cardinal {
    require(n.signum >= 0)
    override def toString: String = n.toString
  }

  case class Aleph(i: BigInt) extends Cardinal {
    require(i.signum >= 0)
    override def toString: String = "ℵ" + i.toString
  }

  def apply(n: BigInt): Cardinal = Finite(n)
  def apply(n: Long): Cardinal = Finite(BigInt(n))

  def aleph(i: BigInt): Cardinal = Aleph(i)
  def aleph(i: Long): Cardinal = Aleph(BigInt(i))

  val zero: Cardinal = Finite(BigInt(0))
  val one: Cardinal = Finite(BigInt(1))

  implicit val cardinalOrder: Order[Cardinal] =
    Auto.scala.order

  implicit val cardinalRig: Rig[Cardinal] =
    Auto.scala.rig(Cardinal.zero, Cardinal.one)
}
