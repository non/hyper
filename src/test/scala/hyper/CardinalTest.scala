package hyper

import spire.laws._

import org.scalatest.FunSuite
import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary.arbitrary

import org.typelevel.discipline.scalatest.Discipline

class LawTests extends FunSuite with Discipline {

  implicit val arbitraryCardinal: Arbitrary[Cardinal] =
    Arbitrary(Gen.oneOf(
      arbitrary[BigInt].map(n => Cardinal(n.abs)),
      arbitrary[Byte].map(n => Cardinal.aleph(n & 255))))

  checkAll("Rig[Cardinal]", RingLaws[Cardinal].rig)
  checkAll("Order[Cardinal]", OrderLaws[Cardinal].order)
}
