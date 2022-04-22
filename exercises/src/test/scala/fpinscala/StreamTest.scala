package fpinscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import fpinscala.laziness._

class StreamTest extends AnyFlatSpec with Matchers {

  val stream: Stream[Int] = Stream.apply(1,2,3,4,5,6)

  "take" should "return the first x number of elements" in {
    val expected = Stream.apply(1,2,3)

    stream.take(3).toList shouldBe expected.toList
  }

  "drop" should "drop the first x elements" in {
    stream.drop(4).toList shouldBe List(5,6)
  }

  "takeWhile" should "take the first elements from a list matching a predicate" in {
    stream.takeWhile(_ < 5).toList shouldBe List(1,2,3,4)
  }

  "forAll" should "return true if all values of a stream match a predicate" in {
    stream.forAll(_.isInstanceOf[Int]) shouldBe true
  }

  it should "return false if one value doesn't meet the predicate" in {
    stream.forAll(_ % 2 == 0) shouldBe false
  }

  it should "terminate early" in {
    var effect = 0

    def four(): Int = {
      effect += 1
      4
    }

    val badStream = Stream.apply(1,2,3,four(),5)

    badStream.forAll(_.isInstanceOf[Int]) shouldBe true

    assert(effect == 1)

    // Should halt before calling four() again
    badStream.forAll(_ < 3) shouldBe false

    assert(effect == 1)
  }

  "takeWhile2" should "work exactly like takeWhile" in {
    stream.takeWhile2(_ < 5).toList shouldBe List(1,2,3,4)
  }
}
