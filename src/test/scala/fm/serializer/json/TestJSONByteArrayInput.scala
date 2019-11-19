package fm.serializer.json

import org.scalatest.{FunSuite, Matchers, PrivateMethodTester}

final class TestJSONByteArrayInput extends FunSuite with Matchers with PrivateMethodTester {
  private val countLeadingBitsInByteMethod: PrivateMethod[Int] = PrivateMethod[Int](Symbol("countLeadingBitsInByte"))

  test("countLeadingBitsInByte") {
    (0 until 128).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 0
    }

    (128 until 192).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 1
    }

    (192 until 224).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 2
    }

    (224 until 240).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 3
    }

    (240 until 248).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 4
    }

    (248 until 252).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 5
    }

    (252 until 254).foreach { i: Int =>
      JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(i) shouldBe 6
    }

    JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(254) shouldBe 7
    JSONByteArrayInput invokePrivate countLeadingBitsInByteMethod(255) shouldBe 8
  }

}
