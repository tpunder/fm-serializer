package fm.serializer

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

final class TestImplicitResolution extends AnyFunSuite with Matchers {
  test("java.math.BigInteger") {
    implicitly[Serializer[java.math.BigInteger]] should be theSameInstanceAs Serializer.javaBigInteger
    implicitly[Deserializer[java.math.BigInteger]] should be theSameInstanceAs Deserializer.javaBigInteger
  }
}
