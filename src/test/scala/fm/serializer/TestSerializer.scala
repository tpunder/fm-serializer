/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.serializer

import java.io.File
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.util.Date
import org.scalatest.{FunSuite, Matchers}
import scala.collection.JavaConverters._

trait TestSerializer[BYTES] extends FunSuite with Matchers {
  def serialize[T](v: T)(implicit ser: Serializer[T]): BYTES
  def deserialize[T](bytes: BYTES)(implicit deser: Deserializer[T]): T
  
  //===============================================================================================
  // Simple Object Testing
  //===============================================================================================
  
  case class PersonNoAge(name: String)
  case class Person(name: String, age: Int = 18, blah: String = "blah")
  case class LinkedPerson(person: Person, next: LinkedPerson)
  
  test("Person") {
    val p: Person = Person("Bob", 123)
    val bytes: BYTES = serialize(p)
    val p2: Person = deserialize[Person](bytes)
    
    p2.name should equal (p.name)
    p2.age should equal (p.age)
    
    p2 should equal (p)
  }
  
  test("Person - Default Values for missing fields") {
    val p: PersonNoAge = PersonNoAge("Bob")
    val bytes: BYTES = serialize(p)
    val p2: Person = deserialize[Person](bytes)
    
    p2.name should equal (p.name)
    p2.age should equal (18)
    p2.blah should equal ("blah")
  }
  
  test("Linked Person - 1") {
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), null)
    val bytes: BYTES = serialize(p)
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  test("Linked Person - 2") {    
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), LinkedPerson(Person("two", 2, "2"), null))
    val bytes: BYTES = serialize(p)    
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  test("Linked Person - 3") {    
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), LinkedPerson(Person("two", 2, "2"), LinkedPerson(Person("three", 3, "3"), null)))
    val bytes: BYTES = serialize(p)
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  //===============================================================================================
  // Option Testing
  //===============================================================================================
  case class OptionalFoo(string: Option[String], int: Option[Int])
  
  test("Option Handling - None") {
    val foo: OptionalFoo = OptionalFoo(None, None)
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFoo = deserialize[OptionalFoo](bytes)
    
    foo should equal (foo2)
  }
  
  test("Option Handling - Some") {
    val foo: OptionalFoo = OptionalFoo(Some("Hello World!"), Some(123))
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFoo = deserialize[OptionalFoo](bytes)
    
    foo should equal (foo2)
  }
  
  case class OptionalFooWithDefaults(string: Option[String] = Some("default"), int: Option[Int] = Some(123456789))
  
  test("Option Handling with Defaults - None") {
    val foo: OptionalFooWithDefaults = OptionalFooWithDefaults(None, None)
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFooWithDefaults = deserialize[OptionalFooWithDefaults](bytes)
    
    foo should equal (foo2)
  }
  
  test("Option Handling with Defaults - Some") {
    val foo: OptionalFooWithDefaults = OptionalFooWithDefaults(Some("Hello World!"), Some(123))
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFooWithDefaults = deserialize[OptionalFooWithDefaults](bytes)
    
    foo should equal (foo2)
  }
  
  //===============================================================================================
  // Complex Object Testing
  //===============================================================================================
  
  case class Foo (
    string: String = "Hello World!",
    int: Int = 1234,
    long: Long = 12345678912345L,
    float: Float = 3.14159f,
    double: Double = 3.14159d,
    bool: Boolean = true,
    date: Date = new Date(),
    bigInteger: JavaBigInteger = new JavaBigInteger("123456789012345678901234567890"),
    bigDecimal: JavaBigDecimal = new JavaBigDecimal("12345678901234.5678901234567890"),
    intList: Seq[Int] = List(1,2,3,4,5,6,7,8,9,10),
    stringList: Vector[String] = Vector("one", "two", "three", "four", ""),
    emptyList: Seq[String] = Nil,
    stringOpt: Option[String] = Some("Hello"),
    stringOptNone: Option[String] = None,
    listOpt: Option[List[Int]] = Some(List(1,2,3)),
    listOptNone: Option[List[String]] = None,
    tuple: (Int, String, Double) = (111, "222", 333.333),
    map: Map[String,Int] = Map("foo" -> 1, "bar" -> 2),
    bar: Bar = Bar(),
    barOpt: Option[Bar] = Some(Bar(next = Some(Bar("next", 321, Some(Bar()))))),
    barList: List[Bar] = List(Bar("one", 1), Bar("two", 2, Some(Bar("nested", 999, Some(Bar("nest2", 111))))), Bar("three", 3)),
    foo: Option[Foo] = Some(Foo(foo = None))
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
  )
  
  private def IntLengths: Vector[Int] = Vector(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000)
  private def LongLengths: Vector[Long] = Vector(1L,10L,100L,1000L,10000L,100000L,1000000L,10000000L,100000000L,1000000000L,10000000000L,100000000000L,1000000000000L,10000000000000L,100000000000000L,1000000000000000L,10000000000000000L,100000000000000000L,1000000000000000000L)
  
  case class Bar(
    string: String = "bar",
    int: Int = 123,    
    next: Option[Bar] = None,
    file: File = new File("/foo/bar/file.ext"),
    //
    // Using this class for overflow since it would put Foo over the 22 case class param limit:
    //
    emptyString: String = "",
    nullString: String = null,
    specialChars: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC   World!",
    minInt: Int = Int.MinValue,
    maxInt: Int = Int.MaxValue,
    minLong: Long = Long.MinValue,
    maxLong: Long = Long.MaxValue,
    intLengthChecks: Seq[Int] = IntLengths ++ Vector(Int.MinValue, Int.MaxValue),
    intLengthChecksNeg: Seq[Int] = IntLengths.map{ _ * -1 },
    longLengthChecks: Seq[Long] = LongLengths ++ Vector(Long.MinValue, Long.MaxValue),
    longLengthChecksNeg: Seq[Long] = LongLengths.map{ _ * -1 },
    javaInt: Integer = Integer.valueOf(987654),
    javaIntNull: Integer = null,
    // This is 120 chars long.  We should end up prefixing with a 2-byte varint that is padded since the max this string could take up is 120 * 3 (up to 3 bytes per char)
    stringPrefixCheck: String = "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789",
    longString: String = "abcdefghijklmnopqrstuvwxyz"*8190, // This should blow past the size of any output buffer to trigger slow string write paths
    multiByteLongString: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC   World!"*1024,
    anothermultiByteLongString: String = "\u0024\u00A2\u20AC"*8190,
    // Should deserialize as a Vector
    iterable: Iterable[String] = List("one","two","three")
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
  )
  
  case class MostlyEmptyFoo(@Field(19) bar: Bar)
  
  test("Foo") {
    val foo: Foo = Foo()
    val bytes: BYTES = serialize(foo)    
    val foo2: Foo = deserialize[Foo](bytes)
    
    foo2.foo should equal (foo.foo)
    
    foo2 should equal (foo)
    
    // Iterable doesn't have a CanBuildFrom so we default to using a Vector
    require(foo2.bar.iterable.isInstanceOf[Vector[String]])
  }
  
  test("Foo - Skipping unknown fields") {
    val foo: Foo = Foo()
    val bytes: BYTES = serialize(foo)
    val emptyFoo: MostlyEmptyFoo = deserialize[MostlyEmptyFoo](bytes)
    
    emptyFoo.bar should equal (foo.bar)
  }
  
  //===============================================================================================
  // Annotated Object Testing
  //===============================================================================================
  
  case class Car (year: Int = 1234, make: String = "default make", model: String = "default model")
  
  case class CarReversed(
    @Field(3, "model") the_model: String,
    @Field(2, "make") the_make: String,
    @Field(1, "year") the_year: Int
  )
  
  case class CarWithJustYear(year: Int = 4321)
  
  test("Car -> CarReversed") {
    val car: Car = Car(2005, "Subaru", "Legacy")
    val bytes: BYTES = serialize(car)
    val car2: CarReversed = deserialize[CarReversed](bytes)
    
    car2.the_year should equal (car.year)
    car2.the_make should equal (car.make)
    car2.the_model should equal (car.model)
  }
  
  test("CarReversed -> Car") {
    val car: CarReversed = CarReversed("Legacy", "Subaru", 2005)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    car2.year should equal (car.the_year)
    car2.make should equal (car.the_make)
    car2.model should equal (car.the_model)
  }
  
  //===============================================================================================
  // Nulls
  //===============================================================================================
  test("Car with nulls should retain nulls") {
    val car: Car = Car(2005, null, null)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    car should equal (car2)
  }
  
  test("Default values should be used for missing fields") {
    val car: CarWithJustYear = CarWithJustYear(2005)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    car2.year should equal (2005)
    car2.make should equal ("default make")
    car2.model should equal ("default model")
  }
  
  //===============================================================================================
  // Case-like Classes
  //===============================================================================================
  
  class CaseLike(val name: String, var age: Int)
  
  test("Case-Like Class") {
    val caseLike = new CaseLike("foo", 321)
    
    val bytes: BYTES = serialize(caseLike)
    val caseLike2: CaseLike = deserialize[CaseLike](bytes)
    
    caseLike2.name should equal (caseLike.name)
    caseLike2.age should equal (caseLike.age)
  }
  
  //===============================================================================================
  // Java Beans
  //===============================================================================================
  test("FooJavaBean") {
    val foo: FooJavaBean = new FooJavaBean()
    foo.setName("Hello World")
    foo.setNumber(123)
    foo.setBool(true)
    foo.setFooEnum(FooEnum.Bar)
    foo.setList(Vector("aa", "bb", "cc").asJava)
    foo.getListWithoutSetter().addAll(Vector("One", "Two", "Three").asJava)
    foo.setIgnoredField1("ignored1")
    foo.setIgnoredField2("ignored2")
    foo.setIgnoredField4("ignored4")
    
    val bytes: BYTES = serialize(foo)
    val foo2: FooJavaBean = deserialize[FooJavaBean](bytes)
    
    foo2.getName should equal (foo.getName)
    foo2.getNumber should equal (foo.getNumber)
    foo2.isBool should equal (foo.isBool)
    foo2.getFooEnum should equal (foo.getFooEnum)
    foo2.getList.asScala should equal (foo.getList.asScala)
    foo2.getListWithoutSetter.asScala should equal (foo.getListWithoutSetter.asScala)
    foo2.getIgnoredField1 should equal (null)
    foo2.getIgnoredField2 should equal (null)
    
    // This will fail until proper java transient field detection is in place
    foo2.getIgnoredField4 should equal (null)
  }
  
  //===============================================================================================
  // Java Beans Immutable
  //===============================================================================================
  test("FooJavaBeanImmutable") {
    val foo: FooJavaBeanImmutable = new FooJavaBeanImmutable("Hello World", 123, true, FooEnum.Bar, Vector("aa", "bb", "cc").asJava)
    
    val bytes: BYTES = serialize(foo)
    val foo2: FooJavaBeanImmutable = deserialize[FooJavaBeanImmutable](bytes)
    
    foo2.getName should equal (foo.getName)
    foo2.getNumber should equal (foo.getNumber)
    foo2.isBool should equal (foo.isBool)
    foo2.getFooEnum should equal (foo.getFooEnum)
    foo2.getList.asScala should equal (foo.getList.asScala)
  }
  
  //===============================================================================================
  // Serializing a Trait based on a fields from a Case Class
  //===============================================================================================
  
  trait AbstractPerson {
    def name: String
    def age: Int
    def linked: ConcretePerson // TODO: make this work with an AbstractPerson type
  }
  
  case class ConcretePerson (name: String, age: Int, linked: ConcretePerson) extends AbstractPerson
  class ConcretePersonAlt (val name: String, val age: Int, val linked: ConcretePerson) extends AbstractPerson
  
  test("Iface / Concrete") {
    implicit val ser: Serializer[AbstractPerson] = ObjectSerializer.forInterface[AbstractPerson, ConcretePerson]()
    implicit val deser: Deserializer[ConcretePerson] = ObjectDeserializer[ConcretePerson]()

    val person: AbstractPerson = new ConcretePersonAlt("foo", 123, new ConcretePerson("linked", 321, null))
    
    val bytes: BYTES = serialize(person)
    val concrete: ConcretePerson = deserialize[ConcretePerson](bytes)
    
    concrete.name should equal (person.name)
    concrete.age should equal (person.age)
    concrete.linked.name should equal (person.linked.name)
    concrete.linked.age should equal (person.linked.age)
    concrete.linked.linked should equal (null)

  }
  
  //===============================================================================================
  // Specifying a Serializer/Deserializer via @Field annotations
  //===============================================================================================
  
  case class NonCompressedNums(
    @Field(1, fm.serializer.Primitive.fixedInt) fixedInt: Int,
    @Field(2, fm.serializer.Primitive.fixedLong) fixedLong: Long
  )
  
  test("Specifying FixedInt/FixedLong instead of the default int/long serializer") {
    val obj = NonCompressedNums(123, 321L)
    
    val bytes: BYTES = serialize(obj)
    
    if (bytes.isInstanceOf[Array[Byte]]) {
      val b: Array[Byte] = bytes.asInstanceOf[Array[Byte]]
      println("BYTES: "+b.toSeq)
    }

    val obj2: NonCompressedNums = deserialize[NonCompressedNums](bytes)
    
    obj2.fixedInt should equal (obj.fixedInt)
    obj2.fixedLong should equal (obj.fixedLong)
  }
}