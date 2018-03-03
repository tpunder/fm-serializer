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

import fm.common.{IP, ImmutableArray, ImmutableDate, UUID}
import java.io.File
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.LocalDate
import java.util.{Calendar, Date}
import org.bson.types.ObjectId
import org.scalatest.{FunSuite, Matchers}
import scala.collection.JavaConverters._

trait TestSerializer[BYTES] extends FunSuite with Matchers {
  // Does the serialization method support serializing raw collections
  def supportsRawCollections: Boolean = true

  private val RepeatFactor: Int = 1024
  private val LongRepeatFactor: Int = 8190

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
    
    if (!ignoreNullRetainTest) foo should equal (foo2)
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
    specialChars: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!",
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
    longString: String = "abcdefghijklmnopqrstuvwxyz"*LongRepeatFactor, // This should blow past the size of any output buffer to trigger slow string write paths
    multiByteLongString: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"*RepeatFactor,
    anothermultiByteLongString: String = "\u0024\u00A2\u20AC"*LongRepeatFactor,
    baz: Baz = Baz()
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
  )
  
  // Additional overflow since Foo & Bar have 22 items
  case class Baz(
    // Should deserialize as a Vector
    iterable: Iterable[String] = List("one","two","three"),
    children: scala.collection.IndexedSeq[Baz] = Vector(Baz(children = Vector.empty), Baz(children = Vector.empty)),
    indexedSeq: IndexedSeq[String] = Vector("foo0","bar0","baz0"),
    scalaIndexedSeq: scala.IndexedSeq[String] = Vector("foo1","bar1","baz1"),
    collectionIndexedSeq: scala.collection.IndexedSeq[String] = Vector("foo2","bar2","baz2"),
    immutableIndexedSeq: scala.collection.immutable.IndexedSeq[String] = Vector("foo3","bar3","baz3"),
    mutableIndexedSeq: scala.collection.mutable.IndexedSeq[String] = scala.collection.mutable.IndexedSeq("foo4","bar4","baz4"),
    emptyIndexedSeq: IndexedSeq[String] = IndexedSeq.empty,
    emptyVector: Vector[String] = Vector.empty,
    char: Char = 'A',
    calendar: Calendar = Calendar.getInstance,
    calendarNull: Calendar = null,
    dateNull: Date = null,
    localDate: LocalDate = LocalDate.now,
    localDateNull: LocalDate = null,
    bsonTypes: BsonTypes = BsonTypes(),
    fmCommonTypes: FMCommonTypes = FMCommonTypes(),
    //supplementaryCharacters: SupplementaryCharacters = SupplementaryCharacters()
  )

  // Supplementary Characters as represented in Java as 2 characters but need
  // to be converted to a single UTF-8 character (1-4 bytes) when we serialize
  // http://www.oracle.com/us/technologies/java/supplementary-142654.html
  case class SupplementaryCharacters(
    single: String = "\uD83D\uDCA5", // "💥"
    mixed: String = "foo\uD83D\uDCA5bar", // "foo💥bar"
    repeatedMixed: String = "foo\uD83D\uDCA5bar" * 10,
    emojis: String = new String(Array(0x1F600, 0x1F63A, 0x1F9D7, 0x1F1FA, 0x1F1F8).flatMap{ Character.toChars(_) }), // "😀😺🧗🇺🇸"
    repeatedEmojis: String = new String(Array(0x1F600, 0x1F63A, 0x1F9D7, 0x1F1FA, 0x1F1F8).flatMap{ Character.toChars(_) }) * RepeatFactor,
    moreMixed: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!",
    repeatedMoreMixed: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!" * RepeatFactor
  )

  case class BsonTypes(
    objectId: ObjectId = new ObjectId(),
    objectIdNull: ObjectId = null,
    someObjectId: Option[ObjectId] = Some(new ObjectId()),
    noneObjectId: Option[ObjectId] = None,
    objectIds: Vector[ObjectId] = Vector(new ObjectId(), new ObjectId(), new ObjectId(), new ObjectId())
    // Nulls within collections aren't fully supported yet
    //objectIdsWithNulls: Vector[ObjectId] = Vector(new ObjectId(), null, new ObjectId(), null)
  )

  case class FMCommonTypes(
    immutableDate: ImmutableDate = ImmutableDate(),
    immutableDateNull: ImmutableDate = null,
    someImmutableDate: Option[ImmutableDate] = Some(ImmutableDate()),
    noneImmutableDate: Option[ImmutableDate] = None,
    immutableDates: Vector[ImmutableDate] = Vector(ImmutableDate(1), ImmutableDate(2), ImmutableDate()),
    // Nulls within collections aren't fully supported yet
    //immutableDatesNull: Vector[ImmutableDate] = Vector(ImmutableDate(1), ImmutableDate(2), null),
    ipMin: IP = IP.empty,
    ipMid: IP = IP("123.123.123.123"),
    ipMax: IP = IP("255.255.255.255"),
    ipNone: Option[IP] = None,
    ipSome: Option[IP] = Some(IP("192.168.0.1")),
    bytes: ImmutableArray[Byte] = ImmutableArray.wrap("Hello World!".getBytes(UTF_8)),
    moreBytes: ImmutableArray[Byte] = ImmutableArray.wrap(("Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"*RepeatFactor).getBytes(UTF_8)),
    bytesNull: ImmutableArray[Byte] = null,
    immutableArrayInts: ImmutableArray[Int] = ImmutableArray(1,2,3,4,Int.MinValue,Int.MaxValue),
    immutableArrayLongs: ImmutableArray[Long] = ImmutableArray(1,2,3,4,Int.MinValue,Int.MaxValue,Long.MinValue,Long.MaxValue),
    immutableArrayStrings: ImmutableArray[String] = ImmutableArray("one","two","three","four"),
    emptyImmutableArrayInt: ImmutableArray[Int] = ImmutableArray.empty,
    emptyImmutableArrayLong: ImmutableArray[Long] = ImmutableArray.empty,
    emptyImmutableArrayString: ImmutableArray[String] = ImmutableArray.empty,
    uuid: UUID = UUID(),
    uuidNull: UUID = null,
    uuidSome: Option[UUID] = Some(UUID()),
    uuidNone: Option[UUID] = None
  )

  case class MostlyEmptyFoo(@Field(19) bar: Bar)
  
  test("Foo") {
    val foo: Foo = Foo()
    val bytes: BYTES = serialize(foo)
    val foo2: Foo = deserialize[Foo](bytes)

    // TestMinimalJSON doesn't play well with this
    if (!ignoreNullRetainTest) {
      foo2.foo should equal (foo.foo)
      
      foo2 should equal (foo)
    }
    
    // Iterable doesn't have a CanBuildFrom so we default to using a Vector
    require(foo2.bar.baz.iterable.isInstanceOf[Vector[String]])
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
  def ignoreNullRetainTest: Boolean = false
  
  test("Car with nulls should retain nulls") {
    val car: Car = Car(2005, null, null)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    // In TestMinimalJSON we ignore this check since we don't output nulls
    // and end up using the default values
    if (!ignoreNullRetainTest) car should equal (car2)
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


  test("FooJavaBeanContainer") {
    val container: FooJavaBeanContainer = new FooJavaBeanContainer()
    
    val bytes: BYTES = serialize(container)
    val container2: FooJavaBeanContainer = deserialize[FooJavaBeanContainer](bytes)
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
    foo.setChildren(Vector({
      val child: FooJavaBean = new FooJavaBean()
      child.setName("Hello World Child")
      child
    }).asJava)
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
    foo2.getChildren.get(0).getName() should equal(foo.getChildren.get(0).getName())
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
    
//    if (bytes.isInstanceOf[Array[Byte]]) {
//      val b: Array[Byte] = bytes.asInstanceOf[Array[Byte]]
//      println("BYTES: "+b.toSeq)
//    }

    val obj2: NonCompressedNums = deserialize[NonCompressedNums](bytes)
    
    obj2.fixedInt should equal (obj.fixedInt)
    obj2.fixedLong should equal (obj.fixedLong)
  }
  
  //===============================================================================================
  // Nested Collection Serializer (GitHub issue #5: https://github.com/frugalmechanic/fm-serializer/issues/5)
  //===============================================================================================
  
  case class Node(name: String, children: IndexedSeq[Node] = Vector.empty)
  
  test("Nested Nodes - Single Node") {
    val node: Node = Node("one", children = Vector(Node("foo"), Node("bar")))
    
    val bytes: BYTES = serialize(node)
    val node2: Node = deserialize[Node](bytes)
    
    node should equal (node2)
  }

  test("Nested Nodes - Single Node - Multi Level Nesting") {
    val node: Node = Node("one", children = Vector(Node("foo", children = Vector(Node("asd"), Node("qwe"))), Node("bar")))

    val bytes: BYTES = serialize(node)
    val node2: Node = deserialize[Node](bytes)

    node should equal (node2)
  }

  if (supportsRawCollections) test("Nested Nodes - Multiple Nodes") {
    val n: Node = Node("one", children = Vector(Node("foo"), Node("bar")))

    val nodes: IndexedSeq[Node] = Vector(n, n, n)
    val bytes: BYTES = serialize(nodes)
    val nodes2: IndexedSeq[Node] = deserialize[IndexedSeq[Node]](bytes)

    nodes should equal (nodes2)
  }

  //===============================================================================================
  // Renamed Fields
  //===============================================================================================

  case class NonRenamed(foo: String, bar: Int)
  case class Renamed(@RenameField("foo") renamed: String, bar: Int)

  test("Renamed Fields") {
    val p: NonRenamed = NonRenamed("Hello World", 123)
    val renamed: Renamed = Renamed("Hello World", 123)

    val bytes: BYTES = serialize(p)
    val p2: Renamed = deserialize[Renamed](bytes)

    p2 should equal (renamed)
  }

  //===============================================================================================
  // AnyVal Classes
  //===============================================================================================

  test("AnyVal classes should pass through serialization to the single field they wrap") {
    val wrapped: WrappedHolder = WrappedHolder(WrappedInt(123))
    val raw: IntHolder = IntHolder(123)

    val wrappedBytes: BYTES = serialize(wrapped)

    deserialize[WrappedHolder](wrappedBytes) should equal (wrapped)
    deserialize[IntHolder](wrappedBytes) should equal (raw)

    val rawBytes: BYTES = serialize(wrapped)

    deserialize[WrappedHolder](rawBytes) should equal (wrapped)
    deserialize[IntHolder](rawBytes) should equal (raw)
  }

  //===============================================================================================
  // Overriden AnyVal Classes
  //===============================================================================================

  case class IPHolder(ip: IP)
  case class IPAsLong(ip: Long)

  test("IP Should serialize as a long using the manually defined Serializer instead of AnyVal") {
    val ip: IPHolder = IPHolder(IP("255.255.255.255"))
    val long: IPAsLong = IPAsLong(4294967295L)

    val ipBytes: BYTES = serialize(ip)

    deserialize[IPHolder](ipBytes) should equal (ip)
    deserialize[IPAsLong](ipBytes) should equal (long)

    val longBytes: BYTES = serialize(long)

    deserialize[IPHolder](longBytes) should equal (ip)
    deserialize[IPAsLong](longBytes) should equal (long)
  }
}

// Since this is an AnyVal it must be defined separately
case class WrappedInt(value: Int) extends AnyVal
case class IntHolder(count: Int)
case class WrappedHolder(count: WrappedInt)