package fm.serializer

import fm.serializer.json.JSON
import fm.serializer.protobuf.Protobuf

/**
 * Usage Pattern:
 * 
 * import fm.serializer.{SerializableCompanion, SerializableInstance, SimpleSerializer}
 * 
 * object Foo extends SerializableCompanion[Foo] {
 *   protected val serializer: SimpleSerializer[Foo] = makeSerializer[Foo]
 * }
 * 
 * final case class Foo(bar: String) extends SerializableInstance[Foo] {
 *   protected def companion: SerializableCompanion[Foo] = Foo
 * }
 */
trait SerializableCompanion[A] {
  protected def makeSerializer[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
  
  protected val serializer: SimpleSerializer[A]
  
  //
  // JSON Methods
  //
  def toJSON(v: A): String = JSON.toJSON(v)(serializer)
  def toMinimalJSON(v: A): String = JSON.toMinimalJSON(v)(serializer)
  def toPrettyJSON(v: A): String = JSON.toPrettyJSON(v)(serializer)
  
  def fromJSON(json: String): A = JSON.fromJSON(json)(serializer)
  
  //
  // Protobuf Methods
  //
  def toBytes(v: A): Array[Byte] = Protobuf.toBytes(v)(serializer)  
  def fromBytes(bytes: Array[Byte]): A = Protobuf.fromBytes(bytes)(serializer)
}

/**
 * Usage Pattern:
 * 
 * import fm.serializer.{SerializableCompanion, SerializableInstance, SimpleSerializer}
 * 
 * object Foo extends SerializableCompanion[Foo] {
 *   protected val serializer: SimpleSerializer[Foo] = makeSerializer[Foo]
 * }
 * 
 * final case class Foo(bar: String) extends SerializableInstance[Foo] {
 *   protected def companion: SerializableCompanion[Foo] = Foo
 * }
 */
trait SerializableInstance[A] { self: A =>
  protected def companion: SerializableCompanion[A]
  
  def toJSON: String = companion.toJSON(this)
  def toMinimalJSON: String = companion.toMinimalJSON(this)
  def toPrettyJSON: String = companion.toPrettyJSON(this)
  
  def toBytes: Array[Byte] = companion.toBytes(this)
}
