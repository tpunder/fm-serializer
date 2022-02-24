package fm.serializer

class ObjectSerializerObj {
  def apply[T](): ObjectSerializer[T] = macro Macros.makeObjectSerializer[T]
  def apply[T](field: Field, fields: Field*): ObjectSerializer[T] = macro Macros.makeObjectSerializerFromFields[T]
  def forInterface[IFACE,CONCRETE](): ObjectSerializer[IFACE] = macro Macros.makeObjectSerializerForInterface[IFACE,CONCRETE]
}