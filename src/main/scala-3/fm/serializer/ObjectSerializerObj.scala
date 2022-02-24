package fm.serializer

class ObjectSerializerObj {
  inline def apply[T](): ObjectSerializer[T] = ${ Macros.makeObjectSerializer[T]() }
  inline def apply[T](inline field: Field, inline fields: Field*): ObjectSerializer[T] = ${ Macros.makeObjectSerializerFromFields[T]('field, 'fields) }
  inline def forInterface[IFACE,CONCRETE](): ObjectSerializer[IFACE] = ${ Macros.makeObjectSerializerForInterface[IFACE,CONCRETE]() }
}