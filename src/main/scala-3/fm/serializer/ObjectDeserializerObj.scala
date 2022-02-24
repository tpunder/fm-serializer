package fm.serializer

class ObjectDeserializerObj {
  inline def apply[T](): ObjectDeserializer[T] = ${ Macros.makeObjectDeserializer[T]() }
  inline def apply[T](inline field: Field, inline fields: Field*): ObjectDeserializer[T] = ${ Macros.makeObjectDeserializerFromFields[T]('field, 'fields) }
}