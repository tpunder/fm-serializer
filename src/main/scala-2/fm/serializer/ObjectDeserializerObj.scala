package fm.serializer

class ObjectDeserializerObj {
  def apply[T](): ObjectDeserializer[T] = macro Macros.makeObjectDeserializer[T]
  def apply[T](field: Field, fields: Field*): ObjectDeserializer[T] = macro Macros.makeObjectDeserializerFromFields[T]
}