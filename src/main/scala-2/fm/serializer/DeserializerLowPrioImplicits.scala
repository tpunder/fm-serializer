package fm.serializer

trait DeserializerLowPrioImplicits {
  implicit def makeDeserializer[T]: Deserializer[T] = macro Macros.makeDeserializer[T]
}