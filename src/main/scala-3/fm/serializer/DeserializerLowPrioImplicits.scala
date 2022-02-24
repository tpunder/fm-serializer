package fm.serializer

trait DeserializerLowPrioImplicits {
  implicit inline def makeDeserializer[T]: Deserializer[T] = ${ Macros.makeDeserializer[T] }
}