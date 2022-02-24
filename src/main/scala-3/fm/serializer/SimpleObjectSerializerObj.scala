package fm.serializer

class SimpleObjectSerializerObj {
  final inline def make[T](): SimpleObjectSerializer[T] = ${ Macros.makeSimpleObjectSerializer[T]() }
}