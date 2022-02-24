package fm.serializer

class SimpleObjectSerializerObj {
  final def make[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
}