package fm.serializer

trait SerializableCompanionBase {
  protected def makeSerializer[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
}