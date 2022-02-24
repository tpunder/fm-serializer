package fm.serializer

trait SerializableCompanionBase {
  protected inline def makeSerializer[T](): SimpleObjectSerializer[T] = ${ Macros.makeSimpleObjectSerializer[T]() }
}