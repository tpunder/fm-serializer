package fm.serializer

trait SerializerLowPrioImplicits {
  /**
   * This should NOT be called directly (use Serializer.make instead).
   */
  implicit def implicitMakeSerializer[T]: Serializer[T] = macro Macros.makeSerializerAllowImplicits[T]
}