package fm.serializer

trait SerializerLowPrioImplicits {
  /**
   * This should NOT be called directly (use Serializer.make instead).
   */
  implicit inline def implicitMakeSerializer[T]: Serializer[T] = ${ Macros.makeSerializer[T] }
}