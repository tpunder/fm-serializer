package fm.serializer

class SerializerObj {
  /**
   * Call this for creating an "implicit val" that can be picked up by serialize() calls
   */
  inline def make[T]: Serializer[T] = ${ Macros.makeSerializer[T] }
}