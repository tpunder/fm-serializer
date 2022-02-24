package fm.serializer

class SerializerObj {
  /**
   * Call this for creating an "implicit val" that can be picked up by serialize() calls
   */
  def make[T]: Serializer[T] = macro Macros.makeSerializerNoImplicits[T]
}