package fm.serializer.validation

import fm.common.Implicits._

object ValidationError {
  final case class MissingField(path: String, fieldNumber: Int, fieldName: String) extends ValidationError {
    override protected def errorMessage: String = s"Missing Field '$fieldName'"
  }

  final case class UnknownField(path: String, fieldNumber: Int, fieldName: String) extends ValidationError {
    /** A name or number should be set */
    def fieldNameOrNumber: String = if (fieldName.isNotNullOrBlank) fieldName else fieldNumber.toString
    override protected def errorMessage: String = s"Unknown Field '$fieldNameOrNumber'"
  }

  final case class ObjectError(path: String, fieldNumber: Int, fieldName: String)(protected val errorMessage: String) extends ParseError
  final case class CollectionError(path: String, fieldNumber: Int, fieldName: String)(protected val errorMessage: String) extends ParseError
  final case class PrimitiveError(path: String, fieldNumber: Int, fieldName: String)(protected val errorMessage: String) extends ParseError

  sealed abstract class ParseError extends ValidationError
}

sealed abstract class ValidationError {
  def path: String
  def fieldNumber: Int
  def fieldName: String

  protected def errorMessage: String

  final def message: String = {
    val builder = Vector.newBuilder[String]

    if (errorMessage.isNotNullOrBlank) builder += s"Message: $errorMessage"
    if (path.isNotNullOrBlank) builder += s"Path: $path"
    if (fieldNumber > 0) builder += s"Field Number: $fieldNumber"
    if (fieldName.isNotNullOrBlank) builder += s"Field Name: $fieldName"

    builder.result().mkString(", ")
  }
}
