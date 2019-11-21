package fm.serializer.validation

import fm.common.Implicits._
import fm.serializer.{CollectionInput, Deserializer, FieldInput, Input}
import java.math.BigInteger
import scala.collection.mutable.Builder

object ValidationInput {
  private[validation] case object AbortValidationException extends Throwable
}

/**
 * This wraps an Input and provides validation information
 */
final class ValidationInput(self: Input, options: ValidationOptions) extends Input {
  private[this] val errorsBuilder: Builder[ValidationError,Vector[ValidationError]] = Vector.newBuilder
  private[this] var path: String = ""
  private[this] var fieldNumber: Int = 0
  private[this] var fieldName: String = ""

  def result(): ValidationResult = {
    val errors: IndexedSeq[ValidationError] = errorsBuilder.result()
    // Note: The errors.distinct removes duplicate errors that can occur when parsing arrays
    //       if each element trying to be parsed has the same error.
    if (errors.isEmpty) ValidationResult.Success else ValidationResult.Failure(errors.distinct)
  }

  override def readFieldNumber(nameToNumMap: Map[String, Int]): Int = {
    val res: Int = self.readFieldNumber(nameToNumMap)
    fieldNumber = lastFieldNumber()
    fieldName = lastFieldName()

    // If the fieldNumber is not set but we have a fieldName then try to lookup the fieldNumber from the nameToNumMap
    if (fieldName.isNotNullOrBlank && fieldNumber <= 0) {
      fieldNumber = nameToNumMap.get(fieldName) orElse { nameToNumMap.find{ case (name: String, _: Int) => fieldName equalsIgnoreCase name }.map{ _._2 } } getOrElse fieldNumber
    }

    // If the fieldName is not set but we have a fieldNumber then try to lookup what the fieldName from the nameToNumMap
    if (fieldName.isNullOrBlank && fieldNumber > 0) {
      fieldName = nameToNumMap.find{ case (_: String, num: Int) => num === fieldNumber }.map{ _._1 }.getOrElse(fieldName)
    }

    res
  }

  override def skipUnknownField(): Unit = {
    if (!options.ignoreUnknownFields) errorsBuilder += ValidationError.UnknownField(path, fieldNumber, fieldName)
    self.skipUnknownField()
  }

  override def reportUnsetField[T](number: Int, name: String, hasUserDefinedDefaultValue: Boolean, deserializer: Deserializer[T]): Unit = {
    var report: Boolean = !options.ignoreUnsetFields

    report &&= (!hasUserDefinedDefaultValue || options.reportUnsetFieldsWithDefaultValues)
    report &&= (options.reportUnsetOptionFields || deserializer.defaultValue != None)

    if (report) errorsBuilder += ValidationError.MissingField(path, number, name)

    self.reportUnsetField(number, name, hasUserDefinedDefaultValue, deserializer)
  }

  override def readNestedBool(): Boolean = handlePrimitive(false){ self.readNestedBool() }
  override def readNestedFloat(): Float = handlePrimitive(0F){ self.readNestedFloat() }
  override def readNestedDouble(): Double = handlePrimitive(0D){ self.readNestedDouble() }
  override def readNestedString(): String = handlePrimitive[String](null){ self.readNestedString() }
  override def readNestedBigInteger(): BigInteger = handlePrimitive[BigInteger](null){ self.readNestedBigInteger() }
  override def readNestedBigDecimal(): java.math.BigDecimal = handlePrimitive[java.math.BigDecimal](null){ self.readNestedBigDecimal() }
  override def readNestedByteArray(): Array[Byte] = handlePrimitive[Array[Byte]](null){ self.readNestedByteArray() }
  override def readNestedInt(): Int = handlePrimitive(0){ self.readNestedInt() }
  override def readNestedUnsignedInt(): Int = handlePrimitive(0){ self.readNestedUnsignedInt() }
  override def readNestedSignedInt(): Int = handlePrimitive(0){ self.readNestedSignedInt() }
  override def readNestedFixedInt(): Int = handlePrimitive(0){ self.readNestedFixedInt() }
  override def readNestedLong(): Long = handlePrimitive(0L){ self.readNestedLong() }
  override def readNestedUnsignedLong(): Long = handlePrimitive(0L){ self.readNestedUnsignedLong() }
  override def readNestedSignedLong(): Long = handlePrimitive(0L){ self.readNestedSignedLong() }
  override def readNestedFixedLong(): Long = handlePrimitive(0L){ self.readNestedFixedLong() }

  override def readNestedObject[T](f: FieldInput => T): T = handleObject(null.asInstanceOf[T]) {
    self.readNestedObject{ other: FieldInput =>
      assertSelf(other)
      f(this)
    }
  }

  override def readNestedCollection[T](f: CollectionInput => T): T = handleCollection(null.asInstanceOf[T]) {
    self.readNestedCollection{ other: CollectionInput =>
      assertSelf(other)
      f(this)
    }
  }

  override def readRawBool(): Boolean = handlePrimitive(false){ self.readRawBool() }
  override def readRawFloat(): Float = handlePrimitive(0F){ self.readRawFloat() }
  override def readRawDouble(): Double = handlePrimitive(0D){ self.readRawDouble() }
  override def readRawString(): String = handlePrimitive[String](null){ self.readRawString() }
  override def readRawBigInteger(): BigInteger = handlePrimitive[BigInteger](null){ self.readRawBigInteger() }
  override def readRawBigDecimal(): java.math.BigDecimal = handlePrimitive[java.math.BigDecimal](null){ self.readRawBigDecimal() }
  override def readRawByteArray(): Array[Byte] = handlePrimitive[Array[Byte]](null){ self.readRawByteArray() }
  override def readRawInt(): Int = handlePrimitive(0){ self.readRawInt() }
  override def readRawUnsignedInt(): Int = handlePrimitive(0){ self.readRawUnsignedInt() }
  override def readRawSignedInt(): Int = handlePrimitive(0){ self.readRawSignedInt() }
  override def readRawFixedInt(): Int = handlePrimitive(0){ self.readRawFixedInt() }
  override def readRawLong(): Long = handlePrimitive(0L){ self.readRawLong() }
  override def readRawUnsignedLong(): Long = handlePrimitive(0L){ self.readRawUnsignedLong() }
  override def readRawSignedLong(): Long = handlePrimitive(0L){ self.readRawSignedLong() }
  override def readRawFixedLong(): Long = handlePrimitive(0L){ self.readRawFixedLong() }

  override def readRawObject[T](f: FieldInput => T): T = handleObject(null.asInstanceOf[T]) {
    self.readRawObject{ other: FieldInput =>
      assertSelf(other)
      f(this)
    }
  }

  override def readRawCollection[T](f: CollectionInput => T): T = handleCollection(null.asInstanceOf[T]) {
    self.readRawCollection{ other: CollectionInput =>
      assertSelf(other)
      f(this)
    }
  }

  //
  // These are all just pass through
  //

  override def nextValueIsNull: Boolean = self.nextValueIsNull
  override def hasAnotherElement: Boolean = self.hasAnotherElement
  override def readFieldName(): String = self.readFieldName()
  override def allowStringMap: Boolean = self.allowStringMap
  override def lastFieldName(): String = self.lastFieldName()
  override def lastFieldNumber(): Int = self.lastFieldNumber()

  //
  // Helpers
  //

  @inline private def handleObject[T](default: T)(f: => T): T = {
    val prevPath: String = path
    val prevFieldNumber: Int = fieldNumber
    val prevFieldName: String = fieldName

    if (path.isNullOrBlank) path = fieldName else path = path+"."+fieldName

    try {
      f
    } catch {
      case ex: Exception =>
        errorsBuilder += ValidationError.ObjectError(path, fieldNumber, fieldName)(ex.getMessage)
        throw ValidationInput.AbortValidationException
    } finally {
      path = prevPath
      fieldNumber = prevFieldNumber
      fieldName = prevFieldName
    }
  }

  @inline private def handleCollection[T](default: T)(f: => T): T = {
    try {
      f
    } catch {
      case ex: Exception =>
        errorsBuilder += ValidationError.CollectionError(path, fieldNumber, fieldName)(ex.getMessage)
        throw ValidationInput.AbortValidationException
    }
  }

  @inline private def handlePrimitive[T](default: T)(f: => T): T = {
    try {
      f
    } catch {
      case ex: Exception =>
        errorsBuilder += ValidationError.PrimitiveError(path, fieldNumber, fieldName)(ex.getMessage)
        throw ValidationInput.AbortValidationException
    }
  }

  private def assertSelf(other: AnyRef): Unit = {
    assert(other eq self, "Expected other input to be the same instance as self for validation.  Self: "+self+"  Other: "+other)
  }
}
