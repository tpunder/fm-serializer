package fm.serializer.validation

object ValidationResult {
  object Success extends ValidationResult {
    override def errors: IndexedSeq[ValidationError] = Vector.empty
    override def isSuccess: Boolean = true
    override def isFailure: Boolean = false
    override def toString: String = "ValidationResult.Success"
  }

  final case class Failure(errors: IndexedSeq[ValidationError]) extends ValidationResult {
    override def isSuccess: Boolean = false
    override def isFailure: Boolean = true
  }
}

sealed abstract class ValidationResult {
  def errors: IndexedSeq[ValidationError]
  def isSuccess: Boolean
  def isFailure: Boolean
}
