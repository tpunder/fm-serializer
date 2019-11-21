package fm.serializer.validation

import fm.serializer.{Deserializer, Input}

object Validation {
  def validate[T](input: Input)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate(input, ValidationOptions.default)
  }

  def validate[T](input: Input, options: ValidationOptions)(implicit deserializer: Deserializer[T]): ValidationResult = {
    val validatingInput: ValidationInput = new ValidationInput(input, options)

    try {
      deserializer.deserializeRaw(validatingInput)
    } catch {
      case ValidationInput.AbortValidationException => // Should be logged in the ValidationInput so we can ignore here
    }

    validatingInput.result()
  }
}
