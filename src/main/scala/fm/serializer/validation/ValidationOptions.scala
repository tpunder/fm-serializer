package fm.serializer.validation

object ValidationOptions {
  val default: ValidationOptions = ValidationOptions(
    ignoreUnknownFields = false,
    ignoreUnsetFields = false,
    reportUnsetFieldsWithDefaultValues = false,
    reportUnsetOptionFields = false
  )
}

final case class ValidationOptions(
  ignoreUnknownFields: Boolean,
  ignoreUnsetFields: Boolean,
  reportUnsetFieldsWithDefaultValues: Boolean,
  reportUnsetOptionFields: Boolean
)