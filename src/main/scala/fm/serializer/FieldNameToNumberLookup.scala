package fm.serializer

import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap

object FieldNameToNumberLookup {
  val empty: FieldNameToNumberLookup = new FieldNameToNumberLookup()
}

final class FieldNameToNumberLookup(values: (String,Int)*) {
  private val nameToNumberMap: Object2IntOpenHashMap[String] = new Object2IntOpenHashMap(values.size)
  private val numberToNameMap: Int2ObjectOpenHashMap[String] = new Int2ObjectOpenHashMap(values.size)

  // Add all values to our map
  values.foreach { case (fieldName: String, fieldNumber: Int) =>
    nameToNumberMap.put(fieldName, fieldNumber)
    numberToNameMap.put(fieldNumber, fieldName)
  }

  // Also add lowercase values if there are no conflicts
  values.foreach { case (fieldName: String, fieldNumber: Int) =>
    val lowerFieldName: String = fieldName
    if (!nameToNumberMap.containsKey(lowerFieldName)) nameToNumberMap.put(lowerFieldName, fieldNumber)
  }

  def getFieldNameOrDefault(fieldNumber: Int, default: String): String = {
    numberToNameMap.getOrDefault(fieldNumber, default)
  }

  def getFieldNumberOrDefault(fieldName: String, default: Int): Int = {
    if (null == fieldName || fieldName == "") return default

    // Lookup exactly what was passed in
    var res: Int = nameToNumberMap.getOrDefault(fieldName, default)

    // If that fails then try looking up the lowercase version
    if (res == default) res = nameToNumberMap.getOrDefault(fieldName.toLowerCase, default)

    // Return whatever we have
    res
  }
}
