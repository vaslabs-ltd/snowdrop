package snowdrop

import io.circe.Json
import scala.annotation.tailrec
import io.circe.JsonObject

object MaskJson {

  final val StringMask = "*****"

  def apply(json: Json): Json =
    applyWithCustomMask(json)(Json.fromString(StringMask), None)

  def applyWithCustomMask(json: Json)(
      customMask: Json = Json.fromString(StringMask),
      numberMasking: Option[Json] = None
  ): Json =
    if (json.isArray)
      json.mapArray(maskVectorJson(_)(customMask, numberMasking))
    else if (json.isObject) {
      json.mapObject(maskJsonObject(_)(customMask, numberMasking))
    } else if (json.isNumber) {
      numberMasking.getOrElse(json)
    } else if (json.isString) {
      customMask
    } else
      json

  private def maskVectorJson(
      vector: Vector[Json]
  )(customMask: Json, numberMasking: Option[Json]): Vector[Json] = {
    @tailrec
    def vectorLoop(
        unmaskedVector: Vector[Json],
        maskedVector: Vector[Json]
    ): Vector[Json] = {
      if (unmaskedVector.isEmpty)
        maskedVector
      else
        vectorLoop(
          unmaskedVector.tail,
          maskedVector.appended(
            applyWithCustomMask(unmaskedVector.head)(customMask, numberMasking)
          )
        )
    }
    vectorLoop(vector, Vector.empty)
  }

  private def maskJsonObject(
      jobj: JsonObject
  )(customMask: Json, numberMasking: Option[Json]): JsonObject = {
    @tailrec
    def objectLoop(
        unmaskedObject: Vector[(String, Json)],
        maskedObject: Vector[(String, Json)]
    ): Vector[(String, Json)] = {
      if (unmaskedObject.isEmpty)
        maskedObject
      else {
        val (key: String, valueToMask: Json) = unmaskedObject.head
        val maskedValue =
          applyWithCustomMask(valueToMask)(customMask, numberMasking)
        val remainingUnmaskedObject: Vector[(String, Json)] =
          unmaskedObject.tail
        val newMaskedObject: Vector[(String, Json)] =
          maskedObject.appended(key -> maskedValue)

        objectLoop(remainingUnmaskedObject, newMaskedObject)
      }

    }

    JsonObject.fromIterable(objectLoop(jobj.toVector, Vector.empty))
  }

}
