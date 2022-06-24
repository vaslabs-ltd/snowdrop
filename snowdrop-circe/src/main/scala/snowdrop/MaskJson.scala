package snowdrop

import io.circe.Json

object MaskJson {
  final val StringMask = "*****"
  def apply(json: Json): Json =
    Json.fromString(StringMask)
}
