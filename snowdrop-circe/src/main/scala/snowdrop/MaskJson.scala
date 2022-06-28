package snowdrop

import io.circe.Json

object MaskJson {
  final val StringMask = "*****"

  case class Settings(numberMask: Json, stringMaskingSequence: Json)

  object Settings {

    def default =
      Settings(Json.fromString(StringMask), Json.fromString(StringMask))
    
    def apply(stringMaskingSequence: String): Settings =
      Settings(Json.fromString(StringMask), Json.fromString(stringMaskingSequence))
    
    def apply(intNumberMask: Int): Settings =
      Settings(Json.fromInt(intNumberMask), Json.fromString(StringMask))

  }

  def apply(json: Json, maskingSettings: Settings = Settings.default): Json =
    if (json.isNumber)
      maskingSettings.numberMask
    else
      maskingSettings.stringMaskingSequence
}
