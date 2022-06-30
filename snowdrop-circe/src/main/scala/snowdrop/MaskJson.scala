package snowdrop

import io.circe.Json
import scala.annotation.tailrec

object MaskJson {
  final val StringMask = "*****"
  def apply(json: Json): Json = {
    if(json.isArray) 
      json.mapArray(maskVectorJson)
    else
      Json.fromString(StringMask)
  }

  private def maskVectorJson(vector: Vector[Json]): Vector[Json] = {
    @tailrec
    def vectorLoop(unmaskedVector: Vector[Json], maskedVector: Vector[Json]): Vector[Json] = {
      if(unmaskedVector.isEmpty)
        maskedVector
      else
        vectorLoop(unmaskedVector.tail, maskedVector.appended(apply(unmaskedVector.head)))
    }
    vectorLoop(vector, Vector.empty)
  }

}