package snowdrop

import io.circe.Json
import scala.annotation.tailrec
import io.circe.JsonObject

object MaskJson {
  final val StringMask = "*****"

  def apply(json: Json): Json = applyWithCustomMask(json)(Json.fromString(StringMask))

  def applyWithCustomMask(json: Json)(customMask: Json): Json = 
    if(json.isArray) 
        json.mapArray(maskVectorJson(_)(customMask))
      else if(json.isObject){
        json.mapObject(maskJsonObject(_)(customMask))
      }
      else
        customMask

  private def maskVectorJson(vector: Vector[Json])(customMask: Json): Vector[Json] = {
    @tailrec
    def vectorLoop(unmaskedVector: Vector[Json], maskedVector: Vector[Json]): Vector[Json] = {
      if(unmaskedVector.isEmpty)
        maskedVector
      else
        vectorLoop(unmaskedVector.tail, maskedVector.appended(applyWithCustomMask(unmaskedVector.head)(customMask)))
    }
    vectorLoop(vector, Vector.empty)
  }


  private def maskJsonObject(jobj: JsonObject)(customMask: Json): JsonObject = {
    @tailrec
    def objectLoop(unmaskedObject: Vector[(String, Json)], maskedObject: Vector[(String, Json)]): Vector[(String, Json)] = {
       if(unmaskedObject.isEmpty)
         maskedObject
       else {
          val (key: String, valueToMask: Json) = unmaskedObject.head
          val maskedValue = applyWithCustomMask(valueToMask)(customMask)
          val remainingUnmaskedObject: Vector[(String, Json)] = unmaskedObject.tail
          val newMaskedObject: Vector[(String, Json)] = maskedObject.appended(key -> maskedValue)
          
          objectLoop(remainingUnmaskedObject, newMaskedObject)  
       }
      
    }

    JsonObject.fromIterable(objectLoop(jobj.toVector, Vector.empty))
  }



}

