package snowdrop

import io.circe.Json
import scala.annotation.tailrec
import io.circe.JsonObject

object MaskJson {
  final val StringMask = "*****"

  //carrying function in order to set a custom masking
  def applyUsersCustomMasking(text: String) = (customMask:String) => customMask

  def apply(json: Json): Json = {
    if(json.isArray) 
      json.mapArray(maskVectorJson)
    else if(json.isObject){
      json.mapObject(maskJsonObject)
    }
    else
 
     
      Json.fromString(applyUsersCustomMasking(json.toString())("--Hidden text--"))
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


  private def maskJsonObject(jobj: JsonObject): JsonObject = {
    @tailrec
    def objectLoop(unmaskedObject: Vector[(String, Json)], maskedObject: Vector[(String, Json)]): Vector[(String, Json)] = {
       if(unmaskedObject.isEmpty)
         maskedObject
       else {
          val (key: String, valueToMask: Json) = unmaskedObject.head
          val maskedValue = apply(valueToMask)
          val remainingUnmaskedObject: Vector[(String, Json)] = unmaskedObject.tail
          val newMaskedObject: Vector[(String, Json)] = maskedObject.appended(key -> maskedValue)
          
          objectLoop(remainingUnmaskedObject, newMaskedObject)  
       }
      
    }

    JsonObject.fromIterable(objectLoop(jobj.toVector, Vector.empty))
  }



}

