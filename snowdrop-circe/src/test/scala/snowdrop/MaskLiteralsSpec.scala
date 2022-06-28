package snowdrop

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MaskLiteralsSpec extends AnyFlatSpec with Matchers {

  "json masking" must "hide a json string" in {
    val initialJsonString = Json.fromString("hgfdhgfdjhgdsfg")
    val expectedMaskedJsonString = Json.fromString("*****")
    MaskJson(initialJsonString) mustEqual
      expectedMaskedJsonString
  }
  "json masking" must "be configurable" in{
    val initialJsonString = Json.fromString("hgfdhgfdjhgdsfg")
    val expectedMaskedJsonString = Json.fromString("###")

    MaskJson(initialJsonString, MaskJson.Settings(stringMaskingSequence = "###")) mustEqual expectedMaskedJsonString
  }
  
  "json masking" must "hide a json number" in {
    val initialJsonNumber = Json.fromInt(54569)
    val expectedMaskedJsonString = Json.fromString("*****")
    MaskJson(initialJsonNumber) mustEqual
      expectedMaskedJsonString
  }

  "json masking" must "be configurable number" in{
    val initialJsonNumber = Json.fromInt(54569)
    val expectedMaskedNumber = 123
    val expectedMaskedJsonNumber = Json.fromInt(expectedMaskedNumber)

    MaskJson(initialJsonNumber, maskingSettings = MaskJson.Settings(expectedMaskedJsonNumber, Json.Null)) mustEqual 
      expectedMaskedJsonNumber
  }

  "json masking" must "hide json array fields" in {
    val item1 = Json.fromString("1")
    val item2 = Json.fromString("2")
    val expectedItemMask = Json.fromString("*****")

    MaskJson(Json.arr(item1, item2), maskingSettings = MaskJson.Settings.default) mustEqual 
      Json.arr(Json.arr(expectedItemMask, expectedItemMask))
  }
  

  /*
  number => Json.fromString("*****") //default behaviour
  (number, Json) => Json    //configurable

  */


}
