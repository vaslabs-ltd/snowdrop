package snowdrop

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MaskLiteralsSpec extends AnyFlatSpec with Matchers {

  "json masking" must "mask a json string literal" in {
    MaskJson.apply(Json.fromString("")) mustEqual
      Json.fromString("*****")
  }

 "json masking" must "have no effect on an empty json array" in {
    MaskJson.apply(Json.arr()) mustEqual Json.arr()
  }

  "json masking" must "mask all elements within an array(string)" in {
    val input: Json = Json.arr(Json.fromString("fdfdhf"))
    
    val expectedOutput: Json = Json.arr(Json.fromString("*****"))
    
    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all elements within an array(array(string))" in {
    val input: Json = Json.arr(Json.arr(Json.fromString("fdfdhf")))
    
    val expectedOutput: Json = Json.arr(Json.arr(Json.fromString("*****")))
    
    MaskJson.apply(input) mustEqual expectedOutput
  }
}
