package snowdrop

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MaskLiteralsSpec extends AnyFlatSpec with Matchers {

  "json masking" must "mask a json string literal" in {
    MaskJson(Json.fromString("")) mustEqual
      Json.fromString("*****")
  }

}
