package snowdrop

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import io.circe.Json
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class MaskLiteralsPropertySpec extends Properties("MaskJson"){
  
    /*
        Values other that string are not replaced
        json =>
            json number or json boolean, json null
        //mutation testing => changes production code -> if tests pass, bad
    */
    private val genJson: Gen[Json] = Gen.oneOf(
        Gen.nonEmptyListOf(Gen.numChar).map(_.mkString).map(BigDecimal(_)).map(Json.fromBigDecimal(_)),
        Gen.oneOf(true, false).map(Json.fromBoolean),
        Gen.const(Json.Null)
    )
    implicit val jsonArbitrary: Arbitrary[Json] = Arbitrary(genJson)

    property("unchanged values") = forAll { (j: Json) => 
        MaskJson(j) == j
    }

    property("all strings are replaced with *****") = forAll { (s: String) =>
        MaskJson(Json.fromString(s)) == Json.fromString("*****")
    }

    property("idempotent for masking") = forAll { (s: String) =>
        MaskJson(MaskJson(Json.fromString(s))) == MaskJson(Json.fromString(s))
    }

    property("idempotent from pass through") = forAll { (j: Json) =>
        MaskJson(MaskJson(j)) == MaskJson(j)
    }

    property("preserve the contents of the array") = forAll { (jsonArray: Vector[Json]) =>
        MaskJson(Json.arr(jsonArray:_*)).asArray == Some(jsonArray) 
    }

    property("preserve the length of the array when masking") = forAll { (jsonArray: Vector[String]) =>
        MaskJson(Json.arr(jsonArray.map(Json.fromString):_*)).asArray.map(_.length) == Some(jsonArray.length) 
    }
}
