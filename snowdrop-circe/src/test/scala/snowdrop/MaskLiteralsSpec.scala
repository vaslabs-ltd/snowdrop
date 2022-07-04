package snowdrop

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MaskLiteralsSpec extends AnyFlatSpec with Matchers {

  "json masking" must "mask a json string literal" in {
    MaskJson.apply(Json.fromString("")) mustEqual
      Json.fromString("--Hidden text--")
  }

  "json masking" must "mask a json string literal with user's masking input" in {
    MaskJson.apply(Json.fromString("")) mustEqual
      Json.fromString("--Hidden text--")
  }

 "json masking" must "have no effect on an empty json array" in {
    MaskJson.apply(Json.arr()) mustEqual Json.arr()
  }

  "json masking" must "mask all elements within an array(string)" in {
    val input: Json = Json.arr(Json.fromString("fdfdhf"))
    
    val expectedOutput: Json = Json.arr(Json.fromString("--Hidden text--"))
    
    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all elements within an array(array(string))" in {
    val input: Json = Json.arr(Json.arr(Json.fromString("fdfdhf")))
    
    val expectedOutput: Json = Json.arr(Json.arr(Json.fromString("--Hidden text--")))
    
    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all elements within a json object" in {
    val input: Json = Json.obj("dfhdfhhdf" -> Json.fromString("dfhdfhhd"))
    val expectedOutput: Json = Json.obj("dfhdfhhdf" -> Json.fromString("--Hidden text--"))

    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all nested values within a json object" in {
    
    val friendA = createFriend("A", 15, List("food"))

    val friendB = createFriend("B", 16, List("football"))

    val allFriends = Json.arr(friendA, friendB)

    val person = createPerson("George", 17, List("movies"), allFriends)

    val expectedOutput = Json.obj(
      "name" -> Json.fromString("--Hidden text--"),
      "age" -> Json.fromString("--Hidden text--"),
      "hobbies" -> Json.arr(Json.fromString("--Hidden text--")),
      "friends" -> Json.arr(Json.obj("name" -> Json.fromString("--Hidden text--"), "age" -> Json.fromString("--Hidden text--"), "hobbies" -> Json.arr(Json.fromString("--Hidden text--"))), 
      Json.obj("name" -> Json.fromString("--Hidden text--"), "age" -> Json.fromString("--Hidden text--"), "hobbies" -> Json.arr(Json.fromString("--Hidden text--"))))
      
    )    
    MaskJson.apply(person) mustEqual expectedOutput
  }

  private def createFriend(name: String, age: Int, hobbies: List[String]): Json = 
    Json.obj(
      "name" -> Json.fromString(name),
      "age" -> Json.fromString(age.toString),
      "hobbies" -> Json.arr(hobbies.map(Json.fromString):_*)
    )

  private def createPerson(name: String, age: Int, hobbies: List[String], friends: Json): Json =
    createFriend(name, age, hobbies).mapObject(_.add("friends", friends))
}


