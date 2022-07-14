package snowdrop

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MaskLiteralsSpec extends AnyFlatSpec with Matchers {

   trait JsonEncoder[A] {
    def encode(a: A): Json
   }

   implicit object StringJsonEncoder extends JsonEncoder[String] {
    override def encode(a: String): Json = Json.fromString(a)
   }
   implicit object IntJsonEncoder extends JsonEncoder[Int] {
    override def encode(a: Int): Json = Json.fromInt(a)
   }

   implicit object BooleanJsonEncoder extends JsonEncoder[Boolean]{
    override def encode(a: Boolean): Json = Json.fromBoolean(a)
   }

   implicit def listEncoder[A](implicit jsonEncoder: JsonEncoder[A]): JsonEncoder[List[A]] = new JsonEncoder[List[A]] {
    override def encode(a: List[A]): Json = Json.arr(a.map(jsonEncoder.encode):_*)
   }

   implicit object JsonJsonEncoder extends JsonEncoder[Json] {
    override def encode(a: Json): Json = a
   }

   

   implicit def toJson[A](a: A)(implicit jsonEncoder: JsonEncoder[A]): Json =
    jsonEncoder.encode(a)



  "json masking" must "mask a json string literal" in {
    MaskJson.apply("") mustEqual
      toJson("*****")
  }

  "json masking" must "not mask a json number literal by default" in {
    MaskJson.apply(123) mustEqual
      toJson(123)
  }

  "json masking" must "yield the json untouched if it's a boolean" in {
    MaskJson.apply(toJson(false)) mustEqual toJson(false)
  }

  "json masking" must "mask a json string literal according to user defined mask" in {
    val userCustomMask = Json.fromString("#####")
    MaskJson.applyWithCustomMask(Json.fromString(""))(userCustomMask) mustEqual
      userCustomMask
  }

  "json masking" must "mask a json number literal according to user defined mask" in {
    val userCustomMask = Json.fromString("#####")
    MaskJson.applyWithCustomMask(Json.fromInt(123))(numberMasking =
      Some(userCustomMask)
    ) mustEqual
      userCustomMask
  }

  "json masking" must "have no effect on an empty json array" in {
    MaskJson.apply(Json.arr()) mustEqual Json.arr()
  }

  "json masking" must "mask all elements within an array(string)" in {

    implicit val strToJson: String => Json = toJson

    val input: Json = List("fdfdhf")

    val expectedOutput: Json = Json.arr(Json.fromString("*****"))

    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all elements within an array(array(string))" in {
    val input: Json = Json.arr(Json.arr(Json.fromString("fdfdhf")))

    val expectedOutput: Json = Json.arr(Json.arr(Json.fromString("*****")))

    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all elements within a json object" in {
    val input: Json = Json.obj("dfhdfhhdf" -> Json.fromString("dfhdfhhd"))
    val expectedOutput: Json = Json.obj("dfhdfhhdf" -> Json.fromString("*****"))

    MaskJson.apply(input) mustEqual expectedOutput
  }

  "json masking" must "mask all nested values within a json object" in {

    val friendA = createFriend("A", 15, List("food"))

    val friendB = createFriend("B", 16, List("football"))

    val allFriends: Json = List(friendA, friendB)

    val person = createPerson("George", 17, List("movies"), allFriends)

    val expectedOutput = Json.obj(
      "name" -> Json.fromString("*****"),
      "age" -> Json.fromString("*****"),
      "hobbies" -> Json.arr(Json.fromString("*****")),
      "friends" -> Json.arr(
        Json.obj(
          "name" -> Json.fromString("*****"),
          "age" -> Json.fromString("*****"),
          "hobbies" -> Json.arr(Json.fromString("*****"))
        ),
        Json.obj(
          "name" -> Json.fromString("*****"),
          "age" -> Json.fromString("*****"),
          "hobbies" -> Json.arr(Json.fromString("*****"))
        )
      )
    )
    MaskJson.apply(person) mustEqual expectedOutput
  }

  "json masking" must "mask all nested values within a json object with custom user masking" in {
    val userCustomMask = Json.fromString("#")
    val friendA = createFriend("A", 15, List("food"))

    val friendB = createFriend("B", 16, List("football"))

    val allFriends = Json.arr(friendA, friendB)

    val person = createPerson("George", 17, List("movies"), allFriends)

    val expectedOutput = Json.obj(
      "name" -> userCustomMask,
      "age" -> userCustomMask,
      "hobbies" -> Json.arr(userCustomMask),
      "friends" -> Json.arr(
        Json.obj(
          "name" -> userCustomMask,
          "age" -> userCustomMask,
          "hobbies" -> Json.arr(userCustomMask)
        ),
        Json.obj(
          "name" -> userCustomMask,
          "age" -> userCustomMask,
          "hobbies" -> Json.arr(userCustomMask)
        )
      )
    )
    MaskJson.applyWithCustomMask(person)(
      userCustomMask
    ) mustEqual expectedOutput
  }

  private def createFriend(
      name: String,
      age: Int,
      hobbies: List[String]
  ): Json =
    Json.obj(
      "name" -> Json.fromString(name),
      "age" -> Json.fromString(age.toString),
      "hobbies" -> Json.arr(hobbies.map(Json.fromString): _*)
    )

  private def createPerson(
      name: String,
      age: Int,
      hobbies: List[String],
      friends: Json
  ): Json =
    createFriend(name, age, hobbies).mapObject(_.add("friends", friends))
}
