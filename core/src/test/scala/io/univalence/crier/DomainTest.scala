package io.univalence.crier

import io.univalence.crier.Fixtures.fakePost

import zio.test._

object DomainTest extends ZIOSpecDefault {
  override def spec: ZSpec[TestEnvironment, Any] = postSpec

  val postSpec: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("Post Spec")(
      test("Post should build a correct final content") {
        val result =
          """This is my daily post.
            |With a lot of content!
            |
            |Ce post a été écrit par Jon Doe. 🐇""".stripMargin

        assertTrue(fakePost.content == result)
      },
      test("Post should build a correct final content with a link") {
        val link             = "https://google.com"
        val fakePostWithLink = fakePost.copy(properties = fakePost.properties.copy(link = Some(link)))

        val result =
          s"""This is my daily post.
             |With a lot of content!
             |
             |Pour aller plus loin:
             |- $link
             |
             |Ce post a été écrit par Jon Doe. 🐇""".stripMargin

        assertTrue(fakePostWithLink.content == result)
      },
      test("Post should build a correct final content with a scastie link") {
        val link             = "https://scastie.scala-lang.org/ABC"
        val fakePostWithLink = fakePost.copy(properties = fakePost.properties.copy(link = Some(link)))

        val result =
          s"""This is my daily post.
             |With a lot of content!
             |
             |Exemple qui illustre ce poste:
             |- $link
             |
             |Ce post a été écrit par Jon Doe. 🐇""".stripMargin

        assertTrue(fakePostWithLink.content == result)
      },
      test("Cleaned post should escape quote correctly") {
        val fakePostWithQuote = fakePost.copy(lines = List("My post contains \\\\\" \""))

        assertTrue(fakePostWithQuote.escapedContent.contains("\\\""))
      },
      test("Post should remove incorrect ending lines") {
        val original =
          List(
            "This is my daily post.",
            "With a line here.",
            "",
            "",
            "With a lot of content!",
            "",
            "",
            ""
          )

        val fakePostWithTips = fakePost.copy(lines = original)

        val result =
          s"""This is my daily post.
             |With a line here.
             |
             |With a lot of content!
             |
             |Ce post a été écrit par Jon Doe. 🐇""".stripMargin

        assertTrue(fakePostWithTips.cleanedContent == result)
      }
    )
}
