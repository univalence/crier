package io.univalence.crier

import io.univalence.crier.Fixtures.fakePost

import zio.test._
import zio.test.Assertion._

object DomainTest extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] = postSpec

  val postSpec: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("Post Spec")(
      test("Post should build a correct final content") {
        val result =
          """This is my daily post.
            |With a lot of content!
            |
            |Ce post a été écrit par l'Univalien Jon Doe. 🐇""".stripMargin

        assert(fakePost.content)(equalTo(result))
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
             |Ce post a été écrit par l'Univalien Jon Doe. 🐇""".stripMargin

        assert(fakePostWithLink.content)(equalTo(result))
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
             |Ce post a été écrit par l'Univalien Jon Doe. 🐇""".stripMargin

        assert(fakePostWithLink.content)(equalTo(result))
      }
    )
}
