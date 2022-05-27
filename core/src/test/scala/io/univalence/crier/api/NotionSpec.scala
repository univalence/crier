package io.univalence.crier.api

import io.univalence.crier.api.Notion._

import zio.Scope
import zio.test._

object NotionSpec extends ZIOSpecDefault {
  val text: NotionText =
    NotionText(
      List(
        NotionPlainText("This is the first line\nThis is the second line"),
        NotionPlainText("This is the third line")
      )
    )

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Notion blocks to Linkedin translation")(
      test("paragraph") {
        val paragraph = NotionBlock.Paragraph(text)

        val expected =
          """This is the first line
            |This is the second line
            |This is the third line""".stripMargin

        assertTrue(paragraph.toText == expected)
      },
      test("code") {
        val paragraph = NotionBlock.Code(text)

        val expected =
          """> This is the first line
            |> This is the second line
            |> This is the third line""".stripMargin

        assertTrue(paragraph.toText == expected)
      },
      test("bullet point") {
        val paragraph = NotionBlock.Bullet(text)

        val expected =
          """👉 This is the first line
            |👉 This is the second line
            |👉 This is the third line""".stripMargin

        assertTrue(paragraph.toText == expected)
      }
    )
}
