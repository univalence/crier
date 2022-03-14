package io.univalence.crier.api

import cats.syntax.functor._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.extras.{Configuration => CirceConfiguration, _}
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import io.univalence.crier.Domain._
import io.univalence.crier.Domain.PostStatus.NotValid
import io.univalence.crier.Main.Configuration

import zio.{Accessible, Console, Task, ZIO, ZLayer}
import zio.config._

import java.time.{LocalDate, ZonedDateTime}

object Notion {
  final case class NotionSelectFrom[T](name: T)

  final case class NotionSelectProperty[T](select: NotionSelectFrom[T])

  @annotation.nowarn // https://github.com/circe/circe/issues/1411
  @ConfiguredJsonCodec
  final case class NotionMultiSelectProperty[T](@JsonKey("multi_select") selects: List[NotionSelectFrom[T]])

  final case class NotionDate(start: LocalDate)

  final case class NotionDateProperty(date: NotionDate)

  @ConfiguredJsonCodec
  final case class NotionRichTextProperty(@JsonKey("rich_text") richText: List[NotionText])

  final case class NotionTitleProperty(title: List[NotionText])

  implicit val config: CirceConfiguration = CirceConfiguration.default

  @ConfiguredJsonCodec
  final case class NotionProperties(
      @JsonKey("Name")
      name: Option[NotionTitleProperty],
      @JsonKey("Status")
      status: Option[NotionSelectProperty[PostStatus]],
      @JsonKey("Date de publication")
      publicationDate: Option[NotionDateProperty],
      @JsonKey("Mots clés")
      keywords: Option[NotionMultiSelectProperty[String]],
      @JsonKey("Type")
      kind: Option[NotionSelectProperty[PostKind]],
      @JsonKey("Link")
      link: Option[NotionRichTextProperty]
  )

  @ConfiguredJsonCodec
  final case class NotionPage(
      id: String,
      @JsonKey("created_time")
      createdTime: ZonedDateTime,
      properties:  NotionProperties
  )

  implicit val decoderResult: Decoder[NotionBlock] =
    Decoder[NotionBlockParagraph].widen or Decoder[NotionBlockBulletPoint].widen

  sealed trait NotionBlock {
    val text: List[NotionText]

    def lines: List[String] =
      text match {
        case Nil => List("")
        case l   => l.map(_.plainText)
      }
  }

  final case class NotionBlockParagraph(paragraph: NotionParagraph) extends NotionBlock {
    override val text: List[NotionText] = paragraph.text
  }

  @ConfiguredJsonCodec
  final case class NotionBlockBulletPoint(
      @JsonKey("bulleted_list_item")
      bullet: NotionBullet
  ) extends NotionBlock {
    override val text: List[NotionText] = bullet.text
  }

  final case class NotionBullet(
      text: List[NotionText]
  )

  final case class NotionParagraph(
      text: List[NotionText]
  )

  @ConfiguredJsonCodec
  final case class NotionText(
      @JsonKey("plain_text")
      plainText: String
  )

  final case class NotionBlocks(
      results: List[NotionBlock]
  )

  final case class NotionPageAugmented(
      id:          String,
      createdTime: ZonedDateTime,
      properties:  NotionProperties,
      text:        String
  )

  final case class NotionDatabase(results: List[NotionPage])

  trait NotionApi {
    def retrieveDatabase: Task[PropertiesDatabase]

    def retrievePostLines(postId: String): Task[List[String]]

    def updatePost(page: Post): Task[Unit]

    final def retrievePosts(postProperties: List[PostProperties]): ZIO[Console with NotionApi, Throwable, List[Post]] =
      ZIO.foreach(postProperties) { properties =>
        val title = properties.subject.getOrElse("none")
        for {
          _     <- Console.printLine(s"Fetching information for $title post (${properties.id})")
          lines <- retrievePostLines(properties.id)
        } yield Post(properties, lines)
      }

    final def updatePosts(posts: List[Post]): ZIO[NotionApi, Throwable, Unit] = ZIO.foreach(posts)(updatePost).unit
  }

  object NotionApi extends Accessible[NotionApi]

  final case class NotionApiLive(configuration: Configuration, sttp: SttpClient) extends NotionApi {
    val url: String = "https://api.notion.com/v1"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.notion.bearer)
        .header("Notion-Version", "2021-05-13")

    override def retrieveDatabase: Task[PropertiesDatabase] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .post(uri"$url/databases/${configuration.notion.database}/query")
          .response(asJson[NotionDatabase])

      Api.succeedOrDie(sttp.send(request)).map(PropertiesDatabase.fromNotionDatabase)
    }

    override def retrievePostLines(postId: String): Task[List[String]] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/blocks/$postId/children?page_size=100")
          .response(asJson[NotionBlocks])

      val response = Api.succeedOrDie(sttp.send(request))

      response.map(_.results.flatMap(_.lines))
    }

    override def updatePost(page: Post): Task[Unit] = {
      val stringifiedPublicationDateJson =
        page.properties.publicationDate match {
          case Some(date) =>
            s""",
               |"Date de publication": {
               |   "date": {
               |       "start": "$date"
               |   }
               |}
               |""".stripMargin
          case None =>
            """,
              |"Date de publication": {
              |   "date": null
              |}
              |""".stripMargin
        }
      val stringiedErrors =
        page.errors match {
          case Nil =>
            """,
              |"Erreurs": {
              |   "rich_text": []
              |}
              |""".stripMargin
          case errors =>
            s""",
               |"Erreurs": {
               |   "rich_text": [
               |     {
               |        "type": "text",
               |        "text": {
               |          "content": "${errors.map(e => s"- $e").mkString("\\n")}"
               |        }
               |      }
               |   ]
               |}
               |""".stripMargin
        }

      val stringifiedStatus = PostStatus.toNotion(page.properties.status.getOrElse(NotValid))

      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body(
            s"""
               |{
               |    "properties": {
               |        "Status": {
               |            "select": {
               |                "name": "$stringifiedStatus"
               |            }
               |        }
               |        $stringifiedPublicationDateJson
               |        $stringiedErrors
               |    }
               |}
               |""".stripMargin
          )
          .patch(uri"$url/pages/${page.properties.id}")

      Api.succeedOrDieWithoutValue(sttp.send(request))
    }
  }

  object NotionApiLive {
    val layer: ZLayer[SttpClient with Configuration, Nothing, NotionApiLive] =
      ZLayer {
        for {
          configuration <- getConfig[Configuration]
          sttp          <- ZIO.service[SttpClient]
        } yield new NotionApiLive(configuration, sttp)
      }
  }
}
