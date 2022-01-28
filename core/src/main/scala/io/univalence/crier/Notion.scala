package io.univalence.crier

import io.circe.generic.auto._
import io.circe.generic.extras.{Configuration => CirceConfiguration, ConfiguredJsonCodec, JsonKey}
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import io.univalence.crier.Domain.{Post, PostKind, PostLine, PostProperties, PostStatus, PropetiesDatabase}
import io.univalence.crier.Domain.PostStatus._
import io.univalence.crier.Main.Configuration

import zio.{Accessible, Task, ZIO, ZLayer}
import zio.config.getConfig

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

  implicit val config: CirceConfiguration = CirceConfiguration.default

  @ConfiguredJsonCodec
  final case class NotionProperties(
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

  final case class NotionBlock(
      paragraph: NotionParagraph
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
    def retrieveDatabase: Task[PropetiesDatabase]

    def retrievePostLines(postId: String): Task[List[PostLine]]

    def updatePost(page: Post): Task[Unit]

    final def retrievePosts(postProperties: List[PostProperties]): ZIO[NotionApi, Throwable, List[Post]] =
      ZIO.foreach(postProperties) { properties =>
        for {
          blocks <- retrievePostLines(properties.id)
        } yield Post(properties, blocks)
      }

    final def updatePosts(posts: List[Post]): ZIO[NotionApi, Throwable, Unit] = ZIO.foreach(posts)(updatePost).as(())
  }

  object NotionApi extends Accessible[NotionApi]

  final case class NotionApiLive(configuration: Configuration, sttp: SttpClient) extends NotionApi {
    val url: String = "https://api.notion.com/v1"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.notionBearer)
        .header("Notion-Version", "2021-05-13")

    override def retrieveDatabase: Task[PropetiesDatabase] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .post(uri"$url/databases/${configuration.databaseId}/query")
          .response(asJson[NotionDatabase])

      Api.succeedOrDieWithLog(sttp.send(request)).map(PropetiesDatabase.fromNotionDatabase)
    }

    override def retrievePostLines(postId: String): Task[List[PostLine]] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/blocks/$postId/children?page_size=100")
          .response(asJson[NotionBlocks])

      Api.succeedOrDieWithLog(sttp.send(request)).map(_.results.map(PostLine.fromNotionBlock))
    }

    override def updatePost(page: Post): Task[Unit] = {
      val stringifiedPublicationDateJson =
        page.properties.publicationDate match {
          case Some(date) =>
            s"""
               |"Date de publication": {
               |   "date": {
               |       "start": "$date"
               |   }
               |}
               |""".stripMargin
          case None =>
            """
              |"Date de publication": {
              |   "date": null
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
               |        },
               |        $stringifiedPublicationDateJson
               |    }
               |}
               |""".stripMargin
          )
          .patch(uri"$url/pages/${page.properties.id}")

      // TODO: Much better error handling
      sttp.send(request).flatMap(r => ZIO.fromEither(r.body)).fold(e => throw new Exception(e.toString), _ => ())
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
