package io.univalence.crier.api

import cats.syntax.functor._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.extras.{Configuration => CirceConfiguration, ConfiguredJsonCodec, _}
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import io.univalence.crier.Domain._
import io.univalence.crier.Domain.PostStatus.NotValid
import io.univalence.crier.Main.Configuration

import zio._
import zio.config._

import java.time.{LocalDate, ZonedDateTime}

object Notion {
  @ConfiguredJsonCodec
  final case class NotionPlainText(@JsonKey("plain_text") plainText: String)

  final case class NotionText(text: List[NotionPlainText])

  final case class NotionSelectFrom[T](name: T)

  final case class NotionSelectProperty[T](select: NotionSelectFrom[T])

  @annotation.nowarn // https://github.com/circe/circe/issues/1411
  @ConfiguredJsonCodec
  final case class NotionMultiSelectProperty[T](@JsonKey("multi_select") selects: List[NotionSelectFrom[T]])

  final case class NotionDate(start: LocalDate)

  final case class NotionDateProperty(date: NotionDate)

  @ConfiguredJsonCodec
  final case class NotionRichTextProperty(@JsonKey("rich_text") richText: List[NotionPlainText])

  final case class NotionTitleProperty(title: List[NotionPlainText])

  final case class NotionPeopleProperty(people: List[NotionPeople])

  implicit val config: CirceConfiguration = CirceConfiguration.default

  @ConfiguredJsonCodec
  final case class NotionProperties(
      @JsonKey("Name")
      name: Option[NotionTitleProperty],
      @JsonKey("Auteur")
      authors: Option[NotionPeopleProperty],
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

  @ConfiguredJsonCodec
  final case class NotionPeople(
      @JsonKey("id")
      identifier: String
  )

  sealed trait NotionBlock

  object NotionBlock {
    final case class Paragraph(paragraph: NotionText) extends NotionBlock
    final case class Code(code: NotionText)           extends NotionBlock
    @ConfiguredJsonCodec
    final case class Bullet(@JsonKey("bulleted_list_item") bulletedListItem: NotionText) extends NotionBlock

    implicit val decoder: Decoder[NotionBlock] =
      List[Decoder[NotionBlock]](
        Decoder[Paragraph].widen,
        Decoder[Bullet].widen,
        Decoder[Code].widen
      ).reduceLeft(_ or _)
  }

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

  final case class NotionUser(name: String)

  trait NotionApi {
    def retrieveDatabase: Task[PropertiesDatabase]

    def retrievePostBody(postId: String): Task[String]

    def updatePost(post: Post): Task[Unit]

    def retrieveAuthor(authorId: String): Task[String]

    final def retrievePosts(postProperties: List[PostProperties]): Task[List[Post]] =
      for {
        memoizedRetrieveAuthor <- ZIO.memoize(retrieveAuthor)
        posts <-
          ZIO.foreachPar(postProperties) { properties =>
            val title = properties.subject.getOrElse("unknown")
            for {
              _       <- ZIO.logInfo(s"Fetching information for $title post (${properties.id})")
              body    <- retrievePostBody(properties.id)
              authors <- ZIO.foreachPar(properties.authorIds)(memoizedRetrieveAuthor)
            } yield Post(authors, properties, body)
          }
      } yield posts

    final def updatePosts(posts: List[Post]): ZIO[NotionApi, Throwable, Unit] = ZIO.foreachParDiscard(posts)(updatePost)
  }

  object NotionApi {
    def retrieveDatabase: ZIO[NotionApi, Throwable, PropertiesDatabase] =
      ZIO.service[NotionApi].flatMap(_.retrieveDatabase)

    def retrievePostBody(postId: String): ZIO[NotionApi, Throwable, String] =
      ZIO.service[NotionApi].flatMap(_.retrievePostBody(postId))

    def updatePost(page: Post): ZIO[NotionApi, Throwable, Unit] = ZIO.service[NotionApi].flatMap(_.updatePost(page))

    def retrieveAuthor(authorId: String): ZIO[NotionApi, Throwable, String] =
      ZIO.service[NotionApi].flatMap(_.retrieveAuthor(authorId))

    final def retrievePosts(postProperties: List[PostProperties]): ZIO[NotionApi, Throwable, List[Post]] =
      ZIO.service[NotionApi].flatMap(_.retrievePosts(postProperties))

    final def updatePosts(posts: List[Post]): ZIO[NotionApi, Throwable, Unit] =
      ZIO.service[NotionApi].flatMap(_.updatePosts(posts))

  }

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

    override def retrievePostBody(postId: String): Task[String] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/blocks/$postId/children?page_size=100")
          .response(asJson[NotionBlocks])

      val response = Api.succeedOrDie(sttp.send(request))

      response
        .map(
          _.results
            .map {
              case NotionBlock.Paragraph(paragraph)     => paragraph.text.map(_.plainText).mkString("\n")
              case NotionBlock.Bullet(bulletedListItem) => s"👉 ${bulletedListItem.text.map(_.plainText).reduce(_ + _)}"
              case NotionBlock.Code(code)               => code.text.map(t => s"> ${t.plainText}").mkString("\n")
            }
            .mkString("\n")
        )
    }

    override def retrieveAuthor(authorId: String): Task[String] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/users/$authorId")
          .response(asJson[NotionUser])

      val response = Api.succeedOrDie(sttp.send(request))

      response.map(_.name)
    }

    override def updatePost(post: Post): Task[Unit] = {
      val stringifiedPublicationDateJson =
        post.properties.publicationDate match {
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
        post.errors match {
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

      val stringifiedStatus = PostStatus.toNotion(post.properties.status.getOrElse(NotValid))

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
          .patch(uri"$url/pages/${post.properties.id}")

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
