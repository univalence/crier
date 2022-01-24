package io.univalence.crier

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.generic.extras.{Configuration => CirceConfiguration, ConfiguredJsonCodec, JsonKey}
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import zio._
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.refined._

import java.time.{LocalDate, ZonedDateTime}

object Main extends ZIOAppDefault {

  case class Configuration(
      notionBearer: Refined[String, NonEmpty],
      databaseId:   Refined[String, NonEmpty]
  )

  val configurationLayer: Layer[ReadError[String], Configuration] =
    ZConfig.fromMap(
      Map(
        "notionBearer" -> "secret_WPftuer9iBgPPWaqTMyuJNSd437eAiqvRCY1tjLRr1Z",
        "databaseId"   -> "3868f708ae46461fbfcf72d34c9536f9"
      ),
      descriptor[Configuration]
    )

  val sttpLayer: Layer[Throwable, SttpClient] = AsyncHttpClientZioBackend.layer()

  object Api {
    // TODO: We should handle errors without throwing exception and using response information
    def succeedOrDieWithLog[E, A](task: Task[Response[Either[ResponseException[String, E], A]]]): Task[A] =
      for {
        response <- task
        item     <- ZIO.fromEither(response.body)
      } yield item
  }

  sealed trait PostStatus

  case object Pending extends PostStatus

  case object NotValid extends PostStatus

  case object Posted extends PostStatus

  object PostStatus {
    def toNotion(status: PostStatus): String =
      status match {
        case Pending  => "Pending"
        case NotValid => "Not valid"
        case Posted   => "Posted"
      }
  }

  sealed trait PostKind

  case object Tips extends PostKind

  case object Library extends PostKind

  implicit val encodePostStatus: Encoder[PostStatus] =
    Encoder[String].contramap {
      case Pending  => "Pending"
      case NotValid => "Not valid"
      case Posted   => "Posted"
    }

  implicit val decodePostKind: Decoder[PostKind] =
    Decoder[String].emap {
      case "Tips"    => Right(Tips)
      case "Library" => Right(Library)
      case v         => Left(s"$v is not a valid post")
    }

  implicit val encodePostKind: Encoder[PostKind] =
    Encoder[String].contramap {
      case Tips    => "Tips"
      case Library => "Library"
    }

  implicit val decodePostStatus: Decoder[PostStatus] =
    Decoder[String].emap {
      case "Pending"   => Right(Pending)
      case "Not valid" => Right(NotValid)
      case "Posted"    => Right(Posted)
      case v           => Left(s"$v is not a valid status")
    }

  case class NotionSelectFrom[T](id: String, name: T, color: String)

  case class NotionSelectProperty[T](id: String, select: NotionSelectFrom[T])

  @annotation.nowarn // https://github.com/circe/circe/issues/1411
  @ConfiguredJsonCodec
  case class NotionMultiSelectProperty[T](id: String, @JsonKey("multi_select") selects: List[NotionSelectFrom[T]])

  case class NotionDate(start: LocalDate)

  case class NotionDateProperty(id: String, date: NotionDate)

  implicit val config: CirceConfiguration = CirceConfiguration.default

  @ConfiguredJsonCodec
  case class NotionProperties(
      @JsonKey("Status")
      status: Option[NotionSelectProperty[PostStatus]],
      @JsonKey("Date de publication")
      publicationDate: Option[NotionDateProperty],
      @JsonKey("Mots clés")
      keywords: Option[NotionMultiSelectProperty[String]],
      @JsonKey("Type")
      kind: Option[NotionSelectProperty[PostKind]]
  )

  @ConfiguredJsonCodec
  case class NotionPage(
      id: String,
      @JsonKey("created_time")
      createdTime: ZonedDateTime,
      properties:  NotionProperties
  )

  case class NotionBlock(
      paragraph: NotionParagraph
  )

  case class NotionParagraph(
      text: List[NotionText]
  )

  @ConfiguredJsonCodec
  case class NotionText(
      @JsonKey("plain_text")
      plainText: String
  )

  @ConfiguredJsonCodec
  case class NotionBlocks(
      results: List[NotionBlock]
  )

  case class NotionPageAugmented(
      id:          String,
      createdTime: ZonedDateTime,
      properties:  NotionProperties,
      text:        String
  )

  case class NotionDatabase(results: List[NotionPage])

  trait NotionApi {
    def retrieveDatabase(): Task[NotionDatabase]

    def retrieveBlocks(pageId: String): Task[NotionBlocks]

    def updatePage(pageId: String, publicationDate: LocalDate, status: PostStatus): Task[Unit]
  }

  object NotionApi extends Accessible[NotionApi]

  case class NotionApiLive(configuration: Configuration, sttp: SttpClient) extends NotionApi {
    val url = "https://api.notion.com/v1"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.notionBearer.toString())
        .header("Notion-Version", "2021-05-13")

    override def retrieveDatabase(): Task[NotionDatabase] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .post(uri"$url/databases/${configuration.databaseId}/query")
          .response(asJson[NotionDatabase])

      Api.succeedOrDieWithLog(sttp.send(request))
    }

    override def retrieveBlocks(pageId: String): Task[NotionBlocks] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/blocks/$pageId/children?page_size=100")
          .response(asJson[NotionBlocks])

      Api.succeedOrDieWithLog(sttp.send(request))
    }

    override def updatePage(pageId: String, publicationDate: LocalDate, status: PostStatus): Task[Unit] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body(
            s"""
               |{
               |    "properties": {
               |        "Status": {
               |            "select": {
               |                "name": "${PostStatus.toNotion(status)}"
               |            }
               |        },
               |        "Date de publication": {
               |            "date": {
               |                "start": "${publicationDate.toString}"
               |            }
               |        }
               |    }
               |}
               |""".stripMargin
          )
          .patch(uri"$url/pages/$pageId")

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

  def augmentPage(page: NotionPage, blocks: NotionBlocks): NotionPageAugmented =
    NotionPageAugmented(
      id          = page.id,
      createdTime = page.createdTime,
      properties  = page.properties,
      text        = blocks.results.map(_.paragraph.text.map(_.plainText).mkString("\n")).mkString("\n")
    )

  case class NotionPageValidator(predicate: NotionPageAugmented => Boolean) {
    def and(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: NotionPageAugmented) => predicate(page) && that.predicate(page))

    def or(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: NotionPageAugmented) => predicate(page) || that.predicate(page))
  }

  val sizeValidator: NotionPageValidator = NotionPageValidator(_.text.length < 280)
  val minimumKeywordsValidator: NotionPageValidator =
    NotionPageValidator(
      _.properties.keywords match {
        case Some(keywords) => keywords.selects.length >= 2
        case None           => false
      }
    )
  val mandatoryTypeValidator: NotionPageValidator = NotionPageValidator(_.properties.kind.isDefined)

  val validatePage: NotionPageValidator = sizeValidator and minimumKeywordsValidator and mandatoryTypeValidator

  def validateDatabasePages: ZIO[Console with NotionApi, Throwable, Unit] =
    for {
      database <- NotionApi(_.retrieveDatabase())
      pages <-
        ZIO.collectAll(
          database.results.map(page =>
            for {
              blocks <- NotionApi(_.retrieveBlocks(page.id))
            } yield augmentPage(page, blocks)
          )
        )
      pagesToValidate =
        pages.filter(_.properties.status match {
          case Some(s) =>
            s.select.name match {
              case Posted => false
              case _      => true
            }
          case None => true
        })
      _ <-
        ZIO.collectAll(
          pagesToValidate.map(page =>
            NotionApi(
              _.updatePage(
                page.id,
                LocalDate.of(2022, 1, 1),
                if (validatePage.predicate(page)) Pending else NotValid
              )
            )
          )
        )
    } yield ()

  def program: ZIO[Console with NotionApi, Throwable, Unit] = validateDatabasePages

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    program.provide(Console.live, configurationLayer, sttpLayer, NotionApiLive.layer)
}
