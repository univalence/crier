package io.univalence.crier

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.generic.extras.{Configuration => CirceConfiguration, ConfiguredJsonCodec, JsonKey}
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import io.univalence.crier.Main.PostKind.{Library, Tips}
import io.univalence.crier.Main.PostStatus.{NotValid, Pending, Posted}

import zio._
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

import java.time.{DayOfWeek, LocalDate, ZonedDateTime}

object Main extends ZIOAppDefault {

  case class Configuration(
      notionBearer:        String,
      databaseId:          String,
      linkedinAccessToken: String
  )

  val configurationLayer: Layer[ReadError[String], Configuration] =
    ZConfig.fromMap(
      Map(
        "notionBearer" -> "secret_WPftuer9iBgPPWaqTMyuJNSd437eAiqvRCY1tjLRr1Z",
        "databaseId"   -> "3868f708ae46461fbfcf72d34c9536f9",
        "linkedinAccessToken" -> "AQU6o9J9giFL1O_SvgT4YJ1O_yzb86la-R3yOMcnzG_dwdM-FvkRj2xgE_YU8gRIsv0l2b1WCnWUzsMNE0kcYceTqPb-AQnVOPEid_s-GPjAp1qiOB4a82_QMJauMlmC_4L0Hh-9Y7KSIsdP2paOduE9oHtok3_My6PNnGq1cA8cpv2uvxzHUPdXAs1Dw1rORSGmaRzyPbsJXL3k9y43w2LXLeLj8tUHqa30Im4WYQjgfjvagfms5VaX5A_k8NAXDtIJpEP3cmiRv7icIVB9BW6sPX-Nl2qmaBH5STcFsc2yt9r9AsPnjk_298EOXXVCzxp0XNkhR3KfJU2Z6aAchx7Tq8I82A"
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

  object PostStatus {
    case object Pending extends PostStatus

    case object NotValid extends PostStatus

    case object Posted extends PostStatus

    def toNotion(status: PostStatus): String =
      status match {
        case Pending  => "Pending"
        case NotValid => "Not valid"
        case Posted   => "Posted"
      }
  }

  sealed trait PostKind

  object PostKind {
    case object Tips    extends PostKind
    case object Library extends PostKind
  }

  implicit val encodePostStatus: Encoder[PostStatus] =
    Encoder[String].contramap {
      case PostStatus.Pending  => "Pending"
      case PostStatus.NotValid => "Not valid"
      case PostStatus.Posted   => "Posted"
    }

  implicit val decodePostKind: Decoder[PostKind] =
    Decoder[String].emap {
      case "Tips"    => Right(PostKind.Tips)
      case "Library" => Right(PostKind.Library)
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

  case class NotionSelectFrom[T](name: T)

  case class NotionSelectProperty[T](select: NotionSelectFrom[T])

  @annotation.nowarn // https://github.com/circe/circe/issues/1411
  @ConfiguredJsonCodec
  case class NotionMultiSelectProperty[T](@JsonKey("multi_select") selects: List[NotionSelectFrom[T]])

  case class NotionDate(start: LocalDate)

  case class NotionDateProperty(date: NotionDate)

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

  case class RefinedNotionProperties(
      id:              String,
      createdTime:     ZonedDateTime,
      kind:            Option[PostKind],
      status:          Option[PostStatus],
      publicationDate: Option[LocalDate],
      keywords:        List[String]
  )

  object RefinedNotionProperties {
    def fromNotionPage(page: NotionPage): RefinedNotionProperties =
      RefinedNotionProperties(
        id              = page.id,
        createdTime     = page.createdTime,
        status          = page.properties.status.map(_.select.name),
        publicationDate = page.properties.publicationDate.map(_.date.start),
        keywords        = page.properties.keywords.map(_.selects.map(_.name)).getOrElse(Nil),
        kind            = page.properties.kind.map(_.select.name)
      )
  }

  case class RefinedNotionPage(
      properties: RefinedNotionProperties,
      content:    List[RefinedNotionBlock]
  )

  case class RefinedNotionDatabase(pageProperties: List[RefinedNotionProperties])

  case class RefinedNotionBlock(
      text: String
  )

  object RefinedNotionBlock {
    def fromNotionBlock(block: NotionBlock): RefinedNotionBlock =
      RefinedNotionBlock(text = block.paragraph.text.map(_.plainText).mkString("\n"))
  }

  object RefinedNotionDatabase {
    def fromNotionDatabase(database: NotionDatabase): RefinedNotionDatabase =
      RefinedNotionDatabase(pageProperties = database.results.map(RefinedNotionProperties.fromNotionPage))
  }

  trait NotionApi {
    def retrieveDatabase: Task[RefinedNotionDatabase]

    def retrieveBlocks(pageId: String): Task[List[RefinedNotionBlock]]

    def updatePage(page: RefinedNotionPage): Task[Unit]

    final def retrieveAllPages(
        pageProperties: List[RefinedNotionProperties]
    ): ZIO[NotionApi, Throwable, List[RefinedNotionPage]] =
      ZIO.foreach(pageProperties) { properties =>
        for {
          blocks <- retrieveBlocks(properties.id)
        } yield RefinedNotionPage(properties, blocks)
      }

    final def updateAllPages(pages: List[RefinedNotionPage]): ZIO[NotionApi, Throwable, Unit] =
      ZIO.foreach(pages)(updatePage).as(())
  }

  object NotionApi extends Accessible[NotionApi]

  case class NotionApiLive(configuration: Configuration, sttp: SttpClient) extends NotionApi {
    val url = "https://api.notion.com/v1"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.notionBearer.toString())
        .header("Notion-Version", "2021-05-13")

    override def retrieveDatabase: Task[RefinedNotionDatabase] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .post(uri"$url/databases/${configuration.databaseId}/query")
          .response(asJson[NotionDatabase])

      Api.succeedOrDieWithLog(sttp.send(request)).map(RefinedNotionDatabase.fromNotionDatabase)
    }

    override def retrieveBlocks(pageId: String): Task[List[RefinedNotionBlock]] = {
      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body("{}")
          .get(uri"$url/blocks/$pageId/children?page_size=100")
          .response(asJson[NotionBlocks])

      Api.succeedOrDieWithLog(sttp.send(request)).map(_.results.map(RefinedNotionBlock.fromNotionBlock))
    }

    override def updatePage(page: RefinedNotionPage): Task[Unit] = {
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

  case class NotionPageValidator(predicate: RefinedNotionPage => Boolean) {
    def and(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: RefinedNotionPage) => predicate(page) && that.predicate(page))

    def or(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: RefinedNotionPage) => predicate(page) || that.predicate(page))
  }

  val sizeValidator: NotionPageValidator = NotionPageValidator(_.content.length < 280)
  val minimumKeywordsValidator: NotionPageValidator =
    NotionPageValidator(
      _.properties.keywords.length >= 2
    )
  val mandatoryTypeValidator: NotionPageValidator = NotionPageValidator(_.properties.kind.isDefined)

  val validatePage: NotionPageValidator = sizeValidator and minimumKeywordsValidator and mandatoryTypeValidator

  /**
   * Return True if the validator system should check the page or not.
   */
  def shouldBeChecked(properties: RefinedNotionProperties): Boolean =
    properties.status.getOrElse(NotValid) match {
      case Posted => false
      case _      => true
    }

  /**
   * Sort pages to validate according to their creation time and the
   * publication date.
   */
  def sortPagesForDateAttribution(pages: List[RefinedNotionPage]): List[RefinedNotionPage] =
    pages.sortBy(_.properties.createdTime).sortBy(_.properties.publicationDate.getOrElse(LocalDate.MAX))

  /**
   * Validate the pages according to our predicates and separate them
   * between pending and other status.
   */
  def validateAndSeparatePages(
      pages: List[RefinedNotionPage]
  ): (List[RefinedNotionPage], List[RefinedNotionPage]) =
    pages.map(validatePageStatus).partition(_.properties.status.getOrElse(NotValid) == Pending)

  /** Update a page status */
  def updateStatus(page: RefinedNotionPage, status: PostStatus): RefinedNotionPage =
    page.copy(properties = page.properties.copy(status = Some(status)))

  /** validate a page and update the status */
  def validatePageStatus(page: RefinedNotionPage): RefinedNotionPage = {
    val statusFor: RefinedNotionPage => PostStatus = page => if (validatePage.predicate(page)) Pending else NotValid

    updateStatus(page, statusFor(page))
  }

  /** Update a page publication date */
  def updatePagePublicationDate(page: RefinedNotionPage, publicationDate: Option[LocalDate]): RefinedNotionPage =
    page.copy(properties = page.properties.copy(publicationDate = publicationDate))

  /** Attribute publication date to valid pages. */
  def attributePublicationDateToPages(
      pages: List[RefinedNotionPage]
  ): ZIO[Clock, Nothing, List[RefinedNotionPage]] = {
    val sortedPages = sortPagesForDateAttribution(pages)
    val publicationDatesEffect =
      ZIO.collectAll(
        sortedPages
          .scanLeft(Clock.localDateTime.map(_.toLocalDate.minusDays(1)))((previousDate, _) =>
            previousDate.map(date =>
              date.getDayOfWeek match {
                case DayOfWeek.FRIDAY   => date.plusDays(3)
                case DayOfWeek.SATURDAY => date.plusDays(2)
                case _                  => date.plusDays(1)
              }
            )
          )
          .drop(1)
      )

    publicationDatesEffect.map(_.zip(sortedPages).map { case (date, page) =>
      updatePagePublicationDate(page, Some(date))
    })
  }

  def validateDatabasePages: ZIO[Clock with Console with NotionApi, Throwable, List[RefinedNotionPage]] =
    for {
      database <- NotionApi(_.retrieveDatabase)
      pageProperties = database.pageProperties.filter(shouldBeChecked)
      augmentedPages <- NotionApi(_.retrieveAllPages(pageProperties))
      (pendingPages, notValidPages)       = validateAndSeparatePages(augmentedPages)
      notValidPagesWithoutPublicationDate = notValidPages.map(updatePagePublicationDate(_, None))
      _                               <- NotionApi(_.updateAllPages(notValidPagesWithoutPublicationDate))
      pendingPagesWithPublicationDate <- attributePublicationDateToPages(pendingPages)
      _                               <- NotionApi(_.updateAllPages(pendingPagesWithPublicationDate))
    } yield pendingPagesWithPublicationDate

  def postPage(page: RefinedNotionPage): ZIO[NotionApi, Throwable, Unit] =
    for {
      _ <- ZIO.collectAllPar(List()) // TODO : transform page into posts
      _ <- NotionApi(_.updatePage(updateStatus(page, Posted)))
    } yield ()

  def program: ZIO[Clock with Console with NotionApi, Throwable, Unit] =
    for {
      pendingPages <- validateDatabasePages
      todayPage = pendingPages.headOption
      _ <-
        todayPage match {
          case Some(page) => postPage(page)
          case None       => Console.printLine("No post to share today :(")
        }
    } yield ()

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    program
      .provide(
        Clock.live,
        Console.live,
        configurationLayer,
        sttpLayer,
        NotionApiLive.layer
      )
}
