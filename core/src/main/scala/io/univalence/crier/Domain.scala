package io.univalence.crier

import io.circe.{Decoder, Encoder}

import io.univalence.crier.Domain.PostStatus.{NotValid, Pending}
import io.univalence.crier.Notion.{NotionBlock, NotionDatabase, NotionPage}
import io.univalence.crier.Validator.validatePage

import java.time.{LocalDate, ZonedDateTime}

object Domain {
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

    implicit val encodePostStatus: Encoder[PostStatus] =
      Encoder[String].contramap {
        case PostStatus.Pending  => "Pending"
        case PostStatus.NotValid => "Not valid"
        case PostStatus.Posted   => "Posted"
      }

    implicit val decodePostStatus: Decoder[PostStatus] =
      Decoder[String].emap {
        case "Pending"   => Right(Pending)
        case "Not valid" => Right(NotValid)
        case "Posted"    => Right(Posted)
        case v           => Left(s"$v is not a valid status")
      }
  }

  sealed trait PostKind

  object PostKind {
    case object Tips extends PostKind

    case object Library extends PostKind

    case object Tool extends PostKind

    implicit val decodePostKind: Decoder[PostKind] =
      Decoder[String].emap {
        case "Tips"    => Right(PostKind.Tips)
        case "Library" => Right(PostKind.Library)
        case "Tool"    => Right(PostKind.Tool)
        case v         => Left(s"$v is not a valid post")
      }

    implicit val encodePostKind: Encoder[PostKind] =
      Encoder[String].contramap {
        case Tips    => "Tips"
        case Library => "Library"
        case Tool    => "Tool"
      }
  }

  final case class PostProperties(
      id:              String,
      createdTime:     ZonedDateTime,
      kind:            Option[PostKind],
      status:          Option[PostStatus],
      publicationDate: Option[LocalDate],
      keywords:        List[String],
      link:            Option[String]
  )

  object PostProperties {
    def fromNotionPage(page: NotionPage): PostProperties =
      PostProperties(
        id              = page.id,
        createdTime     = page.createdTime,
        status          = page.properties.status.map(_.select.name),
        publicationDate = page.properties.publicationDate.map(_.date.start),
        keywords        = page.properties.keywords.map(_.selects.map(_.name)).getOrElse(Nil),
        kind            = page.properties.kind.map(_.select.name),
        link            = page.properties.link.flatMap(_.richText.headOption.map(_.plainText))
      )
  }

  final case class Post(
      properties: PostProperties,
      lines:      List[String]
  ) {
    self =>
    def withStatus(status: PostStatus): Post = self.copy(properties = self.properties.copy(status = Some(status)))

    def withCreatedTime(createdTime: ZonedDateTime): Post =
      self.copy(properties = self.properties.copy(createdTime = createdTime))

    def withPublicationDate(publicationDate: Option[LocalDate]): Post =
      self.copy(properties = self.properties.copy(publicationDate = publicationDate))

    def validate: Post = {
      val status = if (validatePage.predicate(self)) Pending else NotValid
      self.withStatus(status)
    }

    /** Build the post from the post description. */
    val content: String = {
      val postContent = self.lines.mkString("\n")

      self.properties.link match {
        case None => postContent
        case Some(link) =>
          val linkHeader = if (link.contains("scastie")) "Exemple qui illustre ce poste:" else "Pour aller plus loin:"
          s"""$postContent
             |
             |$linkHeader
             |- $link""".stripMargin
      }
    }

  }

  final case class PropertiesDatabase(listOfProperties: List[PostProperties])

  object PostLine {
    def fromNotionBlock(block: NotionBlock): List[String] = block.paragraph.text.map(_.plainText)
  }

  object PropertiesDatabase {
    def fromNotionDatabase(database: NotionDatabase): PropertiesDatabase =
      PropertiesDatabase(listOfProperties = database.results.map(PostProperties.fromNotionPage))
  }
}
