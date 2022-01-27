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
  }

  final case class PostProperties(
      id:              String,
      createdTime:     ZonedDateTime,
      kind:            Option[PostKind],
      status:          Option[PostStatus],
      publicationDate: Option[LocalDate],
      keywords:        List[String]
  )

  object PostProperties {
    def fromNotionPage(page: NotionPage): PostProperties =
      PostProperties(
        id              = page.id,
        createdTime     = page.createdTime,
        status          = page.properties.status.map(_.select.name),
        publicationDate = page.properties.publicationDate.map(_.date.start),
        keywords        = page.properties.keywords.map(_.selects.map(_.name)).getOrElse(Nil),
        kind            = page.properties.kind.map(_.select.name)
      )
  }

  final case class Post(
      properties: PostProperties,
      content:    List[PostLine]
  ) {
    self =>
    def withStatus(status: PostStatus): Post = self.copy(properties = self.properties.copy(status = Some(status)))

    def withPublicationDate(publicationDate: Option[LocalDate]): Post =
      self.copy(properties = self.properties.copy(publicationDate = publicationDate))

    def validate: Post = {
      val status = if (validatePage.predicate(self)) Pending else NotValid
      self.withStatus(status)
    }

  }

  final case class PropetiesDatabase(listOfProperties: List[PostProperties])

  final case class PostLine(
      text: String
  )

  object PostLine {
    def fromNotionBlock(block: NotionBlock): PostLine =
      PostLine(text = block.paragraph.text.map(_.plainText).mkString("\n"))
  }

  object PropetiesDatabase {
    def fromNotionDatabase(database: NotionDatabase): PropetiesDatabase =
      PropetiesDatabase(listOfProperties = database.results.map(PostProperties.fromNotionPage))
  }
}
