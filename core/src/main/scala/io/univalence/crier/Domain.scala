package io.univalence.crier

import io.circe.{Decoder, Encoder}

import io.univalence.crier.Domain.PostStatus.{NotValid, Pending}
import io.univalence.crier.Validator.validatePage
import io.univalence.crier.api.Notion.{NotionDatabase, NotionPage}

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
      authorIds:       List[String],
      createdTime:     ZonedDateTime,
      subject:         Option[String],
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
        authorIds       = page.properties.authors.map(_.people.map(_.identifier)).getOrElse(Nil),
        createdTime     = page.createdTime,
        subject         = page.properties.name.flatMap(_.title.headOption.map(_.plainText)),
        status          = page.properties.status.map(_.select.name),
        publicationDate = page.properties.publicationDate.map(_.date.start),
        keywords        = page.properties.keywords.map(_.selects.map(_.name)).getOrElse(Nil),
        kind            = page.properties.kind.map(_.select.name),
        link            = page.properties.link.flatMap(_.richText.headOption.map(_.plainText))
      )
  }

  final case class Post(
      authors:    List[String],
      properties: PostProperties,
      lines:      List[String],
      errors:     List[String] = Nil
  ) {
    self =>
    def withStatus(status: PostStatus): Post = self.copy(properties = self.properties.copy(status = Some(status)))

    def withErrors(errors: List[String]): Post = self.copy(errors = errors)

    def withCreatedTime(createdTime: ZonedDateTime): Post =
      self.copy(properties = self.properties.copy(createdTime = createdTime))

    def withPublicationDate(publicationDate: Option[LocalDate]): Post =
      self.copy(properties = self.properties.copy(publicationDate = publicationDate))

    def validate: Post = {
      val errors = validatePage.predicate(self)
      if (errors.isEmpty) {
        self.withStatus(Pending)
      } else {
        self.withStatus(NotValid).withErrors(errors)
      }
    }

    def addLink(content: String): String =
      self.properties.link match {
        case None => content
        case Some(link) =>
          val linkHeader = if (link.contains("scastie")) "Exemple qui illustre ce poste:" else "Pour aller plus loin:"
          s"""$content
             |
             |$linkHeader
             |- $link""".stripMargin
      }

    def addKeywords(content: String): String =
      self.properties.keywords match {
        case Nil => content
        case keywords =>
          val stringiedKeywords = keywords.map(keyword => s"#$keyword").mkString(" ")
          s"""$content
             |
             |$stringiedKeywords""".stripMargin
      }

    def addAuthor(content: String): String =
      self.authors match {
        case Nil => content
        case author :: Nil =>
          s"""$content
             |
             |Ce post a été écrit par $author. 🐇""".stripMargin
        case _ =>
          s"""$content
             |
             |Ce post a été écrit par ${self.authors.mkString(", ")}. 🐇""".stripMargin
      }

    def toSlack(linkedinActivity: String): String = {
      val url = s"https://www.linkedin.com/feed/update/$linkedinActivity/"

      s"""Un nouveau post est disponible sur Linkedin. 🚀
         |
         |Le voici ⬇️
         |
         |```
         |$cleanedContent
         |```
         |
         |ℹ️ N'hesitez pas à le liker sur Linkedin pour augmenter sa visibilité: $url !""".stripMargin
    }

    val tips: String = self.lines.mkString("\n").stripLineEnd

    /** Build the post from the post description. */
    val content: String = addKeywords(addAuthor(addLink(tips))).stripLineEnd

    val cleanedContent: String =
      content
        .replace("\"", "\\\"")
        .replace("\\\\\"", "\\\"")
        .replaceAll("\n{3,}", "\n\n")

    val escapedContent: String =
      cleanedContent
        .replace("\n", "\\n")
  }

  final case class PropertiesDatabase(listOfProperties: List[PostProperties])

  object PropertiesDatabase {
    def fromNotionDatabase(database: NotionDatabase): PropertiesDatabase =
      PropertiesDatabase(listOfProperties = database.results.map(PostProperties.fromNotionPage))
  }
}
