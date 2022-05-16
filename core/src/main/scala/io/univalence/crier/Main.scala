package io.univalence.crier

import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}

import io.univalence.crier.Domain.{Post, PostProperties}
import io.univalence.crier.Domain.PostStatus.{NotValid, Pending, Posted}
import io.univalence.crier.api.Linkedin.{LinkedinApi, LinkedinApiLive}
import io.univalence.crier.api.Notion.{NotionApi, NotionApiLive}
import io.univalence.crier.api.Slack.{SlackApi, SlackApiLive}

import zio._
import zio.config._
import zio.config.magnolia.descriptor
import zio.logging._
import zio.logging.LogFormat._
import zio.logging.backend.SLF4J

import java.time.{DayOfWeek, LocalDate}

object Main extends ZIOAppDefault {
  final case class Configuration(
      notion:   NotionConfiguration,
      linkedin: LinkedinConfiguration,
      slack:    SlackConfiguration
  )

  final case class SlackConfiguration(
      webhook: String
  )

  final case class LinkedinConfiguration(
      bearer: String
  )

  final case class NotionConfiguration(
      bearer:   String,
      database: String
  )

  val configurationLayer: Layer[ReadError[String], Configuration] =
    ZConfig.fromSystemEnv(descriptor[Configuration].mapKey(_.toUpperCase), keyDelimiter = Some('_'))

  val sttpLayer: Layer[Throwable, SttpClient] = AsyncHttpClientZioBackend.layer()

  /**
   * Return True if the validator system should check the page or not.
   */
  def postShouldBeChecked(properties: PostProperties): Boolean =
    properties.status.getOrElse(NotValid) match {
      case Posted => false
      case _      => true
    }

  /**
   * Sort pages to validate according to their creation time and the
   * publication date.
   */
  def sortPosts(pages: List[Post]): List[Post] =
    pages.sortBy(_.properties.createdTime).sortBy(_.properties.publicationDate.getOrElse(LocalDate.MAX))

  /** Separate posts between pending and other status. */
  def partitionPosts(posts: List[Post]): (List[Post], List[Post]) =
    posts.partition(_.properties.status.getOrElse(NotValid) == Pending)

  /**
   * Attribute publication date to valid pages.
   *
   * Posts should be send from Monday to Friday
   */
  def assignPublicationDates(posts: List[Post]): UIO[List[Post]] = {
    val sortedPosts = sortPosts(posts)
    val publicationDatesEffect =
      ZIO.collectAll(
        sortedPosts
          .scanLeft(Clock.localDateTime.map(_.toLocalDate.minusDays(1)))((previousDate, _) =>
            previousDate.map(date =>
              date.getDayOfWeek match {
                case DayOfWeek.FRIDAY                                            => date.plusDays(3)
                case DayOfWeek.SATURDAY | DayOfWeek.MONDAY | DayOfWeek.WEDNESDAY => date.plusDays(2)
                case _                                                           => date.plusDays(1)
              }
            )
          )
          .drop(1)
      )

    publicationDatesEffect.map(_.zip(sortedPosts).map { case (date, post) =>
      post.withPublicationDate(Some(date))
    })
  }

  /** Find the post to post today. */
  def findTodayPost(posts: List[Post]): UIO[Option[Post]] =
    Clock.localDateTime.map(now => posts.find(_.properties.publicationDate.contains(now.toLocalDate)))

  def processNotionDatabase: ZIO[NotionApi, Throwable, List[Post]] =
    for {
      database <- NotionApi(_.retrieveDatabase)
      postProperties = database.listOfProperties.filter(postShouldBeChecked)
      _     <- ZIO.logInfo(s"Retrieve ${postProperties.length} rows from Notion")
      posts <- NotionApi(_.retrievePosts(postProperties))
      validatedPosts                = posts.map(_.validate)
      (pendingPosts, notValidPosts) = partitionPosts(validatedPosts)
      _ <- ZIO.logInfo(s"${notValidPosts.length} posts are not valid")
      _ <- ZIO.logInfo(s"${pendingPosts.length} posts are pending")
      notValidPostsWithoutPublicationDate = notValidPosts.map(_.withPublicationDate(None))
      _                               <- NotionApi(_.updatePosts(notValidPostsWithoutPublicationDate))
      pendingPostsWithPublicationDate <- assignPublicationDates(pendingPosts)
      _                               <- NotionApi(_.updatePosts(pendingPostsWithPublicationDate))
    } yield pendingPostsWithPublicationDate

  def postPage(post: Post): ZIO[NotionApi with LinkedinApi with SlackApi, Throwable, Unit] =
    for {
      _                <- ZIO.logInfo(s"Posting the following content:\n${post.content}")
      linkedinResponse <- LinkedinApi(_.writePost(post))
      _                <- NotionApi(_.updatePost(post.withStatus(Posted)))
      _                <- SlackApi(_.sendMessage(post.toSlack(linkedinResponse.activity)))
    } yield ()

  def preventEmptyDatabase(pendingPosts: List[Post]): ZIO[SlackApi, Throwable, Unit] = {
    val url: String =
      "https://www.notion.so/univalence/3868f708ae46461fbfcf72d34c9536f9?v=26ccdeca69d849c09e2b372737ee2040"

    pendingPosts.length match {
      case x if x <= 1 =>
        SlackApi(
          _.sendMessage(
            s"""Aïe, le stock de post est vide, il ne reste plus aucun post en reserve.
               |
               |N'hésitez pas à rajouter du contenue: $url.""".stripMargin
          )
        )
      case x if x <= 4 =>
        SlackApi(
          _.sendMessage(
            s"""Aïe, le stock de post est casi vide, il ne reste plus que ${x - 1} posts en reserve.
               |
               |N'hésitez pas à rajouter du contenue: $url.""".stripMargin
          )
        )
      case _ => ZIO.unit
    }
  }

  def program: ZIO[NotionApi with LinkedinApi with SlackApi, Throwable, Unit] =
    for {
      pendingPosts <- processNotionDatabase
      todayPage    <- findTodayPost(pendingPosts)
      _ <-
        todayPage match {
          case Some(page) => postPage(page)
          case None       => ZIO.logInfo("No post to share today :(")
        }
      _ <- preventEmptyDatabase(pendingPosts)

    } yield ()

  val logFormat: LogFormat =
    label("timestamp", timestamp.fixed(32)).color(LogColor.BLUE) |-|
      label("thread", fiberId.fixed(12)).color(LogColor.WHITE) |-|
      label("message", quoted(line)).highlight

  override def hook: RuntimeConfigAspect =
    RuntimeConfigAspect(_.copy(logger = ZLogger.none)) >>>
      SLF4J.slf4j(zio.LogLevel.Debug, logFormat, _ => "logger")

  override def run: ZIO[ZIOAppArgs, Any, Any] =
    program
      .provide(
        configurationLayer,
        sttpLayer,
        NotionApiLive.layer,
        LinkedinApiLive.layer,
        SlackApiLive.layer
      )
}
