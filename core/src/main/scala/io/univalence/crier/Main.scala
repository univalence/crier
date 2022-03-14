package io.univalence.crier

import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}

import io.univalence.crier.Domain.{Post, PostProperties}
import io.univalence.crier.Domain.PostStatus.{NotValid, Pending, Posted}
import io.univalence.crier.api.Linkedin.{LinkedinApi, LinkedinApiLive}
import io.univalence.crier.api.Notion.{NotionApi, NotionApiLive}

import zio._
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

import java.time.{DayOfWeek, LocalDate}

object Main extends ZIOAppDefault {
  final case class Configuration(
      notion:   NotionConfiguration,
      linkedin: LinkedinConfiguration
  )

  final case class LinkedinConfiguration(
      bearer: String
  )

  final case class NotionConfiguration(
      bearer:   String,
      database: String
  )

  val configurationLayer: ZLayer[System, ReadError[String], Configuration] =
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
  def assignPublicationDates(posts: List[Post]): ZIO[Clock, Nothing, List[Post]] = {
    val sortedPosts = sortPosts(posts)
    val publicationDatesEffect =
      ZIO.collectAll(
        sortedPosts
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

    publicationDatesEffect.map(_.zip(sortedPosts).map { case (date, post) =>
      post.withPublicationDate(Some(date))
    })
  }

  /** Find the post to post today. */
  def findTodayPost(posts: List[Post]): ZIO[Clock, Nothing, Option[Post]] =
    Clock.localDateTime.map(now => posts.find(_.properties.publicationDate.contains(now.toLocalDate)))

  def processNotionDatabase: ZIO[Clock with Console with NotionApi, Throwable, List[Post]] =
    for {
      database <- NotionApi(_.retrieveDatabase)
      postProperties = database.listOfProperties.filter(postShouldBeChecked)
      _     <- Console.printLine(s"Retrieve ${postProperties.length} rows from Notion")
      posts <- NotionApi(_.retrievePosts(postProperties))
      validatedPosts                = posts.map(_.validate)
      (pendingPosts, notValidPosts) = partitionPosts(validatedPosts)
      _ <- Console.printLine(s"${notValidPosts.length} posts are not valid")
      _ <- Console.printLine(s"${pendingPosts.length} posts are pending")
      notValidPostsWithoutPublicationDate = notValidPosts.map(_.withPublicationDate(None))
      _                               <- NotionApi(_.updatePosts(notValidPostsWithoutPublicationDate))
      pendingPostsWithPublicationDate <- assignPublicationDates(pendingPosts)
      _                               <- NotionApi(_.updatePosts(pendingPostsWithPublicationDate))
    } yield pendingPostsWithPublicationDate

  def postPage(post: Post): ZIO[Console with NotionApi with LinkedinApi, Throwable, Unit] =
    for {
      _ <- Console.printLine(s"Posting the following content:\n${post.content}")
      _ <- LinkedinApi(_.writePost(post))
      _ <- NotionApi(_.updatePost(post.withStatus(Posted)))
    } yield ()

  def program: ZIO[Clock with Console with NotionApi with LinkedinApi, Throwable, Unit] =
    for {
      pendingPages <- processNotionDatabase
      todayPage    <- findTodayPost(pendingPages)
      _ <-
        todayPage match {
          case Some(page) => postPage(page)
          case None       => Console.printLine("No post to share today :(")
        }
    } yield ()

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    program
      .provide(
        System.live,
        Clock.live,
        Console.live,
        configurationLayer,
        sttpLayer,
        NotionApiLive.layer,
        LinkedinApiLive.layer
      )
}
