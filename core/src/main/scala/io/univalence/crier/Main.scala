package io.univalence.crier

import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}

import io.univalence.crier.Domain.{Post, PostProperties}
import io.univalence.crier.Domain.PostStatus.{NotValid, Pending, Posted}
import io.univalence.crier.Notion.{NotionApi, NotionApiLive}
import io.univalence.crier.Validator.validatePage

import zio._
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

import java.time.{DayOfWeek, LocalDate}

object Main extends ZIOAppDefault {
  final case class Configuration(
      notionBearer: String,
      databaseId:   String
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

  /**
   * Validate the pages according to our predicates and separate them
   * between pending and other status.
   */
  def validateAndPartitionPosts(posts: List[Post]): (List[Post], List[Post]) =
    posts.map(_.validate).partition(_.properties.status.getOrElse(NotValid) == Pending)

  /** Attribute publication date to valid pages. */
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

  def updateNotionDatabase: ZIO[Clock with Console with NotionApi, Throwable, List[Post]] =
    for {
      database <- NotionApi(_.retrieveDatabase)
      postProperties = database.listOfProperties.filter(postShouldBeChecked)
      posts <- NotionApi(_.retrievePosts(postProperties))
      (pendingPosts, notValidPosts)       = validateAndPartitionPosts(posts)
      notValidPostsWithoutPublicationDate = notValidPosts.map(_.withPublicationDate(None))
      _                               <- NotionApi(_.updatePosts(notValidPostsWithoutPublicationDate))
      pendingPostsWithPublicationDate <- assignPublicationDates(pendingPosts)
      _                               <- NotionApi(_.updatePosts(pendingPostsWithPublicationDate))
    } yield pendingPostsWithPublicationDate

  def postPage(post: Post): ZIO[NotionApi, Throwable, Unit] =
    for {
      _ <- ZIO.collectAllPar(List()) // TODO : send post via Linkedin / Twitter
      _ <- NotionApi(_.updatePost(post.withStatus(Posted)))
    } yield ()

  def program: ZIO[Clock with Console with NotionApi, Throwable, Unit] =
    for {
      pendingPages <- updateNotionDatabase
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
