package io.univalence.crier

import io.univalence.crier.Domain.{Post, PostKind, PostProperties, PostStatus}
import io.univalence.crier.Main.{assignPublicationDates, findTodayPost, partitionPosts, sortPosts}

import zio.test._
import zio.test.Assertion._

import java.time.{LocalDate, ZonedDateTime, ZoneId}

object MainTest extends DefaultRunnableSpec {
  val timeZone: ZoneId             = ZoneId.of("UTC")
  val zonedDateTime: ZonedDateTime = ZonedDateTime.of(2021, 10, 1, 0, 0, 0, 0, timeZone)

  val fakePost: Post =
    Post(
      properties =
        PostProperties(
          id              = "id",
          createdTime     = zonedDateTime,
          kind            = Some(PostKind.Tips),
          status          = Some(PostStatus.NotValid),
          keywords        = List(),
          publicationDate = None,
          link            = None
        ),
      content = List()
    )

  def spec: Spec[Annotations with Live with TestClock, TestFailure[Any], TestSuccess] =
    sortPostsSpec + partitionPostsSpec + assignPublicationDatesSpec + findTodayPostSpec

  val sortPostsSpec: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("sortPosts Spec")(
      test("sortPosts should sort by creation date if there is no publication date") {
        val firstPost: Post  = fakePost.withCreatedTime(zonedDateTime.minusDays(1))
        val secondPost: Post = fakePost

        val posts: List[Post] = List(secondPost, firstPost)

        assert(sortPosts(posts))(equalTo(List(firstPost, secondPost)))
      },
      test("sortPosts should prioritize some publication date over none") {
        val firstPost: Post  = fakePost.withPublicationDate(Some(LocalDate.of(2022, 1, 1)))
        val secondPost: Post = fakePost

        val posts: List[Post] = List(secondPost, firstPost)

        assert(sortPosts(posts))(equalTo(List(firstPost, secondPost)))
      },
      test("sortPosts should prioritize publication date over creation date") {
        val firstPost: Post =
          fakePost
            .withPublicationDate(Some(LocalDate.of(2022, 1, 1)))
        val secondPost: Post =
          fakePost
            .withPublicationDate(Some(LocalDate.of(2022, 1, 2)))
            .withCreatedTime(zonedDateTime.minusDays(1))

        val posts: List[Post] = List(secondPost, firstPost)

        assert(sortPosts(posts))(equalTo(List(firstPost, secondPost)))
      }
    )

  val partitionPostsSpec: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("partitionPosts Spec")(
      test("partitionPosts should separate posts according to their status in two groups") {
        val firstPost: Post  = fakePost.withStatus(PostStatus.NotValid)
        val secondPost: Post = fakePost.withStatus(PostStatus.Pending)
        val thirdPost: Post  = fakePost.withStatus(PostStatus.Posted)

        val posts: List[Post] = List(secondPost, firstPost, thirdPost)

        assert(partitionPosts(posts))(equalTo((List(secondPost), List(firstPost, thirdPost))))
      }
    )

  val assignPublicationDatesSpec: Spec[TestClock, TestFailure[Nothing], TestSuccess] =
    suite("assignPublicationDates Spec")(
      test("assignPublicationDates should assign consecutive publication date") {
        val posts: List[Post] = List(fakePost, fakePost)

        val dateTimeReference = zonedDateTime.plusDays(3).toOffsetDateTime

        // The 2021/10/4 is a Monday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
        } yield assert(dates)(equalTo(List(dateTimeReference.toLocalDate, dateTimeReference.toLocalDate.plusDays(1))))
      },
      test("assignPublicationDates should ignore saturday and sunday") {
        val posts: List[Post] = List(fakePost, fakePost)

        val dateTimeReference = zonedDateTime.toOffsetDateTime

        // The 2021/10/1 is a Friday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
        } yield assert(dates)(equalTo(List(dateTimeReference.toLocalDate, dateTimeReference.toLocalDate.plusDays(3))))
      },
      test("assignPublicationDates can be launched on saturday or friday without issues") {
        val posts: List[Post] = List(fakePost, fakePost)

        val dateTimeReference = zonedDateTime.plusDays(1).toOffsetDateTime

        // The 2021/10/2 is a Saturday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
        } yield assert(dates)(
          equalTo(List(dateTimeReference.toLocalDate.plusDays(2), dateTimeReference.toLocalDate.plusDays(3)))
        )
      }
    )

  val findTodayPostSpec: Spec[TestClock, TestFailure[Nothing], TestSuccess] =
    suite("findTodayPost Spec")(
      test("findTodayPost should find a post if its publication date is today") {
        val dateTimeReference = zonedDateTime.toOffsetDateTime

        val posts: List[Post] = List(fakePost.withPublicationDate(Some(dateTimeReference.toLocalDate)))

        for {
          _         <- TestClock.setDateTime(dateTimeReference)
          maybePost <- findTodayPost(posts)
        } yield assert(maybePost)(isSome)
      },
      test("findTodayPost should not find a post if its publication date is not today") {
        val dateTimeReference = zonedDateTime.toOffsetDateTime

        val posts: List[Post] = List(fakePost)

        for {
          _         <- TestClock.setDateTime(dateTimeReference)
          maybePost <- findTodayPost(posts)
        } yield assert(maybePost)(isNone)
      }
    )
}
