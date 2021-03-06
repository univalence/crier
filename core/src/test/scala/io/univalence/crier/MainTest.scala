package io.univalence.crier

import io.univalence.crier.Domain.{Post, PostStatus}
import io.univalence.crier.Fixtures._
import io.univalence.crier.Main.{assignPublicationDates, findTodayPost, partitionPosts, sortPosts}

import zio.test._
import zio.test.Assertion._

import java.time.{DayOfWeek, LocalDate}

object MainTest extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    sortPostsSpec + partitionPostsSpec + assignPublicationDatesSpec + findTodayPostSpec

  val sortPostsSpec: Spec[TestEnvironment, Any] =
    suite("sortPosts Spec")(
      test("sortPosts should sort by creation date if there is no publication date") {
        val firstPost: Post  = fakePost.withCreatedTime(zonedDateTime.minusDays(1))
        val secondPost: Post = fakePost

        val posts: List[Post] = List(secondPost, firstPost)

        assertTrue(sortPosts(posts) == List(firstPost, secondPost))
      },
      test("sortPosts should prioritize some publication date over none") {
        val firstPost: Post  = fakePost.withPublicationDate(Some(LocalDate.of(2022, 1, 1)))
        val secondPost: Post = fakePost

        val posts: List[Post] = List(secondPost, firstPost)

        assertTrue(sortPosts(posts) == List(firstPost, secondPost))
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

        assertTrue(sortPosts(posts) == List(firstPost, secondPost))
      }
    )

  val partitionPostsSpec: Spec[TestEnvironment, Any] =
    suite("partitionPosts Spec")(
      test("partitionPosts should separate posts according to their status in two groups") {
        val firstPost: Post  = fakePost.withStatus(PostStatus.NotValid)
        val secondPost: Post = fakePost.withStatus(PostStatus.Pending)
        val thirdPost: Post  = fakePost.withStatus(PostStatus.Posted)

        val posts: List[Post] = List(secondPost, firstPost, thirdPost)

        assertTrue(partitionPosts(posts) == (List(secondPost), List(firstPost, thirdPost)))
      }
    )

  val assignPublicationDatesSpec: Spec[TestEnvironment, Any] =
    suite("assignPublicationDates Spec")(
      test("assignPublicationDates should assign consecutive publication date") {
        val posts: List[Post] = List(fakePost, fakePost)

        val dateTimeReference = zonedDateTime.plusDays(3).toOffsetDateTime

        // The 2021/10/4 is a Monday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
          days  = dates.map(_.getDayOfWeek)
        } yield assertTrue(days == List(DayOfWeek.MONDAY, DayOfWeek.WEDNESDAY))
      },
      test("assignPublicationDates should ignore thursday, tuesday, saturday and sunday") {
        val posts: List[Post] = List.fill(20)(fakePost)

        val dateTimeReference = zonedDateTime.toOffsetDateTime

        val availableDayOfWeeks = Set(DayOfWeek.MONDAY, DayOfWeek.WEDNESDAY, DayOfWeek.FRIDAY)

        // The 2021/10/1 is a Friday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
          days  = dates.map(_.getDayOfWeek)
        } yield assertTrue(days.forall(availableDayOfWeeks(_)))
      },
      test("assignPublicationDates can be launched on saturday without issues") {
        val posts: List[Post] = List(fakePost, fakePost)

        val dateTimeReference = zonedDateTime.plusDays(1).toOffsetDateTime

        // The 2021/10/2 is a Saturday
        for {
          _             <- TestClock.setDateTime(dateTimeReference)
          postsWithDate <- assignPublicationDates(posts)
          dates = postsWithDate.map(_.properties.publicationDate.get)
          days  = dates.map(_.getDayOfWeek)
        } yield assertTrue(days == List(DayOfWeek.MONDAY, DayOfWeek.WEDNESDAY))
      }
    )

  val findTodayPostSpec: Spec[TestEnvironment, Any] =
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
