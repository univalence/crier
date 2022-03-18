package io.univalence.crier

import io.univalence.crier.Domain.{Post, PostKind, PostProperties, PostStatus}

import java.time.{ZonedDateTime, ZoneId}

object Fixtures {
  val timeZone: ZoneId             = ZoneId.of("UTC")
  val zonedDateTime: ZonedDateTime = ZonedDateTime.of(2021, 10, 1, 0, 0, 0, 0, timeZone)

  val fakePost: Post =
    Post(
      authors = List("Jon Doe"),
      properties =
        PostProperties(
          id              = "id",
          authorIds       = List("azerty"),
          subject         = None,
          createdTime     = zonedDateTime,
          kind            = Some(PostKind.Tips),
          status          = Some(PostStatus.NotValid),
          keywords        = List(),
          publicationDate = None,
          link            = None
        ),
      lines =
        List(
          "This is my daily post.",
          "With a lot of content!"
        )
    )
}
