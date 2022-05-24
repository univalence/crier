package io.univalence.crier.api

import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.client3.circe._

import io.univalence.crier.Domain.Post
import io.univalence.crier.Main.Configuration

import zio._
import zio.config._

object Linkedin {
  final case class LinkedinResponse(activity: String)

  trait LinkedinApi {
    def writePost(post: Post): Task[LinkedinResponse]
  }

  object LinkedinApi {
    def writePost(post: Post): ZIO[LinkedinApi, Throwable, LinkedinResponse] =
      ZIO.service[LinkedinApi].flatMap(_.writePost(post))
  }

  final case class LinkedinApiLive(configuration: Configuration, sttp: SttpClient) extends LinkedinApi {
    val url: String = "https://api.linkedin.com/v2"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.linkedin.bearer)

    override def writePost(post: Post): Task[LinkedinResponse] = {
      val body =
        s"""{
           |    "distribution": {
           |        "linkedInDistributionTarget": {}
           |    },
           |    "owner": "urn:li:organization:18403479",
           |    "subject": "${post.properties.subject}",
           |    "text": {
           |        "text": "${post.escapedContent}"
           |      }
           |}
           |""".stripMargin

      val request =
        defaultRequest
          .header("Content-Type", "application/json")
          .body(body)
          .post(uri"$url/shares")
          .response(asJson[LinkedinResponse])

      Api.succeedOrDie(sttp.send(request))
    }
  }

  object LinkedinApiLive {
    val layer: ZLayer[SttpClient with Configuration, Nothing, LinkedinApiLive] =
      ZLayer {
        for {
          configuration <- getConfig[Configuration]
          sttp          <- ZIO.service[SttpClient]
        } yield new LinkedinApiLive(configuration, sttp)
      }
  }
}
