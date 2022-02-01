package io.univalence.crier

import sttp.client3._
import sttp.client3.asynchttpclient.zio._

import io.univalence.crier.Domain._
import io.univalence.crier.Main.Configuration

import zio.{Accessible, Task, ZIO, ZLayer}
import zio.config.getConfig

object Linkedin {
  trait LinkedinApi {
    def writePost(post: Post): Task[Unit]
  }

  object LinkedinApi extends Accessible[LinkedinApi]

  final case class LinkedinApiLive(configuration: Configuration, sttp: SttpClient) extends LinkedinApi {
    val url: String = "https://api.linkedin.com/v2"

    val defaultRequest: RequestT[Empty, Either[String, String], Any] =
      basicRequest.auth
        .bearer(configuration.linkedin.bearer)

    override def writePost(post: Post): Task[Unit] = {
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

      Api.succeedOrDieWithoutValue(sttp.send(request))
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
