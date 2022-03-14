package io.univalence.crier.api

import sttp.client3._
import sttp.client3.asynchttpclient.zio._

import io.univalence.crier.Main.Configuration

import zio.{Accessible, Task, ZIO, ZLayer}
import zio.config._

object Slack {
  trait SlackApi {
    def sendMessage(message: String): Task[Unit]
  }

  object SlackApi extends Accessible[SlackApi]

  final case class SlackApiLive(configuration: Configuration, sttp: SttpClient) extends SlackApi {
    val url: String = s"https://hooks.slack.com/services/${configuration.slack.webhook}"

    override def sendMessage(message: String): Task[Unit] = {
      val body = s"{\"text\": \"$message\"}"

      val request =
        basicRequest
          .header("Content-Type", "application/json")
          .body(body)
          .post(uri"$url")

      Api.succeedOrDieWithoutValue(sttp.send(request))
    }
  }

  object SlackApiLive {
    val layer: ZLayer[SttpClient with Configuration, Nothing, SlackApiLive] =
      ZLayer {
        for {
          configuration <- getConfig[Configuration]
          sttp          <- ZIO.service[SttpClient]
        } yield new SlackApiLive(configuration, sttp)
      }
  }
}
