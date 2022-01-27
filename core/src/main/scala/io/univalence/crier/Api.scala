package io.univalence.crier

import sttp.client3.{Response, ResponseException}

import zio.{Task, ZIO}

object Api {
  // TODO: We should handle errors without throwing exception and using response information
  def succeedOrDieWithLog[E, A](task: Task[Response[Either[ResponseException[String, E], A]]]): Task[A] =
    for {
      response <- task
      item     <- ZIO.fromEither(response.body)
    } yield item
}
