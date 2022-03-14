package io.univalence.crier.api

import sttp.client3.{Response, ResponseException}

import zio.{Task, ZIO}

object Api {
  // TODO: We should handle errors without throwing exception and using response information
  def succeedOrDie[E, A](task: Task[Response[Either[ResponseException[String, E], A]]]): Task[A] =
    for {
      response <- task
      item     <- ZIO.fromEither(response.body)
    } yield item

  def succeedOrDieWithoutValue(task: Task[Response[Either[String, String]]]): Task[Unit] =
    task.flatMap(r => ZIO.fromEither(r.body)).fold(e => throw new Exception(e.toString), _ => ())
}
