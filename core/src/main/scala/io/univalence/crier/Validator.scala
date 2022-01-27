package io.univalence.crier

import io.univalence.crier.Domain.Post

object Validator {
  final case class NotionPageValidator(predicate: Post => Boolean) {
    def and(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: Post) => predicate(page) && that.predicate(page))

    def or(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: Post) => predicate(page) || that.predicate(page))
  }

  val sizeValidator: NotionPageValidator            = NotionPageValidator(_.content.length < 280)
  val minimumKeywordsValidator: NotionPageValidator = NotionPageValidator(_.properties.keywords.nonEmpty)
  val mandatoryTypeValidator: NotionPageValidator   = NotionPageValidator(_.properties.kind.isDefined)

  val validatePage: NotionPageValidator = sizeValidator and minimumKeywordsValidator and mandatoryTypeValidator
}
