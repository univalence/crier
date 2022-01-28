package io.univalence.crier

import io.univalence.crier.Domain.Post

object Validator {
  final case class NotionPageValidator(predicate: Post => Boolean) {
    def and(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: Post) => predicate(page) && that.predicate(page))

    def or(that: NotionPageValidator): NotionPageValidator =
      NotionPageValidator((page: Post) => predicate(page) || that.predicate(page))
  }

  def keywordValidator(p: String => Boolean): NotionPageValidator =
    NotionPageValidator(_.properties.keywords.map(p).forall(_ == true))

  val contentSizeGreaterThan600: NotionPageValidator = NotionPageValidator(_.content.length < 600)
  val minimumOneKeyword: NotionPageValidator         = NotionPageValidator(_.properties.keywords.nonEmpty)
  val hasAType: NotionPageValidator                  = NotionPageValidator(_.properties.kind.isDefined)
  val keywordsHaveNoSpace: NotionPageValidator       = keywordValidator(!_.contains(" "))
  val keywordsAreLower: NotionPageValidator          = keywordValidator(_.map(_.isLower).forall(_ == true))

  val validatePage: NotionPageValidator =
    contentSizeGreaterThan600 and minimumOneKeyword and hasAType and keywordsHaveNoSpace and keywordsAreLower
}
