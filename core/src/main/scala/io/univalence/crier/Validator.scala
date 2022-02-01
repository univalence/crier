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

  val subjectDefined: NotionPageValidator            = NotionPageValidator(_.properties.subject.isDefined)
  val tipsNonEmpty: NotionPageValidator              = NotionPageValidator(_.tips.nonEmpty)
  val contentSizeGreaterThan600: NotionPageValidator = NotionPageValidator(_.content.length < 600)
  val minimumOneKeyword: NotionPageValidator         = NotionPageValidator(_.properties.keywords.nonEmpty)
  val hasAType: NotionPageValidator                  = NotionPageValidator(_.properties.kind.isDefined)
  val keywordsHaveNoSpace: NotionPageValidator       = keywordValidator(!_.contains(" "))
  val keywordsAreLower: NotionPageValidator          = keywordValidator(_.map(_.isLower).forall(_ == true))

  val validatePage: NotionPageValidator =
    subjectDefined and tipsNonEmpty and contentSizeGreaterThan600 and minimumOneKeyword and hasAType and keywordsHaveNoSpace and keywordsAreLower
}
