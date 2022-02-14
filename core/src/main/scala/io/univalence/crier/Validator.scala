package io.univalence.crier

import io.univalence.crier.Domain.Post

object Validator {
  final case class NotionPageValidator(predicate: Post => List[String]) {
    def and(that: NotionPageValidator): NotionPageValidator =
      new NotionPageValidator((page: Post) => predicate(page) ++ that.predicate(page))
  }

  object NotionPageValidator {
    def build(predicate: Post => Boolean, error: String): NotionPageValidator =
      NotionPageValidator(post => if (predicate(post)) List[String]() else List(error))
  }

  def keywordValidator(p: String => Boolean, error: String): NotionPageValidator =
    NotionPageValidator.build(post => post.properties.keywords.map(p).forall(_ == true), error)

  val subjectDefined: NotionPageValidator =
    NotionPageValidator.build(
      _.properties.subject.isDefined,
      "Un post doit avoir un titre"
    )
  val tipsNonEmpty: NotionPageValidator =
    NotionPageValidator.build(
      _.tips.nonEmpty,
      "Un post ne peut pas être vide"
    )
  val contentSizeGreaterThan600: NotionPageValidator =
    NotionPageValidator.build(
      _.content.length < 1500,
      "Un post ne peut pas faire plus de 1500 charactères (lien et mots clés compris)"
    )
  val minimumOneKeyword: NotionPageValidator =
    NotionPageValidator.build(
      _.properties.keywords.nonEmpty,
      "Un post doit avoir au moins un mot clé"
    )
  val hasAType: NotionPageValidator =
    NotionPageValidator.build(
      _.properties.kind.isDefined,
      "Un post doit avoir un type définit"
    )
  val keywordsHaveNoSpace: NotionPageValidator =
    keywordValidator(
      !_.contains(" "),
      "Les mots clés ne doivent pas avoir d'espaces"
    )
  val keywordsAreLower: NotionPageValidator =
    keywordValidator(
      _.map(c => c.isLower || c.isDigit).forall(_ == true),
      "Les mots clés doivent être en minuscule"
    )

  val validatePage: NotionPageValidator =
    subjectDefined and tipsNonEmpty and contentSizeGreaterThan600 and minimumOneKeyword and hasAType and keywordsHaveNoSpace and keywordsAreLower
}
