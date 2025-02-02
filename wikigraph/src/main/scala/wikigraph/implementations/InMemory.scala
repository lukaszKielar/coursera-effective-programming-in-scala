package wikigraph.implementations

import wikigraph.Articles.ArticleId
import wikigraph.WikiResult
import wikigraph.WikiResult.*
import wikigraph.Wikipedia
import wikigraph.errors.WikiError
import wikigraph.errors.WikiError.*

import scala.concurrent.ExecutionContext

final class InMemory(graph: Map[ArticleId, Set[ArticleId]]) extends Wikipedia:
  override def linksFrom(art: ArticleId)(using
      ExecutionContext
  ): WikiResult[Set[ArticleId]] =
    graph.get(art) match
      case Some(ids) => WikiResult.successful(ids)
      case None      => WikiResult.domainError(ArticleNotFound(art))
  override def nameOfArticle(
      art: ArticleId
  )(using ExecutionContext): WikiResult[String] =
    if graph.contains(art) then WikiResult.successful(s"TestArticle-${art.raw}")
    else WikiResult.domainError(TitleNotFound(art))
  override def searchId(title: String)(using
      ExecutionContext
  ): WikiResult[ArticleId] =
    val res = graph.keys
      .map(id => id -> s"TestArticle-${id.raw}")
      .find((_, t) => t == title)
    res match
      case None                 => WikiResult.domainError(NoResult(title))
      case Some((articleId, _)) => WikiResult.successful(articleId)
