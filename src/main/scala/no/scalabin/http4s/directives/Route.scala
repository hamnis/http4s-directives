package no.scalabin.http4s.directives

import cats.data.{NonEmptyList, OptionT}
import cats.effect.Sync
import org.http4s.dsl.impl.Path
import org.http4s.headers.Allow
import org.http4s.{HttpRoutes, Method, Response, Status}

object Route {
  def apply[F[_]: Sync](path: Path, methods: Map[Method, HttpRoutes[F]]): HttpRoutes[F] = HttpRoutes[F] {
    case req if Path(req.pathInfo) == path && methods.isDefinedAt(req.method) =>
      methods(req.method).run(req)
    case _ =>
      NonEmptyList.fromList(methods.keySet.toList) match {
        case Some(allowed) => OptionT.pure(Response[F](Status.MethodNotAllowed).putHeaders(Allow(allowed)))
        case None => OptionT.none
      }
  }

  def directive[F[_]: Sync](path: Path, pf: Map[Method, Directive[F, Response[F]]]): HttpRoutes[F] =
    apply(path, pf.mapValues(_.toHttpRoutes))
}
