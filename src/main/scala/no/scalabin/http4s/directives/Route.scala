package no.scalabin.http4s.directives

import cats.data.OptionT
import cats.effect.Sync
import org.http4s.dsl.impl.Path
import org.http4s.headers.Allow
import org.http4s.{AuthedRoutes, HttpRoutes, Method, Response, Status}

object Route {
  def of[F[_]: Sync](pf: PartialFunction[Path, Map[Method, HttpRoutes[F]]]): HttpRoutes[F] = HttpRoutes[F] { req =>
    val path = Path(req.pathInfo)
    if (pf.isDefinedAt(path)) {
      val methods = pf(path)
      val route = methods.getOrElse(
        req.method,
        HttpRoutes.pure(
          Response[F](Status.MethodNotAllowed).putHeaders(Allow(methods.keySet))
        )
      )
      route(req)
    } else OptionT.none
  }
  def apply[F[_]: Sync](path: Path, methods: (Method, HttpRoutes[F])*): HttpRoutes[F] =
    of { case `path` => methods.toMap }

  def directive[F[_]: Sync](pf: PartialFunction[Path, Map[Method, Directive[F, Response[F]]]]): HttpRoutes[F] =
    of { case path if pf.isDefinedAt(path) => pf(path).map { case (m, dir) => m -> dir.toHttpRoutes } }
}

object AuthedRoute {
  def of[A, F[_]: Sync](pf: PartialFunction[Path, Map[Method, AuthedRoutes[A, F]]]): AuthedRoutes[A, F] =
    AuthedRoutes[A, F] { authed =>
      val path = Path(authed.req.pathInfo)
      if (pf.isDefinedAt(path)) {
        val methods = pf(path)
        val route: AuthedRoutes[A, F] = methods.getOrElse(
          authed.req.method,
          AuthedRoutes(
            _ => OptionT.some[F](Response[F](Status.MethodNotAllowed).putHeaders(Allow(methods.keySet)))
          )
        )
        route(authed)
      } else OptionT.none
    }
  def apply[A, F[_]: Sync](path: Path, methods: (Method, AuthedRoutes[A, F])*): AuthedRoutes[A, F] =
    of { case `path` => methods.toMap }

  def directive[A, F[_]: Sync](pf: PartialFunction[Path, Map[Method, Directive[F, Response[F]]]]): AuthedRoutes[A, F] =
    of {
      case path if pf.isDefinedAt(path) =>
        pf(path).map {
          case (m, dir) =>
            m -> AuthedRoutes[A, F](req => dir.toHttpRoutes.run(req.req))
        }
    }

}
