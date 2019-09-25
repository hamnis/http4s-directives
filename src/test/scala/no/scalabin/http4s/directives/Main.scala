package no.scalabin.http4s.directives

import java.time.LocalDateTime

import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

object Main extends IOApp {

  override def run(args: List[String]) = {
    val dsl = new DirectivesDsl[IO] with DirectiveDslOps[IO]
    import dsl._

    val lm = LocalDateTime.now()

    val service = Route.directive(
      Root / "hello",
      Map(GET -> (for {
        res <- ifModifiedSinceDir(lm, Ok("Hello World"))
        foo <- request.queryParam("foo")
        if foo.isDefined or BadRequest("You didn't provide a foo, you fool!")
      } yield res))
    ).orNotFound

    BlazeServerBuilder[IO].bindHttp(8080, "localhost").withHttpApp(service).resource.use(_ => IO.never).as(ExitCode.Success)
  }
}
