import cats.Parallel
import cats.effect.*
import cats.effect.implicits.{_, given}
import cats.effect.kernel.syntax.resource
import cats.effect.std.Env
import cats.implicits.{_, given}
import com.monovore.decline._
import com.monovore.decline.effect.CommandIOApp
import fs2.hash
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.EntityDecoder
import org.http4s.Request
import org.http4s.*
import org.http4s.circe.jsonOf
import org.http4s.client._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Accept
import org.http4s.headers.*
import org.http4s.implicits._
import org.http4s.syntax.header
import org.typelevel.ci.CIString

import java.io.File
import java.nio.file.Paths
import java.nio.file.{Path => JPath}
import scala.util.boundary
import scala.util.boundary.break

object Main
    extends CommandIOApp(
      name = "dotenv-github-release-updater",
      header = "Release updater for .env files",
      version = "DEV"
    ):

  val githubTokenOpts =
    Opts.option[String]("github-token", help = "Github token")
  val fileOpts =
    Opts.option[JPath]("file", help = ".env file").map(Path.fromNioPath)

  override def main: Opts[IO[ExitCode]] = {
    (githubTokenOpts, fileOpts).mapN((githubToken, file) => {
      EmberClientBuilder
        .default[IO]
        .build
        .use(client => run0(githubToken, file, client))
    })
  }

  def run0(githubToken: String, file: Path, client: Client[IO]): IO[ExitCode] =
    for {
      currentVersions <- collectVersions(file)
      newVersions <- updateVersions(currentVersions, githubToken, client)
      _ <- replaceVersions(file, newVersions)
    } yield ExitCode.Success


  def collectVersions(p: Path): IO[Seq[Application]] =
    Files[IO]
      .readUtf8Lines(p)
      .zipWithNext
      .zipWithIndex
      .collect { case ((s, Some(s2)), line) => (s, s2, line) }
      .map((line1, line2, index) => getApplicationIfExists(line1, line2, index))
      .collect { case Some(x) => x }
      .compile
      .toList

  def replaceVersions(p: Path, application: Seq[Application]): IO[Unit] =
    for {
      newContentLines <- Files[IO]
        .readUtf8Lines(p)
        .zipWithIndex
        .map((line, index) => (line, (index + 1)))
        .map((line, lineNumber) => {

          application.find(_.lineNumber == lineNumber) match {
            case Some(app) => {
              s"${app.variableName}=${app.version}"
            }
            case None => line
          }

        })
        .compile
        .toList

      _ <- fs2
        .Stream.emits(newContentLines)
        .through(Files[IO].writeUtf8Lines(p))
        .compile
        .drain

    } yield ()

  def updateSingleVersion(
      application: Application,
      token: Option[String],
      client: Client[IO]
  ): IO[Application] = {

    val request = Request[IO](
      method = Method.GET,
      uri = Uri.unsafeFromString(
        s"https://api.github.com/repos/${application.repository.owner}/${application.repository.name}/releases/latest"
      ),
      headers = Headers(
        Accept.parse("application/vnd.github+json").toOption.get,
        Header.Raw(
          CIString("X-GitHub-Api-Version"),
          "2022-11-28"
        )
      ) ++ token.fold(
        Headers()
      )(token =>
        Headers(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
      )
    )

    for {
      response <- client.expect[GetLatestVersionResponse](request)
    } yield application.copy(version = response.tag_name.stripPrefix("v"))
  }

  def updateVersions(
      applications: Seq[Application],
      token: String,
      client: Client[IO]
  ): IO[Seq[Application]] = {
    applications.parTraverse(app =>
      updateSingleVersion(app, Some(token), client)
    )
  }

  private def replaceVersionIfRequired(
      line: String,
      applications: Seq[Application]
  ): String = {
    boundary {
      for (application <- applications) {
        if (line.startsWith(s"${application.variableName}=")) {
          break(s"${application.variableName}=${application.version}")
        }
      }
      line
    }
  }

  private def getApplicationIfExists(
      firstLine: String,
      secondLine: String,
      index: Long
  ): Option[Application] = {
    val linkRegex = ".*https://github.com/(.*)/(.*)/releases.*".r
    val versionRegex = "(.*)=(.*)".r

    (firstLine, secondLine) match {
      case (linkRegex(owner, name), versionRegex(variableName, version)) =>
        Some(
          Application(
            lineNumber = index + 2,
            variableName = variableName,
            version = version,
            repository = Repository(
              owner = owner,
              name = name
            )
          )
        )
      case _ => None
    }
  }

case class Repository(owner: String, name: String)
case class Application(
    lineNumber: Long,
    variableName: String,
    repository: Repository,
    version: String
)

case class GetLatestVersionResponse(
    tag_name: String
)

given EntityDecoder[IO, GetLatestVersionResponse] =
  jsonOf[IO, GetLatestVersionResponse]
