import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import java.nio.file.Paths
import fs2.io.file.Path
import java.nio.file.Files
import org.http4s.ember.client.EmberClientBuilder

class MainSuite extends CatsEffectSuite {

  test("should read test1.env") {
    val path = getTestFile("test1.env")

    Main
      .collectVersions(path)
      .assertEquals(
        Seq(
          Application(
            lineNumber = 4,
            variableName = "N1_VERSION",
            version = "0.0.0",
            repository = Repository(
              owner = "repository1",
              name = "name1"
            )
          ),
          Application(
            lineNumber = 7,
            variableName = "N2_VERSION",
            version = "1.1.1",
            repository = Repository(
              owner = "repository2",
              name = "name2"
            )
          )
        )
      )
  }

  test("should find latest version") {
    val application =
      createApplication(Repository("actions", "upload-release-asset"))
    val newVersion = EmberClientBuilder
      .default[IO]
      .build
      .use(client => Main.updateSingleVersion(application, None, client))

    newVersion.map(_.version).assertEquals("1.0.2")
  }

  private def createApplication(repository: Repository): Application =
    Application(
      lineNumber = -1,
      variableName = "EMPTY",
      version = "0.0.0",
      repository = repository
    )

  def getTestFile(name: String): Path = {
    val p1 = Paths.get("test-data", name)
    val p2 = Paths.get("..", "test-data", name)

    if (Files.exists(p1)) {
      Path.fromNioPath(p1)
    } else if (Files.exists(p2)) {
      Path.fromNioPath(p2)
    } else {
      throw RuntimeException(s"Cannot find test file: ${name}")
    }
  }

}
