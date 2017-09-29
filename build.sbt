name := "GoMoviesCrawler"

version := "0.1"

scalaVersion := "2.12.3"

val http4sVersion = "0.17.1"

libraryDependencies ++= Seq(
    "org.scalaj" %% "scalaj-http" % "2.3.0",
    "io.circe" %% "circe-parser" % "0.8.0",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.19"
)
