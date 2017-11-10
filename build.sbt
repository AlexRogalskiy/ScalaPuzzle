import Dependencies._

lazy val root = (project in file(".")).
settings(
    inThisBuild(
		List(
			organization := "com.wildbeeslabs",
			scalaVersion := "2.12.3",
			version      := "0.1.0-SNAPSHOT"
		)
	),
    name := "Puzzle",
    libraryDependencies += scalaTest % Test,
	scalacOptions := List("-encoding", "utf8", "-Xfatal-warnings", "-deprecation", "-unchecked"),
	scalacOptions := {
		val old = scalacOptions.value
		scalaBinaryVersion.value match {
			case "2.12" => old
			case _      => old filterNot (Set("-Xfatal-warnings", "-deprecation").apply)
		}
	}
)


















