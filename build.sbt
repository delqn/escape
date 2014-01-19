name := "escape"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

libraryDependencies += "postgresql" % "postgresql" % "9.1-901-1.jdbc4"

libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.5-6"

libraryDependencies += "postgresql" % "postgresql" % "9.1-901-1.jdbc4"

libraryDependencies += "com.restfb" % "restfb" % "1.6.11"

libraryDependencies += "com.h2database" % "h2" % "1.3.167"

play.Project.playScalaSettings
