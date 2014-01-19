// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "RestFB repository" at "https://github.com/revetkn/restfb"

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.2.1")

addSbtPlugin("com.ketalo.play.plugins" % "emberjs" % "1.2.0-SNAPSHOT")
