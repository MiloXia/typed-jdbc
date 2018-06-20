name := "typed-jdbc"

version := "1.0.0"

scalaVersion := "2.12.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

unmanagedBase := baseDirectory.value / "libs"

// resolvers += "main" at "http://repo1.maven.org/maven2"
// resolvers += Resolver.mavenLocal
// resolvers ++= Seq(
//   Resolver.sonatypeRepo("releases"),
//   Resolver.sonatypeRepo("snapshots")
// )
// resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


// libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.1"
// libraryDependencies += "com.chuusai" % "shapeless_2.12" % "2.3.2"
// libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.41"
addCompilerPlugin("org.spire-math" % "kind-projector_2.12" % "0.9.3")



