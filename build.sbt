name := "shapes-oo-scala-algebraic"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Ypartial-unification"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"           % "7.2.9",
  "com.slamdata"   %% "matryoshka-core"       % "0.17.0",
  "com.slamdata"   %% "matryoshka-scalacheck" % "0.17.0" % Test,
  "org.scalatest"  %% "scalatest"             % "3.0.1"  % Test
)
    