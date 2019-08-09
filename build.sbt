
ThisBuild / organization := "io.ginger"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.8"

/*
lazy val kdtrey5 = (project in file("."))
  .settings(
    name := "kdtrey5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    mainClass in (Compile, run) := Some("io.ginger.kdtrey5.RecordManager")
  )
*/

 val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" % Test


lazy val root = (project in file("."))
  .aggregate(core, dynamo)
  .settings(
  )


lazy val core = (project in file("core"))
  .settings(
    libraryDependencies += scalaTest
  )

lazy val dynamo = (project in file("dynamo"))
  .dependsOn(core)
  .settings(
    libraryDependencies += "org.scanamo" %% "scanamo" % "1.0.0-M10",
    libraryDependencies += "org.scanamo" %% "scanamo-testkit" % "1.0.0-M10" % Test,
    libraryDependencies += scalaTest,
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in Test).value,
    test in Test := (test in Test).dependsOn(startDynamoDBLocal).value,
    testOnly in Test := (testOnly in Test).dependsOn(startDynamoDBLocal).evaluated,
    testOptions in Test += dynamoDBLocalTestCleanup.value
  )




