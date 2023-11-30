ThisBuild / organization := "io.ginger.kdtrey5"
ThisBuild / version := "0.2.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.17" % Test

val skiis = "org.alexboisvert" %% "skiis" % "2.0.2-SNAPSHOT"

val awsDynamoDB = "software.amazon.awssdk" % "dynamodb" % "2.20.146"

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies += skiis,
    libraryDependencies += scalaTest
  )

lazy val dynamo = (project in file("dynamo"))
  .dependsOn(core)
  .settings(
    libraryDependencies += awsDynamoDB,
    libraryDependencies += "org.scanamo" %% "scanamo" % "1.0.0-M28",
    libraryDependencies += "org.scanamo" %% "scanamo-testkit" % "1.0.0-M28" % Test,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.9" % Test,
    libraryDependencies += scalaTest,
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(compile in Test).value,
    test in Test := (test in Test).dependsOn(startDynamoDBLocal).value,
    testOnly in Test := (testOnly in Test)
      .dependsOn(startDynamoDBLocal)
      .evaluated,
    testOptions in Test += dynamoDBLocalTestCleanup.value
  )
