lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "bnord",
      scalaVersion := "3.0.2",
      version      := "0.0.1",
      scalacOptions := Seq("-unchecked", "-deprecation")
    )),
    name := "ltl2aut",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )
