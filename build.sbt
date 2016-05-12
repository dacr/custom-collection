name := "custom-collection"

version := "2016-05-12"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

mainClass in assembly := Some("dummy.Dummy")

jarName in assembly := "custcol.jar"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
 ,"junit"          % "junit"     % "4.12"  % "test"
)


sourceGenerators in Compile <+= 
 (sourceManaged in Compile, version, name, jarName in assembly) map {
  (dir, version, projectname, jarexe) =>
  val file = dir / "dummy" / "MetaInfo.scala"
  IO.write(file,
  """package dummy
    |object MetaInfo { 
    |  val version="%s"
    |  val projectName="%s"
    |  val jarbasename="%s"
    |}
    |""".stripMargin.format(version, projectname, jarexe.split("[.]").head) )
  Seq(file)
}
