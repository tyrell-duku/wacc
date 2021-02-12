name := "wacc_12"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies += "com.github.j-mie6" %% "parsley" % "2.8.3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
assemblyJarName in assembly := "wacc.jar"
test in assembly := {}
