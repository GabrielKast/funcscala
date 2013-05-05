// Set the project name to the string 'My Project'
name := "Functionnal Programing in scala : exercices"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
version := "1.0"

scalaVersion := "2.10.1"

// Add a single dependency
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"
