// Set the project name to the string 'My Project'
name := "Functionnal Programing in scala : exrcices"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
version := "1.0"

scalaVersion := "2.10.1"

// Add a single dependency
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"


libraryDependencies += "junit" % "junit" % "4.8" % "test"

// Exclude backup files by default.  This uses ~=, which accepts a function of
//  type T => T (here T = FileFilter) that is applied to the existing value.
// A similar idea is overriding a member and applying a function to the super value:
//  override lazy val defaultExcludes = f(super.defaultExcludes)
//
//  Some equivalent ways of writing this:
// defaultExcludes ~= (filter => filter || "*~")
// defaultExcludes ~= (_ || "*~")
// defaultExcludes ~= ( (_: FileFilter) || "*~")
// defaultExcludes ~= ( (filter: FileFilter) => filter || "*~")


// Use the project version to determine the repository to publish to.
publishTo <<= version { (v: String) =>
  if(v endsWith "-SNAPSHOT")
    Some(ScalaToolsSnapshots)
  else
    Some(ScalaToolsReleases)
}