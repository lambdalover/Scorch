name := "scorch"

 // Only needed to track snapshot releases, SBT automatically includes the releases repository.
  resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"
