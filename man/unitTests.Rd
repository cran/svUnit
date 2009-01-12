\name{unitTests}
\alias{unitTests.svUnit}

\title{ Unit tests for the package svUnit }
\description{
  Performs unit tests defined in this package by running
  \code{example(unitTests.svUnit)}. Tests are in \code{runit*.R} files located
  in the '/unitTests' subdirectory or one of its subdirectories ('/inst/unitTests'
  and subdirectories in package sources).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\examples{
library(svUnit)
# Make sure to clear log of errors and failures first
clearLog()

# Run all test units defined in the 'svUnit' package
(runTest(svSuite("package:svUnit"), "svUnit"))

\donttest{
# Tests to run with example() but not with R CMD check
# Run all test units defined in the /unitTests/VirtualClass subdir of 'svUnit'
(runTest(svSuite("package:svUnit (VirtualClass)"), "VirtualClass"))
}

\dontrun{
# Tests to present in ?unitTests.svUnit but to never run automatically
# Run all currently loaded test cases and test suites of all loaded packages
(runTest(svSuiteList(), "AllTests"))
}

\dontshow{
# Put here test units you want to run during R CMD check but don't want to show
# or run with example(unitTests.svUnit)
}

# Check errors at the end of the process (needed to interrupt R CMD check)
errorLog()
}

\keyword{utilities}
