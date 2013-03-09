\name{print.gains}
\alias{print.gains}
\title{Printing Gains Table Objects}
\description{
  Print method for objects of class \code{gains}.
}
\usage{
\method{print}{gains}(x, digits=2, \dots)
}
\arguments{
  \item{x}{an object of class \code{gains}.}
  \item{digits}{number of digits to print after the decimal point for mean responses and model scores.}
  \item{\dots}{additional arguments to \code{print}.}
}
\seealso{
  \code{gains}, \code{print}.
}
\examples{
data(ciaScores)
print(gains(actual=ciaScores$CellPhonesPP[ciaScores$train==0],
            predicted=ciaScores$PredOLS[ciaScores$train==0]),
      digits=2)
}
\keyword{misc}
