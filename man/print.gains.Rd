\name{print.gains}
\alias{print.gains}
\title{Printing Gains Table Objects}
\description{
  Print method for objects of class \code{gains}.
}
\usage{
\method{print}{gains}(x, digits=2, ...)
}
\arguments{
  \item{x}{an object of class \code{gains}.}
  \item{digits}{minimum number of significant digits to print.  See \code{print.default}.}
  \item{\dots}{additional arguments to \code{print}}.
}
\seealso{
  \code{gains}, \code{print}.
}
\examples{
data(ciaScores)
print(with(subset(ciaScores,train==0), 
      gains(actual=CellPhonesPP, predicted=PredOLS, optimal=TRUE)),digits=2)
}
\keyword{misc}
