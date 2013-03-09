\name{plot.gains}
\alias{plot.gains}
\title{Plotting Gains Table Objects}
\description{
  Plot method for objects of class \code{gains}.
}
\usage{
\method{plot}{gains}(x, col1="darkred", col2="blue4", 
                     xlab="Depth of File",
                     ylab="Sample Mean Response",
                     leg1="Mean Response",
                     leg2="Cumulative Mean Response",
                     type="b",
                     pch1=1,pch2=1,
                     lty1=1,lty2=1,
                     main="Gains Table Plot",\dots)
}
\arguments{
  \item{x}{an object of class \code{gains}.}
  \item{col1}{color to use for the series of mean response rates.}
  \item{col2}{color to use for the series of cumulative mean response rates.}
  \item{xlab}{a title for the x axis.  See \code{title}.}
  \item{ylab}{a title for the y axis.  See \code{title}.}
  \item{leg1}{label for the legend item corresponding to mean response rates.}
  \item{leg2}{label for the legend item corresponding to cumulative mean response rates.}
  \item{type}{what type of plot should be drawn.  The default is ``b'' for points and lines.}
  \item{pch1}{plotting character to use for the series of mean response rates.}
  \item{pch2}{plotting character to use for the series of cumulative mean response rates.}
  \item{lty1}{line type to use for the series of mean response rates.}
  \item{lty2}{line type to use for the series of cumulative mean response rates.}
  \item{main}{an overall title for the plot.  See \code{title}.}
  \item{\dots}{additional arguments to \code{plot}}.
}
\seealso{
  \code{gains}, \code{plot}.
}
\examples{
data(ciaScores)
\dontrun{plot(gains(actual=ciaScores$CellPhonesPP[ciaScores$train==0],
           predicted=ciaScores$PredOLS[ciaScores$train==0],
     main="Test Gains Table Plot")}
}
\keyword{misc}
