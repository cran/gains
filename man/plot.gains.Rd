\name{plot.gains}
\alias{plot.gains}
\title{Plotting Gains Table Objects}
\description{
  Plot method for objects of class \code{gains}. These plots are sometimes called lift charts.
}
\usage{
\method{plot}{gains}(x, y=NULL, xlab="Depth of File", ylab="Mean Response", 
    type="b", col=c("red3","bisque4","blue4"), pch=c(1,1,1), lty=c(1,1,1),
    legend=c(
    "Mean Response","Cumulative Mean Response","Mean Predicted Response"),
    ylim=c(min(c(x$mean.resp,x$mean.prediction)),
    max(c(x$mean.resp,x$mean.prediction))), main="Gains Table Plot", ...)
}
\arguments{
  \item{x}{an object of class \code{gains}.}
  \item{y}{included for compatability with the \code{plot} generic but is not used.}
  \item{xlab}{a title for the x axis.  See \code{title}.}
  \item{ylab}{a title for the y axis.  See \code{title}.}
  \item{type}{what type of plot should be drawn.  The default is "b" for points and lines.}
  \item{col}{vector of length 3 specifying the colors for the series of mean response rates, cumulative mean response rates, and mean predicted response rates, respectively.}
  \item{pch}{vector of length 3 specifying the plotting characters for the series of mean response rates, cumulative mean response rates, and mean predicted response rates, respectively.}
  \item{lty}{vector of length 3 specifying the line types for the series of mean response rates, cumulative mean response rates, and mean predicted response rates, respectively.}
  \item{legend}{character or expression vector of length 3 specifying the legend descriptions for the series of mean response rates, cumulative mean response rates, and mean predicted response rates, respectively.}
  \item{ylim}{the y limits of the plot.  See \code{plot.window}.}
  \item{main}{an overall title for the plot.  See \code{title}.}
  \item{\dots}{additional arguments to \code{plot}}.
}
\seealso{
  \code{gains}, \code{plot}.
}
\examples{
data(ciaScores)
\dontrun{plot(with(subset(ciaScores,train==0), 
      gains(actual=CellPhonesPP, predicted=PredOLS, optimal=TRUE)),
      main="Test Gains Table Plot")}
}
\keyword{misc}
