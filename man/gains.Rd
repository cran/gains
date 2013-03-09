\name{gains}
\alias{gains}
\title{Gains Table for a Predictive Model}
\description{
  Takes a vector of actual responses and a vector of predictions and constructs a gains table to evaluate the predictions.
}
\usage{
gains(actual, predicted, groups=10, ties.method=c("max","min","first","average","random"),
      conf=c("none","normal","t","boot"), boot.reps=1000, conf.level=0.95, optimal=FALSE,percents=FALSE)
}
\arguments{
  \item{actual}{a numeric vector of actual response values}
  \item{predicted}{a numeric vector of predicted response values.  This vector must have the same length as \code{actual}, and the ith value of this vector needs to be the model score for the subject with the ith value of the \code{actual} vector as its actual response.}
  \item{groups}{an integer containing the number of rows in the gains table.  The default value is 10.}
  \item{ties.method}{method of breaking ties.  See the \code{ties.method} argument of the \code{rank} procedure.}
  \item{conf}{method to construct confidence intervals for the mean response in each row of the table.  If "none", then no confidence intervals are constructed.  If "normal", then critical values from the normal distribution are used.  If "t", then critical values from the t distribution are used.  If "boot", then 1000 bootstrap samples are drawn from each row, and the upper and lower \code{conf.level}/2 values of the distribution are used as the confidence interval.}
  \item{boot.reps}{the number of bootstrap replications to use for bootstrap confidence intervals.  The default value is 1000.}
  \item{conf.level}{the 1 - alpha level of the confidence interval.  The default value is 0.95.}
  \item{optimal}{a logical indicated whether the user wants optimal lift indices to be computed.  Optimal lift indices represent the results that would be achieved if the input vectors \code{actual} and \code{predict} had exactly the same sort order.}
  \item{percents}{a logical that indicates whether to print the mean responses and predicted responses in percent form.}
}
\value{
  \code{gains} returns an S3 object of class \code{gains}.  The function \code{print.gains} can be used to print the results.  The function \code{plot.gains} can be used to plot the mean response and cumulative mean response for each group.
  An object of class \code{gains} is a list containing at least the following components:
  \item{depth}{cumulative percentage of file covered by each row (e.g. 10,20,30,...,100).}
  \item{obs}{number of observations in each row.}
  \item{cume.obs}{cumulative number of observations by row.}
  \item{mean.resp}{sample mean response in each row.}
  \item{cume.mean.resp}{cumulative mean response by row.}
  \item{cume.pct.of.total}{cumulative percent of total response.}
  \item{lift}{lift index.  The lift index is 100 times the \code{mean.resp} for the row divided by the \code{cume.mean.resp} for the last row.}
  \item{cume.lift}{cumulative lift index.  It is 100 times the \code{cume.mean.resp} for the row divided by the \code{cume.mean.resp} for the last row.}
  \item{mean.prediction}{mean predicted response for the group.}
  \item{min.prediction}{minimum predicted response for the group.  \code{min.prediction} and \code{max.prediction} can be used to construct decision rules for applying the model.}
  \item{max.prediction}{maximum predicted response for the group.}
  \item{conf}{the argument given for \code{conf}.}
  \item{optimal}{the argument given for \code{optimal}.}
  \item{num.groups}{the number of rows of the gains table.  This will equal \code{groups} unless there are fewer distinct predicted values than \code{groups}.}
  \item{percents}{the argument given for \code{percents}.}
}
\seealso{
  \code{print.gains} for printing the table in a nice way.
  \code{plot.gains} for drawing a graph representing the output.
}
\examples{
data(ciaScores)
gains(actual=ciaScores$CellPhonesPP[ciaScores$train==0],
      predicted=ciaScores$PredOLS[ciaScores$train==0],
      optimal=TRUE)
}
\keyword{misc}
