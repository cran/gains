\name{MineThatData}
\docType{data}
\alias{MineThatData}
\title{MineThatData E-Mail Analytics Challenge Data With Predictions}
\description{
  This data set contains information about purchases from an apparel company during a two-week response window. It is based on a dataset used for an analytics challenge on the MineThatData blog in 2008.  Predictions from two different binary response models and two different spend models, conditional on response, are included.  Finally, there is an indicator for each customer indicating whether the customer was used in the training sample for the models. 
}
\usage{MineThatData}
\format{a data frame containing 64000 rows and 7 columns.
\itemize{
  \item conversion: 0/1 indicator of whether the customer purchased merchandise in the two-week response window.
  \item spend: Amount spent in dollars during the two-week response window.
  \item train: 0/1 indicator of whether the observation was used to construct the predictive models.
  \item logistic.score: Estimated response probability from a logistic regression.
  \item svm.score: Estimated response probability from a support vector machine.
  \item linear.score: Estimated revenue ("spend"), conditional on purchase, from the linear regression.
  \item rf.score: Estimated "spend", conditional on purchase, from the random forest.
}
}
\source{The MineThatData E-Mail Analytics and Data Mining Challenge
\url{http://blog.minethatdata.com/2008/03/minethatdata-e-mail-analytics-and-data.html}
}
\keyword{datasets}
