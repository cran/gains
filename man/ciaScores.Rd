\name{ciaScores}
\docType{data}
\alias{ciaScores}
\title{Cell Phones per Country with Predictions}
\description{
  This data set gives the number of cell phones per person for 194 countries, courtesy of the CIA World Factbook.  The data are mostly for 2008.  It also gives predicted values of this variable from 5 different methods (OLS, Lasso, Regression Tree, Random Forest, and Additive Model).  Finally, there is an indicator for each country indicating whether the country was used in the model development sample or not. 
}
\usage{cia.scores}
\format{a data frame containing 194 rows and 8 columns.
\itemize{
  \item CellPhonesPP: Number of cell phones per person, from the CIA Factbook.
  \item PredOLS: Predicted response from an OLS regression.
  \item PredLasso: Predicted response from a LASSO regression.
  \item PredTree: Predicted response from a regression tree.
  \item PredRF: Predicted response from a Random Forest.
  \item PredSM: Predicted response from an additive model.
  \item PredGLM: Predicted probability (from a logistic regression) that the country has more cell phones than people.
  \item train: Indicator, =1 if the country was among the set used to make the predictions, =0 if the country was in the validation set (not used to make predictions).
}
}
\source{CIA - The World Factbook
\url{https://www.cia.gov/library/publications/the-world-factbook/index.html}
}
\keyword{datasets}
