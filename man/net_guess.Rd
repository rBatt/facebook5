% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_guess.R
\name{net_guess}
\alias{net_guess}
\title{Make  a Guess Using a Neural Network}
\usage{
net_guess(train_set, test_set, form = place_id ~ x + y + time,
  use_map3 = FALSE, ...)
}
\arguments{
\item{train_set}{data.table of training data}

\item{test_set}{data.table of test data}

\item{form}{formula used by \code{\link{nnet}}}

\item{use_map3}{Logical, default is FALSE, mean average precision (at 3) is not used as metric during tuning}

\item{...}{other arguments to be passed}
}
\description{
Function that uses the neural network to make a guess at the test set
}
\details{
Just learning to use nnet
}

