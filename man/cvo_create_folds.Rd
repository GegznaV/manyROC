% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvo_create_folds.R
\name{cvo_create_folds}
\alias{cvo_create_folds}
\alias{print.cvo}
\title{Create a cvo (cross-valitation object)}
\usage{
cvo_create_folds(
  data = NULL,
  stratify_by = NULL,
  block_by = NULL,
  folds = 5,
  times = 1,
  seeds = NA_real_,
  kind = NULL,
  mode = c("caret", "mlr")[1],
  returnTrain = c(TRUE, FALSE, "both")[1],
  predict = c("test", "train", "both")[1],
  k = folds
)

\method{print}{cvo}(x, ...)
}
\arguments{
\item{data}{A data frame, that contains variables which names are denoted
by arguments \code{block_by} and by \code{stratify_by}.}

\item{stratify_by}{A vector or a name of factor variable in \code{data},
which levels will be used for \emph{stratification}. E.g.,
a vector with medical groups.}

\item{block_by}{A vector or a name of variable in \code{data}, that
contains identification codes/numbers (ID). These codes
will be used for blocking.}

\item{folds, k}{(\code{integer})\cr A number of folds, default \code{folds = 5}.}

\item{times}{(\code{integer})\cr
A number of repetitions for repeated cross-validation.}

\item{seeds}{(\code{NA_real_} | \code{NULL} | vector of integers)\cr
             Seeds for random number generator for each repetition.
  \itemize{
      \item If \code{seeds = NA_real_} (default), no seeds are set,
             parameter \code{kind} is also ignored.

      \item If \code{seeds = NULL} random seeds are generated
            automatically and registered in attribute \code{"seeds"}.

      \item If numeric vector, then these seeds will be used for each
            repetition of cross-validation.
            If the number of repetitions is greater than the number of
            provided seeds, additional seeds are generated and added to
            the vector. The first seed will be used to ensure
            reproducibility of the randomly generated seeds.
            }

             For more information about random number generation see
             \code{\link[base]{set.seed}}.}

\item{kind}{(\code{NULL} | \code{character})\cr
            The kind of (pseudo)random number generator. Default is
            \code{NULL}, which selects the currently-used generator
            (including that used in the previous session if the
            workspace has been restored): if no generator has been
            used it selects \code{"default"}.\cr

            Generator \code{"L'Ecuyer-CMRG"} is recommended if package
            \pkg{parallel} is used for for parallel computing.
            In this case each seed should have 6 elements neither the first
            three nor the last three should be all zero.
            More information at \code{\link[base]{set.seed}}.}

\item{mode}{(\code{character})\cr Either \pkg{caret}-like or \pkg{mlr}-like
cvo object. \bold{This option is not implemented yet!}}

\item{returnTrain}{(\code{logical} | \code{character}) \cr
If \code{TRUE}, returns indices of variables in
a training set (\pkg{caret} style).
If \code{FALSE}, returns indices of variables in
a test set (\pkg{caret} style).
If \code{"both"}, returns indices of variables
in both training and test sets (\pkg{mlr} style).}

\item{predict}{(\code{character(1)})\cr
What to predict during resampling: \dQuote{train}, \dQuote{test} or \dQuote{both} sets.
Default is \dQuote{test}.}

\item{x}{A \code{cvo} object.}

\item{...}{(any)\cr
Further parameters for strategies.\cr
\describe{
\item{iters (\code{integer(1)})}{Number of iterations, for \dQuote{CV}, \dQuote{Subsample}
and \dQuote{Bootstrap}.}
\item{split (\code{numeric(1)})}{Proportion of training cases for \dQuote{Holdout} and
\dQuote{Subsample} between 0 and 1. Default is 2 / 3.}
\item{reps (\code{integer(1)})}{Repeats for \dQuote{RepCV}. Here \code{iters = folds * reps}.
Default is 10.}
\item{folds (\code{integer(1)})}{Folds in the repeated CV for \code{RepCV}.
Here \code{iters = folds * reps}. Default is 10.}
\item{horizon (\code{numeric(1)})}{Number of observations in the forecast test set for \dQuote{GrowingWindowCV}
and \dQuote{FixedWindowCV}. When \code{horizon > 1} this will be treated as the number of
observations to forecast, else it will be a fraction of the initial window. IE,
for 100 observations, initial window of .5, and horizon of .2, the test set will have
10 observations. Default is 1.}
\item{initial.window (\code{numeric(1)})}{Fraction of observations to start with
in the training set for \dQuote{GrowingWindowCV} and \dQuote{FixedWindowCV}.
When \code{initial.window > 1} this will be treated as the number of
observations in the initial window, else it will be treated as the fraction
of observations to have in the initial window. Default is 0.5.}
\item{skip (\code{numeric(1)})}{ How many resamples to skip to thin the total amount
for \dQuote{GrowingWindowCV} and \dQuote{FixedWindowCV}. This is passed through as the \dQuote{by} argument
in \code{seq()}. When \code{skip > 1} this will be treated as the increment of the sequence of resampling indices,
else it will be a fraction of the total training indices. IE for 100 training sets and a value of .2, the increment
of the resampling indices will be 20. Default is \dQuote{horizon} which gives mutually exclusive chunks
of test indices.}
}}
}
\value{
(\code{list}) A list of folds. In each fold there are indices
        observations. The structure of outputs is the similar to one
        created with either function \code{\link[caret]{createFolds}}
        from \pkg{caret} or function
        \code{\link[mlr]{makeResampleInstance}} in \pkg{mlr}.
}
\description{
Create indices of folds with blocking and stratification (cvo object)
Create a cross-validation object (cvo), which contain a list of indices
for each fold of (repeated) k-fold cross-validation.
Options of blocking and stratification are available. See more in "Details".
}
\details{
Function \code{cvo_create_folds} randomly divides observations into
folds that are used for (repeated) k-fold cross-validation. In these
folds observations are:
\enumerate{
 \item \bold{blocked} by values in variable \code{block_by}
     (i.e. observations with the same "ID" or other kind of blocking factor
     are treated as one unit (a block) and are always in the same fold);
 \item \bold{stratified} by levels of factor variable \code{stratify_by}
      (the proportions of these grouped units of observations per each
      group (level) are kept approximately constant throughout all folds).
 }
}
\note{
If \code{folds} is too big and cases of at least one group (i.e.,
      level in \code{stratify_by}) are not included in at least one fold,
      an error is returned. In that case smaller value of \code{folds} is
      recommended.
}
\examples{
library(manyROC)
set.seed(123456)

# Data
DataSet1 <- data.frame(ID = rep(1:20, each = 2),
  gr = gl(4, 10, labels = LETTERS[1:4]),
  .row = 1:40)

# Explore data
str(DataSet1)

table(DataSet1[, c("gr", "ID")])

summary(DataSet1)


# Explore functions
nFolds <- 5

# If variables of data frame are provided:
Folds1_a <- cvo_create_folds(data = DataSet1,
  stratify_by = "gr", block_by = "ID",
  k = nFolds, returnTrain = FALSE)
Folds1_a

str(Folds1_a)

cvo_test_bs(Folds1_a, "gr", "ID", DataSet1)

# If "free" variables are provided:
Folds1_b <- cvo_create_folds(stratify_by = DataSet1$gr,
  block_by = DataSet1$ID,
  k = nFolds,
  returnTrain = FALSE)
# str(Folds1_b)
cvo_test_bs(Folds1_b, "gr", "ID", DataSet1)

# Not blocked but stratified
Folds1_c <- cvo_create_folds(stratify_by = DataSet1$gr,
  k = nFolds,
  returnTrain = FALSE)
# str(Folds1_c)
cvo_test_bs(Folds1_c, "gr", "ID", DataSet1)

# Blocked but not stratified
Folds1_d <- cvo_create_folds(block_by = DataSet1$ID,
  k = nFolds,
  returnTrain = FALSE)
# str(Folds1_d)
cvo_test_bs(Folds1_d, "gr", "ID", DataSet1)
}
\seealso{
Function \code{\link[caret]{createFolds}} from package
         \pkg{caret}. \cr
         Function \code{\link[mlr]{makeResampleInstance}} from package
         \pkg{mlr}. \cr
Test if folds are blocked and stratified \code{\link{cvo_test_bs}}
}
\author{
Vilmantas Gegzna
}
