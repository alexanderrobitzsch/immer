%% File Name: immer_proc_data.Rd
%% File Version: 0.03

\name{immer_proc_data}
\alias{immer_proc_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Processing Datasets with Rating Data
}

\description{
Processes datatsets containing rating data into a dataset into a long format of
pseudoitems (item \eqn{times} raters).
}

\usage{
immer_proc_data(dat, pid = NULL, rater = NULL, weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Datasets with integer item responses
}
  \item{pid}{
Vector with person identifiers
}
  \item{rater}{
Vector with rater identifiers
}
  \item{weights}{
Vector with sampling weights
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List with several entries (selection)

\item{dat2}{Dataset containing pseudoitems}
\item{dat2.resp}{Dataset containing response indicators for pseudoitems}
\item{dat2.NA}{Dataset containing pseudoitems and missing responses coded as \code{NA}}
\item{dat}{Original dataset}
\item{person.index}{Person identifiers}
\item{rater.index}{Rater identifiers}
\item{VV}{Number of items}
\item{N}{Number of persons}
\item{RR}{Number of raters}
\item{dat2.ind.resp}{Array containing indicators of pseudoitems and categories}
\item{ND}{Number of person-rater inrteractions}
}


%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: Processing rating data
#############################################################################

data(data.immer01a, package="immer")
dat <- data.immer01a

res <- immer::immer_proc_data( dat=dat[,paste0("k",1:5)], pid=dat$idstud, rater=dat$rater)
str(res, max.level=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
