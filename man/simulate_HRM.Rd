%% File Name: simulate_hrm.Rd
%% File Version: 0.09

\name{simulate_hrm}
\alias{simulate_hrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulating the Hierarchical Rater Model (Patz et al., 2002)
}
\description{
Simulates the hierarchical rater model (Patz et al., 2002).
}
\usage{
simulate_hrm(theta, a, b, phi, psi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Vector of \eqn{\theta} parameters
}
  \item{a}{
Vector of \eqn{a} parameters
}
  \item{b}{
Matrix of \eqn{b} parameters
}
  \item{phi}{
Matrix of \eqn{\phi} parameters
}
  \item{psi}{
Matrix of \eqn{\psi} parameters
}
}

\details{
See \code{\link{immer_hrm}} for more details of the hierarchical rater model.
}

\value{
Dataset with simulated item responses as well as vectors of person
and rater identifiers
}

\references{
Patz, R. J., Junker, B. W., Johnson, M. S., & Mariano, L. T. (2002). 
The hierarchical rater model for rated test items and its 
application to large-scale educational assessment data. 
\emph{Journal of Educational and Behavioral Statistics}, 
\bold{27(4)}, 341-384.
}

\author{
Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For examples see Example 1 in \code{\link{immer_hrm}}.
}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
