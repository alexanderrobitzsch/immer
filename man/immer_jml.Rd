%% File Name: immer_jml.Rd
%% File Version: 0.19

\name{immer_jml}
\alias{immer_jml}
\alias{summary.immer_jml}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Joint Maximum Likelihood Estimation for the Partial Credit Model 
with a Design Matrix for Item Parameters 
and \eqn{\varepsilon}-Adjustment Bias Correction
}


\description{
Estimates the partial credit model with a design matrix for item
parameters with joint maximum likelihood (JML). The  \eqn{\varepsilon}-adjustment 
bias correction is implemented with reduces bias of the
JML estimation method (Bertoli-Barsotti, Lando & Punzo, 2014).
}


\usage{
immer_jml(dat, A = NULL, center_theta = TRUE, b_fixed = NULL, irtmodel="PCM", eps = 0.3, 
     eps_adjust_pers = TRUE, eps_adjust_item = TRUE, maxiter = 1000, conv = 1e-06, 
     max_incr = 0.5, maxiter_update = 10, conv_update = 1e-05, verbose = TRUE, 
     use_Rcpp = TRUE)
          
\method{summary}{immer_jml}(object, digits=3,...)     
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Data frame with polytomous item responses \eqn{0,1,\ldots, K}
}
  \item{A}{
Design matrix (items \eqn{\times} categories \eqn{\times} basis parameters).
Entries for categories are for \eqn{1,\ldots,K} 
}
  \item{center_theta}{
Logical indicating whether the trait estimates should be centered
}
  \item{b_fixed}{
Matrix with fixed \eqn{b} parameters
}
\item{irtmodel}{Specified item response model. Can be one of the two
partial credit model parametrizations \code{PCM} and \code{PCM2}.
}
  \item{eps}{
Adjustment parameter \eqn{\varepsilon}
}
  \item{eps_adjust_pers}{
Logical indicating whether \eqn{\varepsilon}-adjustment should be
performed for persons
}
  \item{eps_adjust_item}{
Logical indicating whether \eqn{\varepsilon}-adjustment should be
performed for items
}
  \item{maxiter}{
Maximum number of iterations
}
  \item{conv}{
Convergence criterion
}
  \item{max_incr}{
Maximum increment
}
  \item{maxiter_update}{
Maximum number of iterations for parameter updates
}
  \item{conv_update}{
Convergence criterion for updates
}
  \item{verbose}{
Logical indicating whether convergence progress should be displayed
}
  \item{use_Rcpp}{
Logical indicating whether \pkg{Rcpp} package should be used
for computation.
}
\item{object}{Object of class \code{immer_jml}}
\item{digits}{Number of digits after decimal to print}
\item{\dots}{Further arguments to be passed.}
}




\details{
The function uses the partial credit model as 
\eqn{P(X_i = h | \theta ) \propto \exp( h \theta - b_{ih} )} with
\eqn{b_{ih} = \sum_l a_{ihl} \xi_l} where the values \eqn{a_{ihl}}
are included in the design matrix \code{A} and \eqn{\xi_l} denotes
basis item parameters.

The adjustment parameter \eqn{\varepsilon} is applied to the sum score
as the sufficient statistic for the person parameter. In more detail,
extreme scores \eqn{S_p=0} (minimum score) or \eqn{S_p=M_p} (maximum score)
are adjusted to \eqn{S_p^\ast = \varepsilon} or \eqn{S_p^\ast = M_p - \varepsilon},
respectively. Therefore, the adjustment possesses more influence on
parameter estimation for datasets with a small number of items.
}

\value{
List with following entries

\item{b}{Item parameters \eqn{b_{ih}}}
\item{theta}{Person parameters}
\item{theta_se}{Standard errors for person parameters}
\item{xsi}{Basis parameters}
\item{xsi_se}{Standard errors for bias parameters}
\item{probs}{Predicted item response probabilities}
\item{person}{Data frame with person scores}
\item{dat_score}{Scoring matrix}
\item{score_pers}{Sufficient statistics for persons}
\item{score_items}{Sufficient statistics for items}
\item{loglike}{Log-likelihood value}

}

\references{
Bertoli-Barsotti, L., Lando, T., & Punzo, A. (2014). Estimating a Rasch Model via 
fuzzy empirical probability functions. In D. Vicari, A. Okada, G. Ragozini & 
C. Weihs (Eds.). 
\emph{Analysis and Modeling of Complex Data in Behavioural and Social Sciences}, 
Springer.
}

%\author{
%%  ~~who you are~~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[TAM:tam.jml]{TAM::tam.jml}} for
joint maximum likelihood estimation. The \eqn{varepsilon}-adjustment
is also implemented in \code{\link[sirt:mle.pcm.group]{sirt::mle.pcm.group}}.
}


\examples{
#############################################################################
# EXAMPLE 1: Rasch model
#############################################################################

data(data.read, package="sirt")
dat <- data.read

#- estimate Rasch model with JML
mod1 <- immer::immer_jml(dat)
summary(mod1)

\dontrun{	
#- estimate LLTM by defining a design matrix for item difficulties
A <- array(0 , dim=c(12,1,3) )
A[1:4,1,1] <- 1
A[5:8,1,2] <- 1
A[9:12,1,3] <- 1

mod2 <- immer::immer_jml(dat, A=A)
summary(mod2)

#############################################################################
# EXAMPLE 2: Partial credit model
#############################################################################

library(TAM)
data(data.gpcm, package="TAM")
dat <- data.gpcm

#-- JML estimation in TAM
mod0 <- TAM::tam.jml(resp=dat, bias=FALSE)
summary(mod0)

# extract design matrix
A <- mod0$A
A <- A[,-1,]

#-- JML estimation without bias correction
mod1 <- immer::immer_jml(dat, A=A, eps_adjust_pers=FALSE)
summary(mod1)

#-- JML estimation with bias correction
mod2 <- immer::immer_jml(dat, A=A, eps_adjust_pers=TRUE)
summary(mod2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
