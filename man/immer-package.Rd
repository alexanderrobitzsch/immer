%% File Name: immer-package.Rd
%% File Version: 0.38
%% File Last Change: 2017-02-08 11:14:17

\name{immer-package}
\alias{immer-package}
\alias{immer}
\docType{package}
\title{
Item Response Models for Multiple Ratings
}
\description{
 Implements some item response models for multiple
 ratings, including the hierarchical rater model, 
 conditional maximum likelihood estimation of linear 
 logistic partial credit model and a wrapper function
 to the commercial FACETS program.
}
\details{

% URL: \tab \url{https://sites.google.com/site/alexanderrobitzsch/software} \cr

The \pkg{immer} package has following features: 

\itemize{   

%% Hierarchical rater model
\item Estimation of the hierarchical rater model (Patz et al., 2002) with 
\code{\link{immer_HRM}} and simulation of it with \code{\link{simulate_HRM}}.

%% conditional maximum likelihood estimation
\item The linear logistic partial credit model as an extension to the 
linear logistic test model (LLTM) for dichtomous data can be estimated
with \code{\link{immer_cml}}.

%% wrapper function
\item Wrapper function \code{\link{immer_FACETS}} to the commercial
program FACETS for analyzing multi-faceted Rasch models.

\item \dots

}

}
\author{
Alexander Robitzsch, Jan Steinfeld \cr 
\cr
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de> \cr 
}

%Federal Institute for Education Research, Innovation and Development of the 
%Austrian School System (BIFIE Salzburg), Austria


\references{
Patz, R. J., Junker, B. W., Johnson, M. S., & Mariano, L. T. (2002). 
The hierarchical rater model for rated test items and its 
application to large-scale educational assessment data. 
\emph{Journal of Educational and Behavioral Statistics}, 
\bold{27(4)}, 341-384.
}



\seealso{
For estimating the Rasch multi-facets model with marginal
maximum likelihood see also the
\code{\link[TAM:tam.mml.mfr]{TAM::tam.mml.mfr}} and
\code{\link[sirt:rm.facets]{sirt::rm.facets}} functions.

For estimating the hierarchical rater model based on signal
detection theory see \code{\link[sirt:rm.sdt]{sirt::rm.sdt}}.

For conditional maximum likelihood estimation of linear logistic
partial credit models see the \pkg{eRm} (e.g. \code{\link[eRm:LPCM]{eRm::LPCM}})
and the \pkg{psychotools} (e.g. \code{\link[psychotools:pcmodel]{psychotools::pcmodel}})
packages.
}


\examples{   
  ##  
  ##  
  ##  immer 0.0-16 (2015-06-27)
  ##  
  ##   
  ##      #@#################################################################@   
  ##     :#@##################################################################`  
  ##     #####################################################################+  
  ##     #####################################################################@  
  ##    .##########@###########################################################` 
  ##    +######@+;'@##########################################################@: 
  ##    ######'     +@########################################################## 
  ##    #####+ ##'  `##########################################################@ 
  ##    ####@ ####  +@#@##################################################@##### 
  ##   .#####.#####@######@#########@#@######@@####@##@############@######@#####`
  ##   :####+:###@:,@##@,;##@+@##@+'###@;.'##@##@##@''@#####@@'+@###,;##@;######.
  ##   ;####@ @##   ##@  ,;    :,    .#    ':    :.    ,@@#'     ';   ,+   @####.
  ##   '#####`:##   #@   : ''    ++   @@;  ` +;    +'   #@' `+@   #@  .    #####,
  ##   +######, #   : ,   @#@   +#@  `##,   @#@   @##  `#@  `##   @#`  +,  @####:
  ##   +#######@@  :+@`  ;##'  `##@  .@#   +##:  `###  .@.  @#+   #:  .##@@#####:
  ##   '########:  ##@   @#@,  ;##;  ;#@   @##`  +##,  +#       `@#`  ##########,
  ##   ;########   @##   ###`  @##`  @#+  `###   @##   ##   ,;'@##@   @#########,
  ##   :#######@  `##+  ,##@   @#@   @#,  '##@   ##@   @@  `#######  `##########.
  ##   .#######+  ,##.  @##+  `##@  .##`  @##;  ,##@  `##   ###; #;  :#@,+###### 
  ##    #######'    #   @##:  ,##;  ;##   @##`  +###   `@:     `;#.  @@:  ###### 
  ##    #######@,`,##.`,###,..###,  #@#..,##@,..####:` +#@'  ``#+#` `@#+ :#####@ 
  ##    #########################:   ,###################@#####@###@#@#########@ 
  ##    '########################@+;+##########################################: 
  ##    ,######################################################################` 
  ##     ##@##################################################################@  
}

\keyword{package}
