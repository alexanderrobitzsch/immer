%% File Name: data.ptam.Rd
%% File Version: 0.12

\name{data.ptam}

\alias{data.ptam1}
\alias{data.ptam2}
\alias{data.ptam3}
\alias{data.ptam4}


\docType{data}


\title{
Example Datasets for Robitzsch and Steinfeld (2018, PTAM)
}


\description{
Example datasets for Robitzsch and Steinfeld (2018, PTAM).
}


\usage{
data(data.ptam1)
data(data.ptam2)
data(data.ptam3)
data(data.ptam4)
}


\format{

\itemize{	
	
\item The dataset \code{data.ptam1} is a subset of the dataset from Example 3 
of the ConQuest manual and
contains 9395 ratings for 6877 students and 9 raters 
on 2 items (\code{OP} and \code{TF}). The format is 

\code{'data.frame':   9395 obs. of  4 variables:} \cr
\code{ $ pid  : int  1508 1564 1565 1566 1567 1568 1569 1629 1630 1631 ...} \cr
\code{ $ rater: num  174 124 124 124 124 124 124 114 114 114 ...} \cr
\code{ $ OP   : int  2 1 2 1 1 1 2 2 2 3 ...} \cr
\code{ $ TF   : int  3 1 2 2 1 1 2 2 2 3 ...} \cr


\item The dataset \code{data.ptam2} contains 1043 ratings for 
262 students and 17 raters on 19 items (\code{A1}, ..., \code{D9}). The format is 

\code{'data.frame':   1043 obs. of  21 variables:} \cr
\code{ $ idstud : int  1001 1001 1001 1001 1002 1002 1002 1002 1003 1003 ...} \cr
\code{ $ idrater: int  101 108 212 215 104 108 209 211 103 104 ...} \cr
\code{ $ A1     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A2     : int  1 1 1 1 0 0 0 1 1 1 ...} \cr
\code{ $ A3     : int  1 1 1 1 1 1 0 1 0 0 ...} \cr
\code{ [...]} \cr
\code{ $ D9     : int  2 2 2 2 2 2 2 2 1 0 ...} \cr


\item The dataset \code{data.ptam3} contains 523 ratings for 
262 students and 8 raters on 23 items (\code{A1}, ..., \code{J0}). The format is 

\code{'data.frame':   523 obs. of  25 variables:} \cr
\code{ $ idstud : int  1001 1001 1002 1002 1003 1003 1004 1004 1005 1005 ...} \cr
\code{ $ idrater: int  101 108 104 108 103 104 102 104 102 108 ...} \cr
\code{ $ A1     : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A2     : int  1 1 0 0 1 1 NA 0 1 1 ...} \cr
\code{ $ A3     : int  1 1 1 1 0 0 0 0 0 0 ...} \cr
\code{ [...]} \cr
\code{ $ J0     : int  2 3 3 2 0 0 2 2 0 1 ...} \cr

\item The dataset \code{data.ptam4} contains 592 ratings for 
209 students and 10 raters on 3 items (\code{crit2}, \code{crit3} and
\code{crit4}). The format is

\code{'data.frame':   592 obs. of  5 variables:} \cr
\code{ $ idstud: num  10005 10009 10010 10010 10014 ...} \cr
\code{ $ rater : num  802 802 844 802 837 824 820 803 816 844 ...} \cr
\code{ $ crit2 : int  3 2 0 2 1 0 2 1 1 0 ...} \cr
\code{ $ crit3 : int  3 2 1 2 2 2 2 2 2 2 ...} \cr
\code{ $ crit4 : int  2 1 2 1 2 2 2 2 2 2 ...} \cr

}
}





%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


%\seealso{
%Some free datasets can be obtained from \cr
%}

%\source{
%  ~~ reference to a publication or URL from which the data were obtained ~~
%}



\references{
Robitzsch, A., & Steinfeld, J. (2018). XXX
}


%\examples{
%# xx
% }


\keyword{datasets}
