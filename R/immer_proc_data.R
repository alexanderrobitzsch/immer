## File Name: immer_proc_data.R
## File Version: 0.04

immer_proc_data <- function( dat, pid=NULL, rater=NULL, weights=NULL)
{
	#-- indicator variable for providing rating data
	is_rating_data <- ! is.null(rater)
	#-- include person IDs if not provided
	N1 <- nrow(dat)
	I <- ncol(dat)
	if (is.null(pid)){
		pid <- 1:N1
	}	
	if ( is.null(rater) ){
		rater <- rep(0,N1)
	}	
	#-- apply sirt::rm_proc_data() function for processing rating data
	res <- sirt::rm_proc_data( dat=dat, pid=pid, rater=rater, rater_item_int=FALSE, reference_rater=NULL )
	N <- res$N
	#-- create weights if not provided		
	if ( ! is.null(weights) ){
		a1 <- stats::aggregate( weights, list(pid), mean )
		weights <- a1[,2]
	}	
	if ( is.null(weights) ){
		weights <- rep(1,N)
	}		
	res$weights <- weights
	res$W <- sum(weights)
	res$ND <- nrow(res$dat)
	res$I <- I
	res$dataproc.vars$pseudoitems <- colnames(res$dat2)
	res$is_rating_data <- is_rating_data
	if ( ! is_rating_data ){
		res$ND <- NA
		res$RR <- NA
		res$rater <- NULL
	}
	#--- output
	return(res)
}
