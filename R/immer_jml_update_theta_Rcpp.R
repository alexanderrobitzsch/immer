## File Name: immer_jml_update_theta_Rcpp.R
## File Version: 0.06

immer_jml_update_theta_Rcpp <- function(score_pers, I, K, N, theta, b, dat_resp, maxiter_update,
	conv_update, center_theta, max_incr )
{
	KM <- matrix( 0:K , nrow=N , ncol=K+1, byrow=TRUE )
	iterate <- TRUE
	eps <- 1E-7
	iter <- 0	
	# probs <- array( 0 , dim=c(N,I,K+1) )
	while(iterate){
		theta0 <- theta			
		res <- immer_jml_update_theta_derivatives( theta=theta, score_pers=score_pers, N=N, K=K, I=I, 
						b=b, max_incr=max_incr, dat_resp=dat_resp ) 
		theta <- res$theta
		der2 <- res$der2
		probs <- res$probs		
		theta <- immer_jml_center_theta( theta=theta, center_theta=center_theta )
		iter <- iter + 1
		theta_change <- max( abs(theta - theta0) )
		if (iter > maxiter_update){ iterate <- FALSE }
		if (theta_change < conv_update){ iterate <- FALSE }			
	}	
	#--- probs
	probs <- array( probs, dim=c(N,I,K+1))	
	#--- output
	res <- list(theta=theta, theta_der2=der2, probs=probs)	
	return(res)
}
