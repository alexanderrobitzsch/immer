## File Name: immer_jml_update_item_Rcpp.R
## File Version: 0.11


immer_jml_update_item_Rcpp <- function( score_items, I, K, b, A, xsi, theta, N, dat_resp, max_incr, 
	maxiter_update, conv_update, b_fixed )
{
	iterate <- TRUE
	iter <- 0
	eps <- 1E-20
	der1_b <- matrix( 0 , nrow=I, ncol=K)
	der2_b <- matrix( 0 , nrow=I, ncol=K)
	NX <- length(xsi)	
	A_ <- as.vector(A)
	
	xsi_converged <- rep(0,NX)
	
	
	while(iterate){
		b0 <- b		
		xsi0 <- xsi
		#-- derivatives with respect to b and xsi
		res <- immer_jml_update_item_derivatives( theta=theta, score_items=score_items, N=N, K=K, I=I, 
					dat_resp=dat_resp, b=b, A_=A_, xsi=xsi, max_incr=max_incr, b_fixed=b_fixed) 
		der1_b <- res$der1_b
		der2_b <- res$der2_b
		der1_xsi <- res$der1_xsi
		der2_xsi <- res$der2_xsi
		b <- res$b
		xsi <- res$xsi		
		xsi_converged <- ( abs( xsi - xsi0 ) < conv_update )		
		iter <- iter + 1
		b_change <- max( abs( b - b0) )
		if (iter > maxiter_update){ iterate <- FALSE }
		if (b_change < conv_update){ iterate <- FALSE }
	}
	#--- output
	res <- list(b=b, xsi=xsi, xsi_der2 = der2_xsi )
	return(res)
}
 
    
	
