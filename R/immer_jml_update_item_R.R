## File Name: immer_jml_update_item_R.R
## File Version: 0.15


immer_jml_update_item_R <- function( score_items, I, K, b, A, xsi, theta, N, dat_resp, max_incr, 
	maxiter_update, conv_update, b_fixed )
{
	iterate <- TRUE
	iter <- 0
	eps <- 1E-7
	der1_b <- matrix( 0 , nrow=I, ncol=K)
	der2_b <- matrix( 0 , nrow=I, ncol=K)
	NX <- length(xsi)
	while(iterate){
		b0 <- b
		for (ii in 1:I){
			sc_ii <- score_items[ii,]
			b_ii <- c(0, b[ii,] )
			probs_ii <- probs_pcm_one_item(theta=theta, b_ii=b_ii)
			der1_b[ii,] <- - sc_ii + colSums(probs_ii * dat_resp[,ii] )[-1]
			der2_b[ii,] <- colSums( probs_ii*(1-probs_ii)* dat_resp[,ii] )[-1]			
		}
		der1_xsi <- rep(0,NX)
		der2_xsi <- rep(0,NX)
		for (kk in 1:K){
			der1_xsi <- der1_xsi + colSums( der1_b[,kk] * A[,kk,] )
			der2_xsi <- der2_xsi + colSums( der2_b[,kk] * A[,kk,] )
		}
		incr <- der1_xsi / ( abs(der2_xsi) + eps )
		incr <- immer_trim_increment(incr=incr, max_incr=max_incr)
		xsi <- xsi + incr
		b <- b_fixed
		for (kk in 1:K){
			b[,kk] <- b[,kk] + A[,kk,] %*% xsi
		}
		iter <- iter + 1
		b_change <- max( abs( b - b0) )
		if (iter > maxiter_update){ iterate <- FALSE }
		if (b_change < conv_update){ iterate <- FALSE }
	}
	#--- output
	res <- list(b=b, xsi=xsi, xsi_der2 = der2_xsi )
	return(res)
}
 
    
	
