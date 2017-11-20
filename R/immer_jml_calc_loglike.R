## File Name: immer_jml_calc_loglike.R
## File Version: 0.01

immer_jml_calc_loglike <- function( dat_score, probs, K)
{
	loglike <- 0
	eps <- 1E-20
	for (kk in 1:(K+1)){
		loglike <- loglike + sum( dat_score[,,kk] * log( probs[,,kk] + eps ) )
	}			
	return(loglike)	
}
