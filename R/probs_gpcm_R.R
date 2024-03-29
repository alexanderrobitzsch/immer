## File Name: probs_gpcm_R.R
## File Version: 0.161


#*****************
# R version
probs_gpcm_R <- function( x, theta, b, a, K, x_ind=NULL )
{
    N <- length(theta)
    KM <- matrix( 0:K, nrow=N, ncol=K+1, byrow=TRUE)
    b0 <- c( 0, b[1L:K] )
    bM <- matrix( b0, nrow=N, ncol=K+1, byrow=TRUE)
    probs <- exp( a * KM *  theta - bM )
    probs <- probs / rowSums(probs, na.rm=TRUE)
    if ( ! is.null(x) ){
        ind <- cbind( 1L:N, x+1)
        probs <- probs[ ind ]
    }
    if ( ! is.null( x_ind) ){
        probs <- ifelse( x_ind==0, 1, probs )
    }
    return(probs)
}
