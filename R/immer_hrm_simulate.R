## File Name: immer_hrm_simulate.R
## File Version: 0.193

#############################################################
# simulating hierarchical rater model
immer_hrm_simulate <- function( theta, a, b, phi, psi )
{
    RR <- ncol(phi)
    I <- nrow(b)
    #*** simulate partial credit items
    K <- ncol(b)
    N <- length(theta)
    oneN <- rep(1,N)
    KM <- outer( oneN, 0:K )
    xiM <- matrix(0,nrow=N, ncol=I)
    for (ii in 1L:I){
        KM <- matrix( 0:K, nrow=N, ncol=K+1, byrow=TRUE)
        b0 <- c( 0, b[ii, 1L:K] )
        bM <- matrix( b0, nrow=N, ncol=K+1, byrow=TRUE)
        probs <- exp( a * KM *  theta - bM )
        probs <- probs / rowSums( probs )
        probs <- sirt::rowCumsums.sirt(probs)
        vals <- sirt::rowIntervalIndex.sirt( matr=probs, rn=stats::runif(N) )
        xiM[,ii] <- vals - 1
    }
    #*** simulate items for all raters
    items <- paste0('I', 1L:I)
    dat <- NULL
    for (rr in 1L:RR){
        dat.rr <- matrix( NA, nrow=N, ncol=I)
        colnames(dat.rr) <- items
        for (ii in 1L:I){
            # ii <- 1
            probs <- exp( - ( KM - ( xiM[,ii] + phi[ii,rr] ) )^2 / psi[ii,rr] / 2 )
            probs <- probs / rowSums(probs )
            probs <- sirt::rowCumsums.sirt(probs)
            vals <- sirt::rowIntervalIndex.sirt(matr=probs, rn=stats::runif(N))
            dat.rr[,ii] <- vals - 1
        }
        dat <- rbind( dat, dat.rr )
    }
    dat1 <- data.frame( pid=rep(1L:N, RR), rater=rep(1L:RR, each=N), dat )
    dat1 <- dat1[ order( dat1$pid ), ]
    rownames(dat1) <- NULL
    return(dat1)
}
