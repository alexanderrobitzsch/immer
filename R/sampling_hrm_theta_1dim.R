## File Name: sampling_hrm_theta_1dim.R
## File Version: 0.172

################################################
# simulation theta values
sampling_hrm_theta_1dim <- function( theta, N, I, maxK, a, b, xi, xi_ind,
        dat, dat_ind, pid, MHprop, mu_theta, SD_theta, useRcpp, eps=1E-20 )
{

    # refresh count
    MHprop$refresh_count$theta <- MHprop$refresh_count$theta + 1

    theta_new <- theta_old <- theta
    theta_new <- stats::rnorm(N, mean=theta, sd=MHprop$SD$theta )

    p_new <- log( stats::dnorm( theta_new, mean=mu_theta, sd=SD_theta ) )
    p_old <- log( stats::dnorm( theta_old, mean=mu_theta, sd=SD_theta ) )

    for (ii in 1L:I){
        ll_new <- probs_gpcm( x=xi[,ii], theta=theta_new, b=b[ii,], a=a[ii], K=maxK[ii],
                    x_ind=xi_ind[,ii], useRcpp )
        ll_new <- log( ll_new  + eps )
        ll_old <- probs_gpcm( x=xi[,ii], theta=theta_old, b=b[ii,], a=a[ii], K=maxK[ii],
                    x_ind=xi_ind[,ii], useRcpp )
        ll_old <- log( ll_old  + eps )
        p_new <- p_new + ll_new
        p_old <- p_old + ll_old
    }

    p_diff <- p_new - p_old
    max_diff <- 200
    p_diff <- ifelse( p_diff > max_diff, max_diff, p_diff )
    ratio <- exp( p_diff )
    zuf <- stats::runif(N)
    theta <- ifelse( ratio > zuf, theta_new, theta_old )
    MHprop$accept$theta <- MHprop$accept$theta + 1*( ratio > zuf )
    res <- list( theta=theta, MHprop=MHprop )
    return(res)
}
###################################################################



