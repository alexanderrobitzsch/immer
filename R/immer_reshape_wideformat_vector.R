## File Name: immer_reshape_wideformat_vector.R
## File Version: 0.02


immer_reshape_wideformat_vector <- function(y, pid,rater, Nmin_ratings )
{
    rater <- paste(rater)
    Nobs <- rowsum( 1 - is.na(y),  pid  )
    Nobs <- Nobs[ Nobs >=Nmin_ratings, ]
    persons <- names(Nobs)
    NP <- length(persons)
    data <- data.frame( 'pid'=pid, 'rater'=rater, 'y'=y )
    data <- data[ data$pid %in% persons, ]
    raters <- sort( unique( paste(data$rater )))
    RR <- length(raters)
    y <- matrix( NA, nrow=NP, ncol=RR+1 )
    y <- as.data.frame(y)
    colnames(y) <- c('pid', raters )
    y$pid <- persons
    indM <- cbind( match( paste(data$pid), persons),
                match( paste(data$rater), raters)+1 )
    y[ indM ] <- data$y
    return(y)
}
