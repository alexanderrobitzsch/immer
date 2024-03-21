## File Name: immer_ginv.R
## File Version: 0.03

immer_ginv <- function(x, ...)
{
    requireNamespace('MASS')
    y <- MASS::ginv(X=x, ...)
    return(y)
}
