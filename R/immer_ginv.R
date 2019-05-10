## File Name: immer_ginv.R
## File Version: 0.02

immer_ginv <- function(x, ...)
{
    requireNamespace("MASS")
    y <- MASS::ginv(X=x, ...)
    return(y)
}
