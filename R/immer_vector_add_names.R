## File Name: immer_vector_add_names.R
## File Version: 0.04

immer_vector_add_names <- function(vec, pre)
{
    n1 <- paste0(pre, 1L:length(vec) )
    names(vec) <- n1
    return(vec)
}
