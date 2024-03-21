## File Name: MHprop_refresh_parstype.R
## File Version: 0.09

MHprop_refresh_parstype <- function( accept, SD, MHprop, SDchange )
{
    #--- vector
    if ( is.vector(accept) ){
        SD <- MHprop_refresh_pars( acc=accept, SD.pp=SD, MHprop=MHprop,
                            SDchange=SDchange )
    }
    #--- matrix
    if ( is.matrix(accept) ){
        NP <- ncol(accept)
        for (pp in 1L:NP){
            SD[,pp] <- MHprop_refresh_pars( acc=accept[,pp], SD.pp=SD[,pp],
                            MHprop=MHprop,    SDchange=SDchange)
        }
    }
    #--- output
    return(SD)
}

