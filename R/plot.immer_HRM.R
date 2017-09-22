## File Name: plot.immer_HRM.R
## File Version: 0.04
## File Last Change: 2017-01-16 19:59:28

plot.immer_HRM <- function( x , ... ){
	class(x) <- "mcmc.sirt"
	graphics::plot( x , ... )
}
	
