## File Name: plot.immer_HRM.R
## File Version: 0.04

plot.immer_HRM <- function( x , ... ){
	class(x) <- "mcmc.sirt"
	graphics::plot( x , ... )
}
	
