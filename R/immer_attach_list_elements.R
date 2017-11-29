## File Name: immer_attach_list_elements.R
## File Version: 0.01


immer_attach_list_elements <- function(x, envir)
{
	vars <- names(x)
	for (vv in vars){
		assign( vv , x[[ vv ]] , envir=envir )
	}	
}
