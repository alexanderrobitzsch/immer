## File Name: logLik_immer.R
## File Version: 0.07

###############################################################
# log-likelihood immer_HRM
logLik.immer_HRM <- function (object, ...)
{
	out <- object$like
	attr(out, "df") <- sum(object$ic$Npars)
	attr(out, "nobs") <- object$ic$N
	class(out) <- "logLik"
	return(out)
}
#################################################################



###############################################################
# log-likelihood immer_cml
logLik.immer_cml <- function (object, ...)
{
	out <- object$loglike
	attr(out, "df") <- sum(object$npars)
	attr(out, "nobs") <- object$N
	class(out) <- "logLik"
	return(out)
}
#################################################################
