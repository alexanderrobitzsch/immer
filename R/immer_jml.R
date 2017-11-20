## File Name: immer_jml.R
## File Version: 0.66


immer_jml <- function(dat, A=NULL, center_theta=TRUE, b_fixed=NULL, irtmodel="PCM", 
				eps = .3, eps_adjust_pers=TRUE, eps_adjust_item=TRUE, 
				maxiter=1000, conv=1E-6, max_incr=.5, maxiter_update=10, conv_update=1E-5, 
				verbose = TRUE, use_Rcpp=TRUE )
{

	time <- list( "start" = Sys.time() )
	CALL <- match.call()
	
	#-- sufficient statistics
	dat0 <- dat
	dat_resp <- 1 - is.na(dat)	
	dat[ is.na(dat) ] <- 0
	N <- nrow(dat)
	I <- ncol(dat)
	K <- max(dat, na.rm=TRUE)
	maxK <- apply( dat, 2, max, na.rm=TRUE)
	
	#-- create design matrix if not provided
	res <- immer_jml_create_design_matrix_A( maxK=maxK, A=A, b_fixed=b_fixed, irtmodel=irtmodel )
	A <- res$A
	b_fixed <- res$b_fixed

	#-- scoring
	maxK_M <- immer_matrix2(maxK, nrow=N)
	max_pers <- rowSums( maxK_M * dat_resp )
	sumscore_pers <- rowSums(dat)
	
	person <- data.frame(index = 1:N, sum_score = sumscore_pers, max_pers=max_pers )
	if (eps_adjust_pers){
		person$eps <- eps
		max_pers1 <- maxK_M * dat_resp		
		person$N_adj <- rowSums( max_pers1 * (max_pers1 + 1 ) / 2 )	
		person$eps_i <- person$eps / person$N_adj
		person$score_pers <- person$eps + ( person$max_pers - 2 * person$eps )/person$max_pers * person$sum_score
	} else {
		person$eps <- eps
		person$eps_i <- 0
		person$score_pers <- person$sum_score
		person$score_pers <- ifelse( person$sum_score == 0, eps, person$score_pers )
		person$score_pers <- ifelse( person$sum_score == person$max_pers, person$max_pers - eps, person$score_pers )
	}

	dat_score <- array( dat_resp * person$eps_i , dim=c(N,I,K+1) )
	for (ii in 1:I){		
		if (maxK[ii] < K){
			dat_score[,ii, seq(maxK[ii]+2,K+1) ] <- 0
		}	
		for (kk in 0:K){
			dat_add <- ( dat[,ii] == kk ) * ( 1 - person$eps_i * (maxK[ii]+1) ) * dat_resp[,ii]
			dat_score[,ii,kk+1] <- dat_score[,ii,kk+1] + dat_add
		}
	}

	#-- sufficient statistics
	score_pers <- rep(0,N)
	for (kk in 1:K){
		score_pers <- score_pers + kk * rowSums( dat_score[,,kk+1] )
	}		
	if ( ! eps_adjust_pers ){
		score_pers <- person$score_pers
	}
	
	score_items0 <- matrix( NA , nrow=I, ncol=K)
	score_items <- matrix( NA , nrow=I, ncol=K)	
	for (kk in 1:K){
		score_items0[,kk] <- colSums( (dat == kk)*dat_resp , na.rm=TRUE )
		score_items[,kk] <- colSums( dat_score[,,kk+1] , na.rm=TRUE )
	}

	if ( ! eps_adjust_item){
		score_items <- score_items0
	}
	
	#-- initial parameters
	theta <- stats::qlogis( ( score_pers + .5) / ( max_pers + 1) )
	theta <- immer_jml_center_theta( theta=theta, center_theta=center_theta )
	
	dimA <- dim(A)
	xsi <- rep(0,dimA[3])
	b <- matrix(0, nrow=I, ncol=K)
	if (is.null(b_fixed)){
		b_fixed <- b	
	}

	#-- process data
	N <- length(theta)
	I <- ncol(dat)

	iter <- 0	
	iterate <- TRUE
	
	if (use_Rcpp){
		fct_item <- immer_jml_update_item_Rcpp
		fct_theta <- immer_jml_update_theta_Rcpp
	} else {	
		fct_item <- immer_jml_update_item_R
		fct_theta <- immer_jml_update_theta_R
	}
		
	#--- begin algorithm 
	while (iterate){
		
		xsi0 <- xsi
		theta0 <- theta

		#** update item parameters
		args_item <- list( score_items=score_items, I=I, K=K, b=b, A=A, xsi=xsi, theta=theta, N=N, dat_resp=dat_resp, 
					max_incr=max_incr, maxiter_update=maxiter_update, conv_update=conv_update, 
					b_fixed=b_fixed ) 
		res <- do.call( what=fct_item, args=args_item)
		b <- res$b
		xsi <- res$xsi
		xsi_der2 <- res$xsi_der2

		#** update person parameters
		args_theta <- list( score_pers=score_pers, I=I, K=K, N=N, theta=theta, b=b, dat_resp=dat_resp, 
					 maxiter_update=maxiter_update, conv_update=conv_update, center_theta=center_theta, 
					 max_incr=max_incr ) 
		res <- do.call( what=fct_theta, args=args_theta)
		theta <- res$theta
		theta_der2 <- res$theta_der2
		probs <- res$probs
		
		#** calculate log-likelihood
		loglike <- immer_jml_calc_loglike( dat_score=dat_score, probs=probs, K=K)		
		deviance <- -2*loglike
		
		iter <- iter + 1		
		item_parm_change <- max( abs(xsi0-xsi))
		pers_parm_change <- max( abs(theta0-theta))
		
		if (iter >= maxiter ){ iterate <- FALSE }
		if (item_parm_change < conv ){ iterate <- FALSE }
		if (pers_parm_change < conv ){ iterate <- FALSE }

		if (verbose){
			v1 <- paste0("* Iteration " , iter , " | Deviance = " , round( deviance, 6 ) , "\n" , 
						"    Item parm. change = " , round(item_parm_change, 8) )
			v1 <- paste0( v1 , " | Person parm. change = " , round(pers_parm_change, 8) )
			cat(v1 , "\n")
			utils::flush.console()		
		}
		
	}
	#--------------
	
	#-- standard errors
	eps <- 1E-20
	xsi_se <- sqrt( abs(1 / xsi_der2 ) + eps)
	theta_se <- sqrt( abs(1 / theta_der2 ) + eps)
	
	#-- information criteria
	ic <- immer_jml_ic( loglike=loglike, N=N, center_theta=center_theta, xsi=xsi, I=I ) 
		
	#-- person parameters
	person$theta <- theta
	person$theta_se <- theta_se
	person_desc <- list( mean=mean(theta), sd = stats::sd(theta),
						min=min(theta) , max=max(theta) )
	
	#-- item parameters
	rownames(b) <- colnames(dat)
	colnames(b) <- paste0("Cat", 1:K)	
	item <- data.frame("item"= colnames(dat), "N" = colSums(dat_resp) ,
					"M" = colSums( dat * dat_resp ) / colSums(dat_resp) )	
	item$SD <- colSums( dat^2 * dat_resp ) / colSums(dat_resp)
	item$SD <- sqrt( item$SD - item$M^2 )					
	item <- data.frame( item, b )
	names(xsi) <- dimnames(A)[[3]]	
	xsi_dfr <- data.frame("par" = names(xsi) , "est" = xsi, "se" = xsi_se )
	
	#--- output
	time$end <- Sys.time()
	time$diff <- time$end - time$start
	res <- list(b=b, item=item, theta=theta, theta_se=theta_se, xsi=xsi, xsi_se=xsi_se, 
					xsi_dfr=xsi_dfr, probs=probs, 
					person=person, person_desc=person_desc,
					dat=dat, dat_score=dat_score, dat_resp=dat_resp, score_pers=score_pers,
					score_items=score_items, eps=eps, eps_adjust_pers=eps_adjust_pers, 
					eps=eps, eps_adjust_item=eps_adjust_item,  A=A, b_fixed=b_fixed, 
					iter=iter, 
					loglike=loglike, deviance=deviance, ic=ic, 
					CALL=CALL,	time=time)
	res$description <- "Function 'immer_jml' | Joint maximum likelihood estimation" 
	class(res) <- "immer_jml"
	return(res)
}
