//// File Name: immer_jml_rcpp_functions.cpp
//// File Version: 0.39


// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// using namespace arma;


///********************************************************************
///** immer_jml_prob_one_item_one_person      
// [[Rcpp::export]]    
Rcpp::NumericVector immer_jml_prob_one_item_one_person(double theta1, Rcpp::NumericVector b_ii)
{
	int K = b_ii.size();
	Rcpp::NumericVector probs_ii(K);
	double temp=0;
	double tot=0;
	for (int kk=0; kk<K; kk++){
		temp = exp( kk * theta1 - b_ii[kk] );
		tot += temp;
		probs_ii[kk] = temp;
	}
	for (int kk=0; kk<K; kk++){
		probs_ii[kk] = probs_ii[kk] / tot; 
	}   
	//-- output
	return probs_ii;
}
///********************************************************************

///********************************************************************
///** immer_trim_increment_rcpp      
// [[Rcpp::export]]    
double immer_trim_increment_rcpp(double incr, double max_incr)
{
	double incr1 = incr;	
	bool do_cut = TRUE;	
	while ( do_cut ){
		do_cut = FALSE;
		if (incr1 > max_incr){ 
			incr1 = max_incr / 2 ;
			do_cut = TRUE;
		}
		if (incr1 < - max_incr){ 
			incr1 = - max_incr / 2; 
			do_cut = TRUE;
		}
	}	
	//-- output
	return incr1;
}
///********************************************************************

///********************************************************************
///** immer_abs2      
// [[Rcpp::export]]    
double immer_abs2(double x)
{
	double y = x;	
	if (x<0){
		y = - x;
	}
	//-- output
	return y;
}
///********************************************************************

///********************************************************************
///** immer_jml_update_item_derivatives      
// [[Rcpp::export]]    
Rcpp::List immer_jml_update_item_derivatives(Rcpp::NumericVector theta, 
		Rcpp::NumericMatrix score_items, int N, int K, int I, Rcpp::IntegerMatrix dat_resp,
		Rcpp::NumericMatrix b, Rcpp::NumericVector A_ , Rcpp::NumericVector xsi,
		double max_incr, Rcpp::NumericMatrix b_fixed )
{	
	Rcpp::NumericMatrix der1_b(I,K);
	Rcpp::NumericMatrix der2_b(I,K);
	Rcpp::NumericVector b_ii(K+1);
	Rcpp::NumericVector probs_ii(K+1);
	
	//		for (ii in 1:I){
	//			sc_ii <- score_items[ii,]
	//			b_ii <- c(0, b[ii,] )
	//			probs_ii <- probs_pcm_one_item(theta=theta, b_ii=b_ii)			
	//			der1_b[ii,] <- - sc_ii + colSums(probs_ii * dat_resp[,ii] )[-1]
	//			der2_b[ii,] <- colSums( probs_ii*(1-probs_ii)* dat_resp[,ii] )[-1]			
	//		}	

	for (int ii=0; ii<I; ii++){
		for (int kk=0;kk<K;kk++){
			b_ii[kk+1] = b(ii,kk);
			der1_b(ii,kk) = - score_items(ii,kk);
		}
		for (int nn=0;nn<N; nn++){
			if ( dat_resp(nn,ii) == 1 ){
				probs_ii = immer_jml_prob_one_item_one_person(theta[nn], b_ii);
				for (int kk=0; kk<K; kk++){
					der1_b(ii,kk) += probs_ii[kk+1];
					der2_b(ii,kk) += probs_ii[kk+1]*(1-probs_ii[kk+1]);
				}
			}
		}	
	}
	
	//		der1_xsi <- rep(0,NX)
	//		der2_xsi <- rep(0,NX)
	//		for (kk in 1:K){
	//			der1_xsi <- der1_xsi + colSums( der1_b[,kk] * A[,kk,] )
	//			der2_xsi <- der2_xsi + colSums( der2_b[,kk] * A[,kk,] )
	//		}	

	int NX = xsi.size();
	Rcpp::NumericVector der1_xsi(NX);
	Rcpp::NumericVector der2_xsi(NX);
	Rcpp::NumericVector xsi_new(NX);
	Rcpp::NumericVector incr(NX);
	double Aval;	
	for (int hh=0;hh<NX;hh++){	
		for (int ii=0;ii<I;ii++){
			for (int kk=0;kk<K;kk++){	
				Aval = A_[ ii + kk*I + hh*I*K ];
				if ( Aval != 0 ){
					der1_xsi[hh] += der1_b(ii,kk) * Aval ;
					der2_xsi[hh] += der2_b(ii,kk) * Aval ;
				}
			}	
		}
	}
	
	//-- compute increments		
	double eps=1e-20;
	for (int hh=0; hh<NX; hh++){
		incr[hh] = der1_xsi[hh] / ( immer_abs2( der2_xsi[hh] ) + eps ); 
		incr[hh] = immer_trim_increment_rcpp( incr[hh], max_incr);
		xsi_new[hh] = xsi[hh] + incr[hh];
	}
	
	Rcpp::NumericMatrix b_new(I,K);	
	for (int ii=0;ii<I;ii++){
		for (int kk=0;kk<K;kk++){
			b_new(ii,kk) = b_fixed(ii,kk);
			for (int hh=0; hh<NX; hh++){
				Aval = A_[ ii + kk*I + hh*I*K ];
				if ( Aval != 0 ){
					b_new(ii,kk) += Aval * xsi_new[hh];
				}			
			}
		}
	}
	
	//-- output
   return Rcpp::List::create(    
            Rcpp::Named("der1_b") = der1_b,
            Rcpp::Named("der2_b") = der2_b,
			Rcpp::Named("der1_xsi") = der1_xsi,
			Rcpp::Named("der2_xsi") = der2_xsi,
			Rcpp::Named("incr") = incr,			
			Rcpp::Named("xsi") = xsi_new,
			Rcpp::Named("b") = b_new
            ) ; 
}
///********************************************************************

///********************************************************************
///** immer_jml_update_theta_derivatives      
// [[Rcpp::export]]    
Rcpp::List immer_jml_update_theta_derivatives(Rcpp::NumericVector theta, 
		Rcpp::NumericVector score_pers, int N, int K, int I,
		Rcpp::NumericMatrix b, double max_incr, Rcpp::IntegerMatrix dat_resp  )
{
	Rcpp::NumericVector theta_new(N);
	int NP=N*I*(K+1);
	Rcpp::NumericVector probs(NP); 

	//		der1 <- score_pers
	//		der2 <- rep(0,N)
	//		for (ii in 1:I){
	//			b_ii <- c(0,b[ii,])		
	//			probs_ii <- probs_pcm_one_item(theta=theta, b_ii=b_ii)
	//			probs[,ii,] <- probs_ii
	//			M_ii <- rowSums( KM * probs_ii * dat_resp[,ii] )
	//			Var_ii <- rowSums( KM^2 * probs_ii * dat_resp[,ii] ) - M_ii^2
	//			der1 <- der1 - M_ii
	//			der2 <- der2 - Var_ii
	//		}	
	
	Rcpp::NumericVector der1(N);
	Rcpp::NumericVector der2(N);
	Rcpp::NumericVector b_ii(K+1);		
	Rcpp::NumericVector probs_ii(K+1);
	
	for (int nn=0; nn<N; nn++){
		der1[nn] = score_pers[nn];
	}	
	double M_ii=0;
	double Var_ii=0;
	
	for (int ii=0; ii<I; ii++){
		for (int kk=0;kk<K;kk++){
			b_ii[kk+1] = b(ii,kk);
		}	
		for (int nn=0; nn<N; nn++){
			if (dat_resp(nn,ii)==1){		
				probs_ii = immer_jml_prob_one_item_one_person(theta[nn], b_ii);				
				for (int kk=0; kk<K+1; kk++){
					probs[nn + ii*N + kk*N*I] = probs_ii[kk];
				}
				M_ii=0;
				Var_ii=0;
				for (int kk=0;kk<K;kk++){
					M_ii += (kk+1) * probs_ii[kk+1] ;
					Var_ii += pow(kk+1, 2.0) * probs_ii[kk+1] ;
				}
				Var_ii = Var_ii - pow(M_ii, 2.0);
				der1[nn] += - M_ii ; 
				der2[nn] += - Var_ii ; 
			}
		}
	}
	
	//-- compute increments		
	Rcpp::NumericVector incr(N);
	double eps=1e-20;
	for (int nn=0; nn<N; nn++){
		incr[nn] = der1[nn] / ( immer_abs2( der2[nn] ) + eps ); 
		incr[nn] = immer_trim_increment_rcpp( incr[nn], max_incr);
		theta_new[nn] = theta[nn] + incr[nn];
	}	
		
	//-- output
   return Rcpp::List::create(    
            Rcpp::Named("theta") = theta_new,
			Rcpp::Named("der1") = der1,
			Rcpp::Named("der2") = der2,
			Rcpp::Named("probs") = probs
            ) ; 

}
