#include "ci.h"

//Equal-prediction Adjusted Concordance Index
double equalci_c(const double* predictions, const double* observations, R_len_t n)
{
	int pos_score = 0, count = 0, i, j;
	double c;
	for(i = 0; i < n-1; i++){	//Only go through the triangle
	  	for(j = i+1; j < n; j++){
	  		//Do not consider the case of equal times or equal predictions
	  		//Case: j>i, i dead
	  		if((observations[n + i] == 1) && (observations[j] > observations[i])){ 
	  			if(observations[i] < observations[j]) {
	  				count +=2;
	  				if( (predictions[i] > predictions[j]) ) pos_score += 2;	//concordant pair
	  				if( (predictions[i] == predictions[j])) pos_score += 1; //score balancing
	  			}
			}

			//Case: i>j, j dead
			if((observations[n + j] == 1) && (observations[i] > observations[j])){
	  			if(observations[i] > observations[j]) {
	  				count +=2;
	  				if( (predictions[i] < predictions[j]) ) pos_score += 2;	//concordant pair
	  				if( (predictions[i] == predictions[j])) pos_score += 1; //score balancing
	  			}
			}

		}//end of for j
	  }//end of for i
	c = (double) pos_score/(count);
	return c;
}

//Interface between R and C
SEXP equalciR2C(SEXP predIn, SEXP obsIn){
	SEXP out;
	double *predictions, *observations, *o;
	R_len_t n;

	PROTECT(predIn = AS_NUMERIC(predIn));
	PROTECT(obsIn = AS_NUMERIC(obsIn));
	PROTECT(out = NEW_NUMERIC(1));
	n = LENGTH(predIn);

	predictions = NUMERIC_POINTER(predIn);
	observations = NUMERIC_POINTER(obsIn);
	o = NUMERIC_POINTER(out);

	*o = equalci_c(predictions, observations, n);
	UNPROTECT(3);
	return out;

}
