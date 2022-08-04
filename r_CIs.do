/********************************************
* Pairwise correlation confidence intervals *
* Maureen Craig                             *
* Last updated: 8/4/2022                    *
*********************************************
* Program to run to calculate the           *
* 95% CIs around correlation coefficients   *
********************************************/


/* to use this program, run this and call it with the following format:

	r_CIs firstvariable secondvariable
	
*/ 

program r_CIs
args var1 var2
pwcorr `var1' `var2', sig obs
quietly{
	scalar corrcoef = r(rho)
	scalar zLCI = atanh(corrcoef) - (1.96*(1/(sqrt(r(N) - 3)))) 
    scalar zUCI = atanh(corrcoef) + (1.96*(1/(sqrt(r(N) - 3))))
	scalar r_LCI = tanh(zLCI)
	scalar r_UCI = tanh(zUCI)
	}
display "variables = `var1' `var2'"
	scalar list corrcoef r_LCI r_UCI /* point estimate and 95% CIs */
display "  "
end
