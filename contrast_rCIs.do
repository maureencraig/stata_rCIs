/***************************************
* Contrast rs & confidence intervals   *
* Maureen Craig                        *
* Last updated: 8/13/2021              *
****************************************
* Program to run after contrasts to    *
* calculate the contrast correlation   *
* coefficients and 95% CIs             *
* formulas from Furr & Rosenthal 2003  *
***************************************/

/* to use this program, run this with the following format 
after running a contrast command (e.g., following a regression):

regress outcome FactorVariable covariates_if_desired 
scalar samplesize = e(N)
contrast {FactorVariable -1 -1 1 1}, overall // <-these contrast weights should be whatever your question dictates 
contrast_rCIs

*/

program contrast_rCIs
	quietly {
		matrix contrastcoeff = r(b)
		matrix Fvalue = r(F) 
		matrix dfs = r(df2)
		scalar corrcoeff = sqrt((Fvalue[1,1])/(Fvalue[1,1]+dfs[1,1]))
		if(contrastcoeff[1,1]<0) { /*ensure sign of effect size will be correct*/
			scalar corrcoeff = -(corrcoeff)
			}
			else {
			scalar corrcoeff = corrcoeff
			}	
		scalar rzLCI = atanh(corrcoeff) - (1.96*(1/(sqrt(samplesize - 3))))
		scalar rzUCI = atanh(corrcoeff) + (1.96*(1/(sqrt(samplesize - 3))))
		scalar rLCI = tanh(rzLCI)
		scalar rUCI = tanh(rzUCI)
	}
	scalar list corrcoeff rLCI rUCI /* point estimate and 95% CIs */
end
