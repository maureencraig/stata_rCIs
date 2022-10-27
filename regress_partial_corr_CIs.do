/*******************************************
* Partial r with confidence intervals       *
* for one predictor of interest to use as   *
* regression effect sizes                   *
* Maureen Craig                             *
* Last updated: 10/27/2022                  *
*******************************************
* Program to run to calculate a partial     *
* correlation coefficient and 95% CIs for   *
* an individual predictor in a linear       *
* regression                                *
********************************************/



/* to use this program, run this program and call it with the following format:

	regress_rp_CIs outcome_variable variable_of_interest covariate1 covariate2 covariate3...
	
	list regression model as the arugments with the outcome first, followed by
	the predictor of interest and any covariates afterwards.
	This program can handle up to 12 variables, but you can adjust line 44 as needed, 
	if your model requires more variables. Don't use indicator notation (i.) as that will 
	mess up the program.
	
	Example of how to use this:
	
	regress_rp_CIs attnrace structsex c.cpolitconserv c.cagenum c.cincomenum 
	
*/ 

program regress_rp_CIs
tempvar fullsample
regress `*'
quietly{
  matrix Full_bs = e(b)
  scalar pred_b = Full_bs[1,1] // labeling things as indicator variables will mess this up, so don't use i. notation
  scalar num_pred = colsof(Full_bs) // remember that this counts the intercept.
  scalar num_adj = num_pred-2 // subtract 2 for predictor of interest + intercept
  gen `fullsample' = e(sample) /* ensure that the models w/ fewer parameters use the same observations */
  scalar samplesize = e(N)
//semi-partial & partial correlation coefficients 
  scalar r2FULL = e(r2)
  regress `1' `3' `4' `5' `6' `7' `8' `9' `10' `11' `12' if(`fullsample')
  scalar r2no_pred = e(r2)
  scalar r2change = r2FULL - r2no_pred
  scalar semipartial = sqrt(r2change)
  scalar partialcorr = semipartial / sqrt((1-r2no_pred))
  if(pred_b<0) { /*ensure sign of effect size is going to be correct*/
			scalar partialcorr = -(partialcorr)
			}
			else {
			scalar partialcorr = partialcorr
			}	
  scalar zLCI = atanh(partialcorr) - (1.96*(1/(sqrt(samplesize - 3 - num_adj)))) /* subtract 3 + # predictors being adjusted for */
  scalar zUCI = atanh(partialcorr) + (1.96*(1/(sqrt(samplesize - 3 - num_adj))))
  scalar LCI = tanh(zLCI)
  scalar UCI = tanh(zUCI)
}
display "regression model = `*'"
scalar list num_adj pred_b samplesize r2FULL r2no_pred r2change // print what numbers you used, as a double-check
display "predictor of interest = `2'"
display "partial correlation, LCI, UCI"
scalar list partialcorr LCI UCI 
display "  "
end

