/*********************************************
* Partial r with confidence intervals        *
* for one predictor of interest to use as    *
* regression effect sizes                    *
* Maureen Craig                              *
* Last updated: 10/27/2022                   *
**********************************************
* Program to run to calculate a partial      *
* correlation coefficient and 95% CIs for    *
* an individual predictor in a complex       *
* survey data regression on a subpopulation  *
*********************************************/



/* to use this program, run this program and call it with the following format:

	svy_sub_rp_CIs subpopulation_variable outcome_variable variable_of_interest covariate1 covariate2 covariate3...
	
	list the subpopulation variable first, followed by the terms for the regression model, with the outcome 
	followed by the predictor of interest and then any covariates afterwards.
	This program can handle up to 12 variables, but you can adjust lines 40, 50, and 70 as needed, 
	if your model requires more variables. Don't use indicator notation (i.) as that will 
	mess up the program.
	
	Example of how to use this:
	
	svy_sub_rp_CIs WWsubpop attnrace structsex c.cpolitconserv c.cagenum c.cincomenum 
	
	^in this example, the subpopulation variable is WWsubpop, the outcome variable is attnrace, the predictor of interest
	is structsex, and there are 3 covariates (c.cpolitconserv c.cagenum c.cincomenum)
	
*/ 


//list the subpopulation variable first, then the regression model with the predictor of interest listed immediately after the outcome

program svy_sub_rp_CIs
tempvar fullsample
svy linearized, subpop(`1') : regress `2' `3' `4' `5' `6' `7' `8' `9' `10' `11' `12'
quietly{
  matrix Full_bs = e(b)
  scalar pred_b = Full_bs[1,1] // labeling things with indicator notation will mess this up, so don't use i. notation
  scalar num_pred = colsof(Full_bs) // remember that this counts the intercept.
  scalar num_adj = num_pred-2 // subtract 2 for predictor of interest + intercept
  gen `fullsample' = e(sample) /* ensure that the models w/ fewer parameters use the same observations */
  scalar samplesize = e(N)
//semi-partial & partial correlation coefficients 
  scalar r2FULL = e(r2)
  svy linearized, subpop(`1') : regress `2' `4' `5' `6' `7' `8' `9' `10' `11' `12' if(`fullsample')
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
    scalar zLCI = atanh(partialcorr) - (1.96*(1/(sqrt(samplesize - 3 - num_adj)))) /* subtract 3 + # covariates being adjusted for */
    scalar zUCI = atanh(partialcorr) + (1.96*(1/(sqrt(samplesize - 3 - num_adj))))
	scalar LCI = tanh(zLCI)
	scalar UCI = tanh(zUCI)
}
display "arguments = `0'"
display "subpop variable = `1'"
display "regression model outcome = `2'"
display "predictor of interest = `3'"
display "all regression model predictors = `3' `4' `5' `6' `7' `8' `9' `10' `11' `12'"
scalar list num_adj pred_b samplesize r2FULL r2no_pred r2change // print what numbers you used, as a double-check
display "partial correlation, LCI, UCI"
scalar list partialcorr LCI UCI 
display "  "
end
