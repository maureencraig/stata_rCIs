/******************************************
* Partial rs & confidence intervals       *
* for 2 predictor regression effect sizes *
* Maureen Craig                           *
* Last updated: 8/3/2022                 *
*******************************************
* Program to run to calculate the         *
* partial correlation coefficients        *
* and 95% CIs for individual predictors   *
* in a 2-predictor linear regression      *
******************************************/



/* to use this program, run this with the following format:

    gen fullsample = . //  you need to create this variable first, so that the replace can work in the program
	regress_rpCIs_2predictors outcomevariable firstpredictorvariable secondpredictorvariable
	
*/ 

program regress_rpCIs_2predictors
args outcome pred1 pred2
regress `outcome' `pred1' `pred2'
quietly{
	matrix Full_Bs = e(b)
	replace fullsample = e(sample) /* ensure that the models w/ fewer parameters use the same observations */
scalar samplesize = e(N)
scalar pred1_b = Full_Bs[1,1]
scalar pred2_b = Full_Bs[1,2]
//semi-partial & partial correlation coefficients 
  scalar r2FULL = e(r2)
regress `outcome' `pred2' if(fullsample)
  scalar r2noP1 = e(r2)
regress `outcome' `pred1' if(fullsample)
  scalar r2noP2 = e(r2)
scalar r2changeP1 = r2FULL - r2noP1
scalar r2changeP2 = r2FULL - r2noP2
scalar semipartialP1 = sqrt(r2changeP1)
scalar semipartialP2 = sqrt(r2changeP2)
scalar partialcorrP1 = semipartialP1 / sqrt((1-r2noP1))
scalar partialcorrP2 = semipartialP2 / sqrt((1-r2noP2))
  if(pred1_b<0) { /*ensure sign of effect size is going to be correct*/
			scalar partialcorrP1 = -(partialcorrP1)
			}
			else {
			scalar partialcorrP1 = partialcorrP1
			}	
  if(pred2_b<0) { /*ensure sign of effect size is going to be correct*/
			scalar partialcorrP2 = -(partialcorrP2)
			}
			else {
			scalar partialcorrP2 = partialcorrP2
			}	
    scalar P1zLCI = atanh(partialcorrP1) - (1.96*(1/(sqrt(samplesize - 4)))) /* it's 4 because 3 + 1 predictor being adjusted for */
    scalar P1zUCI = atanh(partialcorrP1) + (1.96*(1/(sqrt(samplesize - 4))))
	scalar P1_LCI = tanh(P1zLCI)
	scalar P1_UCI = tanh(P1zUCI)
	scalar P2zLCI = atanh(partialcorrP2) - (1.96*(1/(sqrt(samplesize - 4)))) 
    scalar P2zUCI = atanh(partialcorrP2) + (1.96*(1/(sqrt(samplesize - 4))))
	scalar P2_LCI = tanh(P2zLCI)
	scalar P2_UCI = tanh(P2zUCI)
}
display "OUTCOME = `outcome'"
display "Predictor1 = `pred1'"
scalar list partialcorrP1 P1_LCI P1_UCI 
display "Predictor2 = `pred2'"
scalar list partialcorrP2 P2_LCI P2_UCI 
display "  "
end
