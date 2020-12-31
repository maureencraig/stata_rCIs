/***********
* R CIs    *
* M. Craig *
* 12/2020  *
************
* program to use after regression command to pop out 
correlation coefficients and 95% CIs */

program contrast_rCIs
	quietly {
		matrix contrastcoeff = r(b)
		matrix Fvalue = r(F) 
		matrix dfs = r(df2)
		scalar corrcoeff = sqrt((Fvalue[1,1])/(Fvalue[1,1]+dfs[1,1]))
		if(contrastcoeff[1,1]<0) { /*ensure sign of effect size is going to be correct*/
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
