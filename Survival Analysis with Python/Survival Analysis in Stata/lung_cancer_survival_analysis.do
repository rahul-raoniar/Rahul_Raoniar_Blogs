**# Reading csv file
cls
clear

cd "E:\I'm Currently Working On\1_Currently working on\Youtube Videos\Survival analysis lung data\Survival Analysis Lung Data Stata"

import delimited "lung.csv"


**# Descriptive stats
describe


**# Creating Labels
label variable inst "Institute code"

label variable time  "Survival time in days"

label variable status "censoring status 1 = censored, 2 = dead"

label variable age "Age in years"

label variable sex "Male = 1 Female = 2"

label variable ph_ecog "ECOG performance score as rated by the physician. 0 = asymptomatic, 1 = symptomatic but completely ambulatory, 2 = in bed <50% of the day, 3 = in bed > 50% of the day but not bedbound, 4 = bedbound"

label variable ph_karno "Karnofsky performance score (bad=0-good=100) rated by physician"

label variable pat_karno "Karnofsky performance score as rated by patient"

label variable meal_cal "Calories consumed at meals"

label variable wt_loss "Weight loss in last six months"

describe

**********************************
**# Check for missing values
**********************************
//search mdesc //install it
mdesc


// Count stats
tab sex

tab ph_ecog


**# Treating missing values #4
drop if ph_ecog == .
drop if ph_ecog == 3


for var ph_karno pat_karno meal_cal wt_loss: summ X \\ replace X = r(mean) if missing(X)

mdesc

**# Recoding variables
label define statuslab 1 "censored" 2 "dead"
label values status statuslab

label define genderlab 1 "Male" 2 "Female"
label values sex genderlab

label define ecoglab 0 "asymptomatic" 1 "symptomatic" 2 "in_bed<50" 
label values ph_ecog ecoglab

describe

//Save and use data
save "Survival analysis of lung data stata.dta" , replace

clear
cls
use "Survival analysis of lung data stata"

describe

*************************************
**# Descriptive statistics
*************************************
su time age ph_karno pat_karno meal_cal wt_loss

tab sex 
tab ph_ecog

*************************************
**# Plotting
*************************************

histogram time, width(25) frequency fcolor(eltblue) lcolor(black%70) lwidth(vthin) kdensity kdenopts(lcolor(orange_red)) xtitle(Time in days)

*************************************
**# Declare survival data
*************************************


//Set survival vars
stset time, failure(status == 2)


*************************************
**# Kaplan Meier Curve
*************************************
sts graph, risktable ylabel(, format(%9.1f))

sts graph, by(sex) risktable ylabel(, format(%9.1f))


*************************************
**# Fitting Cox-Proportional Hazard model
*************************************

// [Cox Proportional Hazard Model (lifelines webpage)] https://lifelines.readthedocs.io/en/latest/Survival%20Regression.html


stcox age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, allbaselevels nohr cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)

// Hazard Ratio
stcox age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, allbaselevels cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)

/*
*************************************
Interpretation
*************************************

Wt.loss has a coefficient of about -0.011.

We can recall that in the Cox proportional hazard model, a higher hazard means more at risk of the event occurring.
The value exp(-0.011) is called the hazard ratio.

Here, a one unit increase in wt loss means the baseline hazard will increase by a factor 
of exp(-0.011) = 0.99 -> about a 1% decrease. 

Similarly, the values in the ecog column are: [0 = asymptomatic (base), 1 = symptomatic but completely ambulatory, 2 = in bed <50% of the day]. The value of the coefficient associated with ecog2, exp(1.194), is the value of ratio of hazards associated with being "in bed <50% of the day (coded as 2)" compared to asymptomatic (coded as 0; base category).

*/

//AIC and BIC
estat ic

estat concordance

//Survival plot
stcurve, survival ylabel(, format(%9.1f))

// Survival function based on sex
stcurve, survival at(sex = (1 2)) ylabel(, format(%9.1f))

//Hazard plot
stcurve, hazard 

//Cumulative hazard
stcurve, cumhaz ylabel(, format(%9.1f))

//Failure
stcurve, failure ylabel(, format(%9.1f))


*************************************
**# Proportional hazard test
*************************************
estat phtest

estat phtest, detail

//Log log curve
stphplot, by(sex) ylabel(, format(%9.1f))

stphplot, by(ph_ecog) ylabel(, format(%9.1f))


//Schoenfield Residual Plot
stphtest, plot(ph_karno) msym(oh)  ylabel(, format(%9.1f))

stphtest, plot(meal_cal) msym(oh)  ylabel(, format(%9.1f))



******************************************
**# Accelerated Failure Time Models
******************************************

//[AFT Lifelines package webpage]
https://lifelines.readthedocs.io/en/latest/Survival%20Regression.html#accelerated-failure-time-models


// Exponential
streg age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, distribution(exponential) time allbaselevels cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)

estat ic

ereturn list

di e(chi2)


***********************************************
**# Comparing multiple distributions
***********************************************
frame dir

frame create ics str32 model float(aic bic)

foreach model in exponential loglogistic  weibull lognormal ggamma  {
    quietly streg age ib(1).sex ib(0).ph_ecog  ph_karno pat_karno meal_cal wt_loss, distribution(`model') time
    quietly estat ic
    matrix S = r(S)
    frame post ics ("`model'") (S[1,5]) (S[1, 6])
}

frame change ics
format aic bic %3.2f
sort aic bic
list

frame change default


*************************************
**# Best Distribution is weibull
*************************************

// Weibull without time ratio
streg age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, distribution(weibull) time allbaselevels cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)


// Weibull with time ratio

streg age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, distribution(weibull) time tratio allbaselevels cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)



/*
*************************************
Interpretation
*************************************

A unit increase in x_i means the average/median survival time changes by a factor of exp(b_i).

Suppose bi was positive, then the factor exp(b_i) is greater than 1, which will decelerate the event time since we divide time by the factor ⇿ increase mean/median survival. Hence, it will be a protective effect.

Likewise, a negative b_i will hasten the event time ⇿ reduce the mean/median survival time.

This interpretation is opposite of how the sign influences event times in the Cox model!



## Example 

Sex, which contains [1: Male (base) and 2: Female], has a positive coefficient. 

This means being a female subject compared to male changes mean/median survival time by exp(0.416) = 1.516, approximately a 52% increase in mean/median survival time.

*/



*************************************
**# Plotting
*************************************

// Survival function for weibull regression
stcurve, survival ylabel(, format(%9.1f))

// Survival function based on sex
stcurve, survival at(sex = (1 2)) ylabel(, format(%9.1f))

// Hazard function for weibull regression
stcurve, hazard

//Cumulative hazard
stcurve, cumhaz ylabel(, format(%9.1f))


/*
*************************************
**# Frality (Gamma)
*************************************
streg age ib(1).sex ib(0).ph_ecog ph_karno pat_karno meal_cal wt_loss, distribution(weibull) time frailty(gamma) tratio allbaselevels cformat(%9.3f) pformat(%5.3f) sformat(%8.3f)

estat ic

*/






















