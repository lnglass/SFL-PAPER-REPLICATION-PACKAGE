
ssc install ftools, replace
ssc install reghdfe, replace 
ssc install csdid, replace

###without covariates


preserve
keep if "insert subgroup"

csdid "outcome", time(year) gvar(firsttreatmentyear2_max) method(dripw) never cluster(idschool1)

estat simple
estat pretrend
estat event 

csdid_plot, title("title")

restore



###with private school covariate
preserve
keep if "insert subgroup"

csdid "outcome" priv, time(year) gvar(firsttreatmentyear2_max) method(dripw) never cluster(idschool1)

estat simple
estat event 

csdid_plot, title("title")

restore




###compositional changes 

preserve
keep if year<2016

reghdfe zntmath, female1 parentedu1 migrant1 childimm, absorb(idschool1) vce(cluster idschool1)
estimates store pre_prediction
restore

estimates restore pre_prediction
predict predicted_math, xb
forvalues year=2016/2022{
	su predicted_math zntmath if year == 'year'
}

graph twoway (scatter predicted_math zntmath if year==2017)(lfit predicted_math zntmath if year==2017)



csdid "predicted outcome", time(year) gvar(firsttreatmentyear2_max) method(dripw) never cluster(idschool1)

estat simple
estat event 

csdid_plot, title("title")


