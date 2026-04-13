gen mathgrade =.
replace mathgrade = math6 if !missing(math6)
replace mathgrade = math9 if !missing(math9)

gen swegrade =.
replace swegrade = swe6 if !missing(swe6)
replace swegrade = swe9 if !missing(swe9)

gen mathnt =. 
replace mathnt = mathtestgrade6 if !missing(mathtestgrade6)
replace mathnt = mathtestgrade9 if !missing(mathtestgrade9)

gen swent =. 
replace swent = swetestgrade6 if !missing(swetestgrade6)
replace swent = swetestgrade9 if !missing(swetestgrade9)

drop swent 
drop mathnt 
drop mathgrade
drop swegrade

summarize swegrade

replace migrationyear=0 if missing(migrationyear)

gen avach=(zntmath+zntswe)/2 
bysort idschool1: egen sweav=mean(zntswe) if year < 2016


bysort idschool1: egen sweavpre2016=max(sweav)



xtile schoolachquartilemath=mathavpre2016, nq(4)
keep if schoolachquartilemath==3



xtile swequart=sweavpre2016, nq(4)
keep if ==3



preserve
keep if relyear<0

csdid predswescore, time(year) gvar(firsttreatmentyear2_max) method(dripw) never cluster(idschool1)

estat simple
estat event 

csdid_plot, title("pred - swe(nt) ")

restore



ssc install ftools, replace
ssc install reghdfe, replace 


preserve
keep if relyear<0

reghdfe zntmath, female1 parentedu1 migrant1 childimm, absorb(idschool1) vce(cluster idschool1)
estimates store pre_prediction
restore

estimates restore pre_prediction
predict predicted_math, xb
forvalues year=2016/2022{
	su predicted_math zntmath if year == 'year'
}

graph twoway (scatter predicted_math zntmath if year==2017)(lfit predicted_math zntmath if year==2017)

























preserve
egen mean_swe=mean(swent) if year <2016, by(idschool)
drop if missing(mean_swe)
tab firsttreatmentyear2
preserve

xtile achquartile = mean_swe, n(4)
keep if achquartile==1
csdid swegrade, time(year) gvar(firsttreatmentyear2) method(dripw) never

estat event 
estat simple
csdid_plot, title ("mathgrade-bottomachquartile in swedish")
restore