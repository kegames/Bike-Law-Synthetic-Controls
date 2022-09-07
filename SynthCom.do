
use S:/kegames/Microeconometrics/SynthDataRed.dta, clear 

drop numdate day_gen month_gen year_gen
generate numdate = date(date, "MDY")
format numdate %td

gen day_gen = day(numdate)
gen month_gen = month(numdate)
gen year_gen = year(numdate)




ssc install synth 
ssc install mat2txt
ssc install schemepack, replace // Appealing graph schemes (optional)
set scheme white_tableau, perm // Add ", perm" after this command to lock change in
********************************************

** Initial Data - Visualize for Parallel Trends

gen notKY = (state != "KY")
bysort numdate notKY: egen avg_crashes = mean(crashes)
twoway (line avg_crashes numdate if notKY == 1) || ///
(line avg_crashes numdate if notKY == 0,	///
legend(lab(1 "Other") lab(2 "KY")) xtitle("time") ytitle("Total Crashes")), xline (`=td(01july2018)')

********************************************

tsset id numdate

** Synth 1 
#delimit; 
synth crashes ///
		incomepercapita population averageelevation temperature  crashes(21275) crashes(21001)  crashes(20636) crashes(20789) crashes(21216), ///
	trunit(5) trperiod(`=td(01jul2018)') unitnames(state) keep(S:/kegames/Microeconometrics.dta) replace fig; mat list e(V_matrix);
	
** Synth 2 
#delimit;  
synth crashes ///
	incomepercapita population averageelevation averageprecipitation crashes(21275) crashes(21001)  crashes(20636) crashes(20789) crashes(21216), ///
	trunit(5) trperiod(`=td(01jul2018)') unitnames(state) keep(S:/kegames/Microeconometrics.dta) replace fig; mat list e(V_matrix);
********************************************

** Gap data for Synth 2
use S:/kegames/Microeconometrics.dta, clear
keep _Y_treated _Y_synthetic _time
drop if _time==.
rename _time monthyear
rename _Y_treated  treat
rename _Y_synthetic counterfact
gen gap5=treat-counterfact
sort monthyear
#delimit ; 
twoway (line gap5 monthyear,lp(solid)lw(vthin)lcolor(black) yscale(range(-30 30))), yline(0, lpattern(shortdash) lcolor(black))
    xline(21366, lpattern(shortdash) lcolor(black)) xtitle("",si(medsmall)) xlabel(#10) 
    ytitle("Gap in Crashes between Kentucky and Other States", size(medsmall)) legend(off); 
    #delimit cr
    save S:/kegames/Microeconometrics.dta, replace
	
********************************************

** Placebo 
* Inference 1 placebo test  
#delimit; 
use S:/kegames/Microeconometrics/SynthDataRed.dta, replace;
local statelist  1 2 3 4 5 6 7 8; 
foreach i of local statelist {;
	synth crashes ///
			incomepercapita population averageelevation temperature crashes(21275) crashes(21001)  crashes(20636) crashes(20789) crashes(21216),
		trunit(`i') trperiod(`=td(01jul2018)') unitnames(state) keep(S:/kegames/Microeconometrics_`i'.dta) replace; 
			
};


********************************************

**Combine files for placebo
local statelist  1 2 3 4 5 6 7 8
foreach i of local statelist {
	use S:/kegames/Microeconometrics_`i'.dta ,clear
    keep _Y_treated _Y_synthetic _time
    drop if _time==.
    rename _time monthyear
    rename _Y_treated  treat`i'
    rename _Y_synthetic counterfact`i'
    gen gap`i'=treat`i'-counterfact`i'
    sort monthyear 
    save S:/kegames/Microeconometrics_`i'.dta, replace
    }
use S:/kegames/Microeconometrics.dta, clear
sort monthyear
save S:/kegames/MicroeconometricsPlaceboDataRed.dta, replace

foreach i of local statelist {
        merge monthyear using S:/kegames/Microeconometrics_`i'.dta 
        drop _merge 
        sort monthyear 
    save S:/kegames/MicroeconometricsPlaceboDataRed.dta, replace 
    }
********************************************

** Calculate pre and post rmspe   
set more off
local statelist  1 2 3 4 5 6 7 8 
foreach i of local statelist {

    use S:/kegames/Microeconometrics_`i', clear
    gen newgap=gap`i'*gap`i'
    egen postmean=mean(newgap) if monthyear>21336
    egen premean=mean(newgap) if monthyear<=21336
    gen rmspe=sqrt(premean) if monthyear<=21336
    replace rmspe=sqrt(postmean) if monthyear>21336
    gen ratio=rmspe/rmspe[_n-1] if 21366
    gen rmspe_post=sqrt(postmean) if monthyear>21336
    gen rmspe_pre=rmspe[_n-1] if 21366
    mkmat rmspe_pre rmspe_post ratio if 21366, matrix (state`i')
	save S:/kegames/Microeconometrics_`i'.dta, replace
}

local statelist  1 2 3 4 5 6 7 8
foreach i of local statelist {
	use S:/kegames/Microeconometrics_`i', clear
	gen state = `i'
	save S:/kegames/Microeconometrics_`i'.dta, replace
}

clear
local statelist 1 2 3 4 5 6 7 8
foreach i of local statelist {
        append using S:/kegames/Microeconometrics_`i'.dta 
	save S:/kegames/Combination.dta, replace 
    }



********************************************

* Inference 3: all the placeboes on the same picture

* Picture of the full sample, including outlier RSMPE
* Extreme outliers removed
#delimit;   
twoway 
(line gap1 monthyear ,lp(solid)lw(vthin)) 
(line gap2 monthyear ,lp(solid)lw(vthin)) 
(line gap3 monthyear ,lp(solid)lw(vthin)) 
(line gap4 monthyear ,lp(solid)lw(vthin))
(line gap5 monthyear ,lp(solid)lw(thick)lcolor(black)) /*treatment unit, Kentucky*/
(line gap6 monthyear ,lp(solid)lw(vthin))
(line gap7 monthyear ,lp(solid)lw(vthin)), 

yline(0, lpattern(shortdash) lcolor(black)) xline(21366, lpattern(shortdash) lcolor(black))
xtitle("",si(small)) xlabel(#10) ytitle("Gap in Kentucky and Other States Prediction Error", size(small))
    legend(off);
#delimit cr


********************************************

* Final Conclussion and Significance test 

gsort - ratio
gen rank = _n
gen p = rank/8
list rank p if (state==5) 
	
histogram ratio, bin(25) frequency fcolor(gs13) lcolor(black) ylabel(0(20)250) xtitle(Post/pre RMSPE ratio) xlabel(0(.5)3) ytitle("Distribution of Ratios", size(small))
	
