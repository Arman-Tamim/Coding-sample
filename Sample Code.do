/******************************************************************************************************************
* Title: Impact of Migration on Landholding
* Created by: Tamim
* Created on: December 06, 2023
* Last Modified on: April 25, 2025
* Last Modified by: Tamim
*****************************************************************************************************************/

* Clear existing data and reset the session
cls
clear

* Check the hostname of the computer and set project directory accordingly
if "`c(hostname)'" == "DESKTOP-TAMIM-ARMAN" {
    global proj_dir "D:\landholding\migration and land renting"
} 
	else {
    * If on a different machine, set project directory to be specified by user
    global proj_dir "Put your directory here"
}

* Define global paths for different data and analysis directories
global data_analysis "$proj_dir\Analysis"        // Folder for data analysis
global data_clean   "$proj_dir\Clean"            // Folder for cleaned data
global analysis_do_dir "$proj_dir\do"            // Folder for Stata do-files
global results       "$proj_dir\Result"          // Folder for results
global figure        "$proj_dir\figure"          // Folder for figures/plots

**# Create necessary directories if they do not already exist
cap mkdir "$data_clean"
cap mkdir "$results"
cap mkdir "$figure"




**# Cleaning
cap set scheme white_tableau
 if _rc!=0{
 	ssc install schemepack
	set scheme white_tableau
 }

**# Renaming Geographic code
use "$proj_dir\hh_sec_a", clear
sort hhold
* Division Code
rename id_01_code div_code
*District Code
rename id_02_code dist_code
*Upzilla Code
rename id_03_code up_code
* Rural Dummy
rename ruc rural
save "$data_clean\dist", replace

**# Educational Attainment
use "$proj_dir\HH_SEC_2A", clear

*Replacing the missing values of s2aq04, which indicates highest class attended, with "0" if if never attende education
replace s2aq04=0 if s2aq03==2
keep hhold id_code s2aq04 s2aq0a 

rename s2aq04 edu


rename id_code lino
rename  s2aq0a resp_lino

* Categoraizing Educational attainment 
recode edu (0=0 "No class")  (1/5=1 "Primary") (6/8=2 "Below 8") (9/12=3 "ssc and hsc") (12/19=4 "higher education") , gen(edu_cat)



* Generating variables for the highest and the average educartional attainment by the household members and for their inverse hyperbolic

bysort hhold : egen max_edu = max(edu)

gen log_max_edu = log( max_edu +sqrt( (max_edu*max_edu)+1))

bysort hhold : egen mean_edu = mean(edu)

gen log_mean_edu = log( mean_edu +sqrt( (mean_edu*mean_edu)+1))



sort hhold lino

count

save "$data_clean\edu", replace

**#Household Roaster

use "$proj_dir\HH_SEC_1A", clear
* Identifying whether the member is Muslim
gen muslim = (s1aq04==1)

* Whether the member is a returnee migrant
gen returnee_migrant = (s1aq09!=.)

* Does the member has any disablity
gen disability_ind = (s1aq12 !=1 | s1aq13 !=1 | s1aq14 !=1 | s1aq15 !=1 | s1aq16 !=1 | s1aq17!=1)
* Wheteher the member is a working age male
gen mwa_mem= (s1aq03>14 & s1aq03<65 & s1aq01==1)
*Wheteher the member is a working age female
gen fwa_mem= (s1aq03>14 & s1aq03<65 & s1aq01==2)
*Total working age males in the household
egen mwa_memT=sum( mwa_mem), by(hhold) 
*Total working age females in the household
egen fwa_memT=sum( fwa_mem), by(hhold)
* Total persons with disabilities in the household
egen sum_disability = sum(disability_ind) , by(hhold)
* Dummy variable for at least one persons with disability in the houshold
gen hh_disability = (sum_disability>0)
* Total returnee migrants in the household
egen sum_returnee_migrant = sum(returnee_migrant), by(hhold)
* Dummy for at least one retuirnee migrant in the household
gen hh_returnee_migrant = (sum_returnee_migrant>0)

* Household Demographics
keep hhold s1aq0a s1aq01 s1aq03 s1aq00 s1aq02 mwa_memT fwa_memT s1aq02 muslim hh_disability hh_returnee_migrant

rename s1aq01 sex
rename s1aq03 age
rename s1aq00 lino
rename s1aq0a resp_lono

* Generating a variable for the sex and age of the household age for each household member 
foreach var in sex age{
	gen `var'hhead = `var' if s1aq02== 1
	bysort hhold (`var'hhead) : replace `var'hhead = `var'hhead[1]
}

save "$data_clean\members_demo", replace


**#Household head's demographic

sort hhold lino
merge 1:1 hhold lino using "$data_clean\edu"
tab _merge
keep if _merge==3
drop _merge
 
* Keep if the member is the household head
keep if s1aq02==1
sort hhold
* Generating variable for head's educational attainment and the inverse hyperbolic of it
gen eduhhead = edu 
gen log_eduhhead = log( edu +sqrt( (edu*edu)+1))

save "D:\landholding\migration and land renting\head_demo", replace

**#Migration Module

* Load household migration dataset
use "$proj_dir\HH_SEC_8C", clear

* Generate a binary variable for internal migration (value 1 if s8cq06 == 1)
gen internal_migration = (s8cq06==1)

* Calculate number of internal migrants within each household
egen sum_internal_migration = sum(internal_migration), by(hhold)

* Create international migration indicator
gen m_i = (s8cq06 == 2)

* Convert months of migration (s8cq05a) into years
gen duration1 = (s8cq05a / 12)

* Compute total migration duration in months (adding months and years)
egen duration = rowtotal(duration1 s8cq05b)
label variable duration "Duration of migration (years)"

* Cap extreme values at 30 years to remove outliers
replace duration = 30 if duration > 30

* Plot kernel density of duration for current migrants (s8cq06 == 2)
kdensity duration if s8cq06==2, color(emerald) yscale(range(0 .15)) ylabel(0 (0.05) .15)

* Save density plot
graph export "$figure\density.png", replace

* Short-term migrants: duration less than or equal to 5 years (5 years is the median)
gen m_is = (s8cq06 == 2 & duration < 5.001)

* Long-term migrants: duration greater than 5 years
gen m_il = (s8cq06 == 2 & duration > 5)

* Crosstab: Migration status by s8cq10 (reason for migration)
ta m_i s8cq10, nof row

* Collapse data to household level and sum relevant migration variables
collapse (sum) sum_internal_migration m_i m_is m_il , by(hhold)

* Generate binary indicators at household level
gen hh_internal_migration = (sum_internal_migration > 0)

* whether the household has any, short-term, and/or long-term migrants
gen m_iD = (m_i > 0)
gen m_isD = (m_is > 0)
gen m_ilD = (m_il > 0)


* Sort dataset by household ID
sort hhold

* Save the final household-level migration dataset
save "$data_clean\migration", replace

**# Land Module

* Load household land data
use "$proj_dir\HH_SEC_7A", clear
sort hhold

* Merge with migration data
merge 1:1 hhold using "$data_clean\migration"
keep if _merge==3
drop _merge

* Merge with geographic codes
merge 1:1 hhold using "$data_clean\dist"
keep if _merge==3
drop _merge

* Merge with household head demographic data
sort hhold
merge hhold using "$data_clean\head_demo"
keep if _merge==3
drop _merge

* Handle missing land data: replace missing values with 0 for six land variables
forvalues i = 1/6 {
    recode s7aq0`i' .=0
}

* Generate variables for different land types (crop, homestead, fallow, rented in, rented out)
gen land1_c = s7aq01
gen land2_h = s7aq02
gen land3_u = s7aq03
gen land4_ri = s7aq04
gen land5_ro = s7aq05

* Total owned land = sum of crop, homestead, and uncultivable
egen land_t = rowtotal(s7aq01 s7aq02 s7aq03)

* Operated land = total owned + rented in - rented out
gen land6_o = s7aq01 + s7aq02 + s7aq03 + s7aq04 - s7aq05

* Replace missing values again as a precaution
forvalues i = 1/5 {
    recode s7aq0`i' .=0
}

* Replace negative land values with 0
local vars land1_c land2_h land3_u land_t land4_ri land5_ro land6_o
foreach x of local vars {
    replace `x' = 0 if `x' < 0
}


* Create categorical variables for land size (in acres)
foreach x of local vars {
    gen `x'_cat = .
    replace `x'_cat = 0 if `x' == 0
    replace `x'_cat = 1 if `x' > 0 & `x' <= 0.5
    replace `x'_cat = 2 if `x' > 0.5 & `x' <= 1
    replace `x'_cat = 3 if `x' > 1 & `x' <= 1.5
    replace `x'_cat = 4 if `x' > 1.5 & `x' <= 2
    replace `x'_cat = 5 if `x' > 2
    label define land_cat 0 "0 acre" 1 "0–0.5" 2 "0.5–1" 3 "1–1.5" 4 "1.5–2" 5 "Above 2 acres", replace
    label values `x'_cat land_cat
}

* Apply Inverse hyperbolic sine transformation transformation for land variables

foreach x of local vars{
	gen `x'i = log(`x' + sqrt((`x'^2) + 1))
}

* label Land Data
label var  land1_c  "Owned cultivable land"

label var  land2_h  "Owned homestead land"

label var  land3_u  "Owned fallow land"

label var  land_t  "Total owned land"

label var  land4_ri  "Rented in land"

label var  land5_ro  "Rented out land"

label var  land6_o  "Total operated land"

**# Construct Iv for migration: Leave on Upzilla Out of the District Migration intensity

* Total Household with at least one migrant in the district
egen dis_total_migration = sum(m_iD), by(dist_code)
* Total number of household in the district
bysort dist_code: gen total_hh_dis = _N

* Total Household with at least one migrant in the Upzilla
egen up_total_migration = sum(m_iD), by(id_03_name)
* Total number of household in the Upzilla
bysort id_03_name: gen total_hh_up = _N


*Number of total households in the district except for that upzilla
gen lo_hh_up = total_hh_dis - total_hh_up
* Number of total households with at least one migrant in the District except for that Upzilla
gen lo_mig_up = dis_total_migration - up_total_migration

* Leave on Upzilla Out of the District Migration intensity
gen network_up = lo_mig_up * 100 / lo_hh_up

save "$data_clean\working_main", replace


**#Yields
* Load crop production dataset (Section 7B of household questionnaire)
use "$proj_dir\HH_SEC_7B", clear

* Generate a new variable 'yields' which holds total quantity for a specific crop
gen yields = s7bq04a

* Aggregate total yields at the household level by summing across observations
collapse (sum) yields, by(hhold)

* Create a log-transformed yield variable to normalize distribution and reduce skewness
gen yieldsi = log(yields + sqrt((yields * yields) + 1))

* Sort data by household ID for merging
sort hhold

* Merge the yields data with the main cleaned dataset by household ID
merge 1:1 hhold using "$data_clean\working_main"

* Keep only successfully matched records (present in both datasets)
keep if _merge == 3
drop _merge

* Save the updated main dataset with yield information included
save "$data_clean\working_main", replace




**# Figure2: Generate Histograms for Land Variables (Transformed and Categorical)


* First: Define transformed (IHS) land variables for histogram plotting
local vars land1_ci land2_hi land3_ui land_ti land4_rii land5_roi land6_oi

* Loop over each transformed variable and generate a histogram
foreach x of local vars {
    hist `x', color(emerald) name(g`x'_i, replace)    // Save each histogram with a unique name
}

* Second: Define original land variables to plot their categorical versions
local vars land1_c land2_h land3_u land_t land4_ri land5_ro land6_o

* Loop over each land variable to create histograms of the corresponding categorical version
foreach x of local vars {
    local vlab: variable label `x'   // Extract label for each variable to use as title

    * Generate histogram of `_cat' version of land variable with custom x-axis labels
    hist `x'_cat, ///
        color(emerald) ///
        title("`vlab' (in acre)", size(medium)) ///
        xlabel(0 "0" 1 "0-0.5" 2 "0.5-1" 3 "1-1.5" 4 "1.5-2" 5 "Above 2", angle(horizontal) valuelabel labsize(*.8)) ///
        legend(off) ///
        name(g`x', replace) ///
        xtitle("")
}

* Combine all histograms into a single graph layout
grc1leg gland1_c gland2_h gland3_u gland_t gland4_ri gland5_ro gland6_o, ///
    colfirst iscale(*1.02) rows(3) ysize(11) xsize(10)


* Sort and save final working dataset
sort hhold

* label other variables
label var age "Age of household head (years)" 
label var eduhhead "Years of education of household head (Years)"
label var mwa_memT "No. of working age male member"
label var fwa_memT "No. of working age female member"
save "$data_clean\working_main", replace

**# Analysis
**# Balance Table

* Balance Table Creation with putexcel - Table 1
* Comparing migrant vs non-migrant households


* Set output Excel file
putexcel set "$results\table1_balance_characteristics.xlsx", replace

* Write table title and headers
putexcel A1 = "Table 1: Characteristics of households with and without migrants", txtwrap bold hcenter vcenter
putexcel  (A1:d1),  merge
putexcel A3 = " " B3 = "Migrant-sending households (SD)" C3 = "Other households (SD)" D3 = "Difference (S.E.)", bold colwise overwritefmt hcenter

* Define variable list 
local vars age eduhhead mwa_memT fwa_memT land1_c land2_h land3_u land_t land4_ri land5_ro land6_o


* Loop through variables and run t-tests
local row = 4
local i = 1

foreach var of local vars {
    * Run t-test
    ttest `var', by(m_iD)

    * Collect group statistics
    local mean1 = string(r(mu_1), "%9.3f")
    local sd1   = string(r(sd_1), "%9.3f")
    local mean2 = string(r(mu_2), "%9.3f")
    local sd2   = string(r(sd_2), "%9.3f")
    local diff  = string(r(mu_1) - r(mu_2), "%9.3f")
    local se    = string(r(se), "%9.3f")
    local pval  = r(p)

    * Add significance stars
    local stars = ""
    if `pval' < 0.01 {
        local stars = "***"
    }
    else if `pval' < 0.05 {
        local stars = "**"
    }
    else if `pval' < 0.10 {
        local stars = "*"
    }

    * Write label
    local lab : var lab `var'
    putexcel A`row' = "`lab'", txtwrap bold

    * Write means and standard deviations(next row)
    putexcel B`row' = `mean1', 
    putexcel B`=`row'+1' = "(`sd1')"

    putexcel C`row' = `mean2'
    putexcel C`=`row'+1' = "(`sd2')"

    putexcel D`row' = "`diff'`stars'"
    putexcel D`=`row'+1' = "(`se')"
	
	* Format fonts and center align
	putexcel B`row':D`row', font("Times New Roman", 9)
	putexcel B`=`row'+1':D`=`row'+1', font("Times New Roman", 9)
	
	putexcel B`row':D`row', hcenter
	putexcel B`=`row'+1':D`=`row'+1', hcenter
	
    * Move two rows down for the next variable
    local row = `row' + 2
    local ++i
}

* Add sample sizes at the end
count if m_iD == 1
local N1 = r(N)
count if m_iD == 0
local N2 = r(N)
count
local Ntotal = r(N)

putexcel A`row' = "Observation"
putexcel B`row' = `N1', hcenter
putexcel C`row' = `N2', hcenter
putexcel D`row' = `Ntotal', hcenter

* Add borders to headers
putexcel A3:D3, border(top)
putexcel A3:D3, border(bottom)
putexcel A`row':D`row', border(bottom)


* Add footnote
putexcel A`=`row'+1' = "*p<0.10, **p<0.05, ***p<0.01"


* Adjust Excel column width using Mata

mata
	
	b = xl()
	
	b.load_book("$results\table1_balance_characteristics.xlsx")
	
	b.set_column_width(1,1,45) 
	
	b.set_column_width(2,4,30) 
	end





**# Table 2: Main Regression result 

* IV Regression: Effect of Migration on Land Outcomes


* Define list of outcome variables (both raw and inverse hyperbolic sine transformed)
local vars land1_c land1_ci land2_h land2_hi land3_u land3_ui ///
           land_t land_ti land4_ri land4_rii land5_ro land5_roi ///
           land6_o land6_oi

* Clean up any previously saved output files (suppress errors if files don't exist)
cap noisily erase "D:\landholding\migration and land renting\Tamim\IV_sub_lo_log.xls"
cap noisily erase "D:\landholding\migration and land renting\Tamim\IV_sub_lo_log.txt"

* Loop over each outcome variable
foreach x of local vars {
    
    * Estimate two-stage least squares (2SLS) using ivreg2
    * Instrument: network_up → endogenous variable: m_iD → outcome: `x'
    * Control variables: household demographics and division fixed effects
    ivreg2 `x' (m_iD = network_up) ruralD agehhead sexhhead eduhhead mwa_memT fwa_memT i.div_code, cluster(dist_code)

    * Kleibergen-Paap rk Wald F statistic due to clustered error
    local fstat = e(rkf)

    * Export IV regression results for `x' to Excel using outreg2
    outreg2 using "D:\landholding\migration and land renting\Tamim\IV_sub_lo_log.xls", ///
        keep(m_iD) append ctitle("`x'") nocons nor2 ///
        addstat(F Statistic, `fstat') wide
}



**# Figure 3 : Plausibly Exogenous Instruments robustness check)

* In this analysis, exclusion criteria would be relaxed (a certain portion of the reduced form coefficient would be assumed to affect the oputcome variable through other channel than the variable of interest). Using plausexog, After allowing for violation, we will get lower bound and upper bound of the treatment variable.
*  I would like to store the upper and lower bound from the plausibly exogenous analysis (Union of confidence interval approach) for each variable and assumed degree of violation of exclution criteria (common support of the degree of violation of exclution criteria). 

gen u_bound=.
gen l_bound=.
gen var_name = ""
gen number= .



* Define the list of outcome variables (IHS-transformed land outcomes)


local d=1
local vars "land1_ci  land2_hi  land3_ui  land_ti  land4_rii  land5_roi land6_oi"

* Here, gmin(0) gmax(0) means no violation of the exclusion criteria
* Since the numlist does not allow to start from 0, I perform two loop, first for the 0 and then 0.05 to 1 with an interval of 0.05

*Installing plausexog package

cap ssc install plausexog

foreach x in `vars' {

    * Run plausexog with delta = 0 (baseline IV)
    plausexog uci `x' i.div_code ruralD agehhead sexhhead eduhhead mwa_memT fwa_memT ///
        (m_iD = network_up), vce(cluster dist_code_hies) gmin(0) gmax(0)

    * Store lower and upper bounds of m_iD effect
    replace l_bound = e(lb_m_iD) in `d'
    replace u_bound = e(ub_m_iD) in `d'
    replace number = 0 in `d'
    replace var_name = "`x'" in `d'
    
    local d = `d' + 1

    * Loop over delta values from 0.05 to 1.00 in steps of 0.05
    foreach y of numlist 0.05(0.05)1.00 {
        
        * Re-run the reduced form regression to get the beta coefficient of the network_up (for delta scaling)
        areg `x' network_up ruralD agehhead sexhhead eduhhead mwa_memT fwa_memT, ///
            absorb(div_code) cluster(dist_code_hies)

        * Calculate the common support, gmin and gmax as ±(delta × beta_hat)
        local i =  `y' * _b[network_up]
        local j = -`y' * _b[network_up]

        * Run plausexog with delta ≠ 0
        plausexog uci `x' i.div_code ruralD agehhead sexhhead eduhhead mwa_memT fwa_memT ///
            (m_iD = network_up), vce(cluster dist_code_hies) gmin(`i') gmax(`j')

        * Store results
        replace var_name = "`x'" in `d'
        replace l_bound = e(lb_m_iD) in `d'
        replace u_bound = e(ub_m_iD) in `d'
        replace number = `y' in `d'

        local d = `d' + 1
    }
}


**#Figure 4: 95% confidence interval for Plausibly exogenous analysis


* Visualization: Bounds Plot by Delta for Each Outcome Var


* Define variable names and custom graph titles
local vars land1_ci land2_hi land3_ui land_ti land4_rii land5_roi land6_oi

* Initialize empty local for collecting graph names
local graphnames

* Set special y-axis range for the last graph
local special_y land6_oi

* Create twoway plots in loop
local i = 1
foreach var of local vars {
	local lab : var lab `var'
    local title : `lab'
    local gname = g`i'
    local graphnames `graphnames' `gname'

    * Construct the plot
    twoway (line u_bound number if var_name == "`var'", lc(black)) ///
           (line l_bound number if var_name == "`var'", lc(blue)), ///
           yline(0, lwidth(thin) lcolor(red) lpattern(dash)) ///
           xlabel(0(0.10)1) ///
           xtitle("Delta") ///
           title("`title'") ///
           name(`gname', replace) ///
           ///
           `=cond("`var'" == "`special_y'", "yscale(range(-6 2)) ylabel(-6(2)2)", "")' ///
           ///
           legend(order(1 "Upper bound" 2 "Lower Bound") row(1) pos(6))

    local ++i
}

* Combine all graphs into a grid layout
grc1leg `graphnames', ///
    colfirst ///
    iscale(*1.02) ///
    rows(3) ///
    legendfrom(g7) ///
    ysize(11) ///
    imargin(0 0 0 0)

* Export the panel figure
graph export "$figure\plausexog_significant.png", replace



**#Falsification test


*In this robustness check, I will randomly assign migration status to households while ensuring that the total number of households with at least one migrant in each Upazila remains the same as in the original dataset. This means that the total number of households with at least one migrant in each district, as well as the instrumental variable (migration intensity, excluding one Upazila from the district), will be preserved.

*The rationale behind this test is that network migration should affect landholding only through actual migration. If, after iterating this randomization process 1,000 times, we observe coefficients concentrated around a non-zero value when instrumenting for the pseudo migration status, this may indicate a violation of the exclusion restriction. In other words, the network effect on landholding may be operating through channels other than migration.

gen  pseudo_beta =0 //I will store pseudo beta of each iteration in this variable


local vars "land1_ci  land2_hi  land3_ui  land_ti  land4_rii  land5_roi land6_oi"

foreach var of local vars{
			forvalues i = 1/1000 {
			   * Randomly assign pseudo migration status maintaining the leave one up out of the district network value 
				set seed `i'
				di `i'
				sort dist_code id_03_name hhold, stable
				gen rand = runiform()  // Generate random numbers
				sort rand // Sort by random values
				bys id_03_name: gen pseudo_migration_status = 1 if _n <= up_total_migration   // Assign migration status randomly
				replace pseudo_migration_status =0 if pseudo_migration_status==.
				drop rand  // Clean up
				gen  pseudo_beta_`var' =0 //I will store pseudo beta of each iteration in this variable

				ivreg2 `var' (pseudo_migration_status=network_up) ruralD agehhead sexhhead eduhhead mwa_memT fwa_memT  i.div_code, cluster(dist_code)
				di e(b)[1,1]
				* Store the treatment effect
				replace pseudo_beta_`var'= e(b)[1,1] in `i'
				sum pseudo_migration_status
				drop pseudo_migration_status
		}
}



**# Figure 5: Plot Falsification test result


* Define variable list and matching plot titles
local vars land1_ci land2_hi land3_ui land_ti land4_rii land5_roi land6_oi

* Empty local for graph names
local fals_graphs

* Loop through variables and generate individual plots
local i = 1
foreach var of local vars {
    local lab : var lab `var'
    local title : `lab'
    local gname = f`i'
    local fals_graphs `fals_graphs' `gname'

    * Plot the histogram + density
    twoway (histogram pseudo_beta_`var', color(blue%40) bin(20)) ///
           (kdensity pseudo_beta_`var', color(red) lwidth(medium)), ///
           title("`title'") ///
           legend(label(1 "Histogram") label(2 "Kernel Density")) ///
           name(`gname', replace)

    local ++i
}

* Combine all falsification plots into a panel layout
grc1leg `fals_graphs', ///
    colfirst ///
    rows(3) ///
    iscale(*1.05) ///
    ysize(10) ///
    imargin(0 0 0 0) ///
    legendfrom(f7)

* Export the panel plot
graph export "$figure\falsification_pseudo_effects.png", replace

