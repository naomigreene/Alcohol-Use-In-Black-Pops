/*
	Postdoc Project: Alcohol Use among Black Populations Descriptive Study
	Program Name: AUBP_Program01
	Date Created: April 28, 2021
	Date Updated: July 28, 2021
	Author: Naomi Greene
	
	This program provides to total analyses for this descriptive study. The purpose of 	
	this study is to describe alcohol use among Black females and males across U.S. 
	states and regions.
	
	NOTE about Updated on July 28, 2021: The age variables were not originally save when 
	this data was cleanned and exported into R for analysis. The updated program includes 
	age variable to complete the age analysis questions. 
	
	NOTE about Updated on October 20, 2021: I had not realized that there was a _sex 	
	variable that captures sex at birth. However, only 7 states used variable. I'm just 
	curious what the numbers would have looked like if we used the _sex var 
	
*/

/*	SECTION A: Behavioral Risk Factor Surveillance Data (BRFSS) 2019

	This data can be downloaded from 	
	https://www.cdc.gov/brfss/annual_data/annual_2019.html along with with the codebook,
	metadata, weighting information, and information on optional module use. 
	
	Raw Data File: 2019 BRFSS Data (SAS Transport Format)
	
	The raw data file for 2019 BRFSS Data was downloaded on April 28, 2021.
*/

*** Import raw BRFSS dataset from SAS XPORT file
import sasxport5 "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\raw data\brfss 2019\LLCP2019.XPT", clear

*** Keep the variables needed for this project's analysis.
keep _state _psu sexvar _sex alcday5 avedrnk3 drnk3ge5 maxdrnks _ststr _strwt _imprace _llcpwt2 _llcpwt _race drnkany5 drocdy3_ _rfbing5 _drnkwk1 _rfdrhv7 _age80 _age_g

/*
	NOTE: There are multiple race and ethinicity varialbes in the BRFSS. The 		
	variable choosen here includes all people in the dataset and includes category 	 
	for race and ethnicity. There will be missing data in this variable. 
*/

*** Codebook log
log using "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\codebook_brfss_2019_varsubset.log", replace

codebook, header

log close

/*
	SECTION B: Data Cleaning
*/


*** Compare sexvar with _sex
tabulate sexvar _sex

*** CLEAN VARIABLES

*** Sex
label define sexvar_l 1 "Male" 2 "Female"
label values sexvar sexvar_l

label values _sex sexvar_1

*** Number of binge drinking occasions (drnk3ge5)
recode drnk3ge5 88 = 0 77 = . 99 = .

*** Past 30-day drinking (drnkany5)
recode drnkany5 7 = .a 9 = .b
label define drnkany5_l 1 "Yes" 2 "No" .a "Dont Know/Not Sure" .b "Refused"
label values drnkany5 drnkany5_l

*** Number of drink occasions per day (drocdy3_)
recode drocdy3_ 900 = .

*** Past-30 day binge drinking (_rfbing5)
recode _rfbing5 9 = .a
label define _rfbing5_l 1 "Yes" 2 "No" .a "Dont Know/Refused/Missing" 
label values _rfbing5 _rfbing5_l

*** Past-30 day heavy drinking (_rfdrhv7)
recode _rfdrhv7 9 = .a
label define _rfdrhv7_l 1 "Yes" 2 "No" .a "Dont Know/Refused/Missing" 
label values _rfdrhv7 _rfdrhv7_l




*** CALCULATE NEW VARIABLES

*** Age group
* Added 7/30/2021
* Combine age groups for age standardization

/*
	_age_g variable - groups
	
	1: 18-24 years
	2: 25-34 years
	3: 35-44 years
	4: 45-54 years
	5: 55-64 years
	6: 65 years and older
	
	age4cat
	
	1: 18-24 years
	2: 25-44 years
	3: 45-64 years
	4: 65 years and over
	
*/

generate age4cat = _age_g
recode age4cat 1 = 1 2/3 = 2 4/5 = 3 6 = 4
label define age4cat_l 1 "18-24 years" 2 "25-44 years" 3 "45-64 years" 4 "65 years and over"
label values age4cat age4cat_l

*** Create two variables from _state: fips, state 
generate fips = _state

label variable fips "State FIPS Code"

generate state = _state 

label define state_l 1 "Alabama" 2 "Alaska" 4 "Arizona" 5 "Arkansas" 6 "California" 8 "Colorado" 9 "Connecticut" 10 "Delaware" 11 "District of Columbia" 12 "Florida" 13 "Georgia" 15 "Hawaii" 16 "Idaho" 17 "Illinois" 18 "Indiana" 19 "Iowa" 20 "Kansas" 21 "Kentucky" 22 "Louisiana" 23 "Maine" 24 "Maryland" 25 "Massachusetts" 26 "Michigan" 27 "Minnesota" 28 "Mississippi" 29 "Missouri" 30 "Montana" 31 "Nebraska" 32 "Nevada" 33 "New Hampshire" 34 "New Jersey" 35 "New Mexico" 36 "New York" 37 "North Carolina" 38 "North Dakota" 39 "Ohio" 40 "Oklahoma" 41 "Oregon" 42 "Pennsylvania" 44 "Rhode Island" 45 "South Carolina" 46 "South Dakota" 47 "Tennessee" 48 "Texas" 49 "Utah" 50 "Vermont" 51 "Virginia" 53 "Washington" 54 "West Virginia" 55 "Wisconsin" 56 "Wyoming" 66 "Guam" 72 "Puerto Rico"

label values state state_l

label variable state "State name"

*** Drop original Fips code variable
drop _state


*** Create region variable
generate region = .

*** Northeast Region: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, Vermont, New Jersey, New York, Pennsylvania
replace region = 1 if fips == 9 | fips == 23 | fips == 25 | fips == 33 | fips == 44 | fips == 50 | fips == 34 | fips == 36 | fips == 42

*** Midwest Region: Indiana, Illinois, Michigan, Ohio, Wisconsin, Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, South Dakota
replace region = 2 if fips == 18 | fips == 17 | fips == 26 | fips == 39 | fips == 55 | fips == 19 | fips == 20 | fips == 27 | fips == 29 | fips == 31 | fips == 38 | fips == 46

*** South Region: Delaware, District of Columbia, Florida, Georgia, Maryland, North Carolina, South Carolina, Virginia, West Virginia, Alabama, Kentucky, Mississippi, Tennessee, Arkansas, Louisiana, Oklahoma, Texas
replace region = 3 if fips == 10 | fips == 11 | fips == 12 | fips == 13 | fips == 24 | fips == 37 | fips == 45 | fips == 51 | fips == 54 | fips == 1 | fips == 21 | fips == 28 | fips == 47 | fips == 5 | fips == 22 | fips == 40 | fips == 48

*** West Region: Arizona, Colorado, Idaho, New Mexico, Montana, Utah, Nevada, Wyoming, Alaska, California, Hawaii, Oregon, Washington
replace region = 4 if fips == 4 | fips == 8 | fips == 16 | fips == 35 | fips == 30 | fips == 49 | fips == 32 | fips == 56 | fips == 2 | fips == 6 | fips == 15 | fips == 41 | fips == 53

label define region_l 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label values region region_l
label variable region "US Census Region"

*** Binary sex variables
generate female = sexvar
recode female 2 = 1 1 = 0
label define female_l 0 "Male" 1 "Female"
label values female female_l
label variable female "Female indicator"

generate male = sexvar
recode male 1 = 1 2 = 0
label define male_l 0 "Female" 1 "Male"
label values male male_l
label variable male "Male indicator"

*** Binary race variable
generate black = _race
recode black 2 = 1 1 = 0 3 = . 4 = . 5 = . 6 = . 7 = . 8 = . 9 = .
label define black_lbl 0 "White" 1 "Black"
label values black black_lbl
label variable black "Black race"

*** Binge Drinker
*** Note: Denominator only includes people who are current drinkers
generate binge_current = .
/*Binge Drinkers among Current Drinkers*/
replace binge_current = 1 if drnkany5 == 1 & _rfbing5 == 2	
/*Not Binge Drinkers among Current Drinkers*/	
replace binge_current = 2 if drnkany5 == 1 & _rfbing5 == 1	
/*Not Current Drinkers*/	
replace binge_current = .a if drnkany5 == 2	
/*Answered DK or Not Sure for drinking any alcoholic beverage*/					
replace binge_current = .b if drnkany5 == .a
/*Refused or missing for drinking any alcoholic beverage*/	
replace binge_current = .c if drnkany5 == .b 
/* Valid current drinking answer, but are missing binge drinking answer*/
replace binge_current = .d if _rfbing5 == .b	
label variable binge_current "Binge Drinking among Current Drinkers"			
label define binge_current_l 1 "Yes" 2 "No" .a "Non-Drinker" .b "Current Drinking Not Sure" .c "Current Drinking Refused" .d "Binge Drinking Missing"
label values binge_current binge_current_l

/*
NOTE: This is a binary version of the above variable.

generate binge_current_bin = binge_current
recode binge_current_bin 1 = 1 2 = 0
label variable binge_current_bin "Binge Drinking among Current Drinkers (1/0)"
label values binge_current_bin yesno10
*/

*** Heavy Drinker
*** Note: Denominator only includes people who are current drinkers
generate hvy_current = .
/*Heavy Drinkers among Current Drinkers*/
replace hvy_current = 1 if drnkany5 == 1 & _rfdrhv7 == 2
/*Not Heavy Drinkers among Current Drinkers*/
replace hvy_current = 2 if drnkany5 == 1 & _rfdrhv7 == 1
/*Not Current Drinkers*/
replace hvy_current = .a if drnkany5 == 2
/*Answered DK or Not Sure for drinking any alcoholic beverage*/
replace hvy_current = .b if drnkany5 == .a
/*Refused or missing for drinking any alcoholic beverage*/
replace hvy_current = .c if drnkany5 == .b
/*Valid current drinking answer, but are missing heavy drinking answer*/
replace hvy_current = .d if drnkany5 == 1 & _rfdrhv7 == .b
label variable hvy_current "Heavy Drinking among Current Drinkers"
label define hvy_current_l 1 "Yes" 2 "No" .a "Non-Drinker" .b "Current Drinking Not Sure" .c "Current Drinking Refused" .d "Heavy Drinking Missing"
label values hvy_current hvy_current_l

/*
NOTE: This is a binary version of the above variable.

generate hvy_current_bin = hvy_current
recode hvy_current_bin 1=1 2=0 
label variable hvy_current_bin "Heavy Drinking among Current Drinkers (1/0)"
label values hvy_current_bin yesno
*/

/*Number of drink occasions per day among current drinkers in the past 30 days*/
generate drnk_occ = drocdy3_
replace drnk_occ = . if drnkany5 > 1
label variable drnk_occ "Drink Occasions per Day among Current Drinkers"

/*Number of binge episodes among current drinking in the past 30 days*/
generate bing_occ = drnk3ge5
replace bing_occ = . if drnkany5 > 1
label variable bing_occ "Past 30-day Binge Epidoses among Current Drinkers"

/* Average number of drinks among current drinkers in the past 30 days*/
/* Create a new variable equal to the old variable that can be cleanned. Do not touch the original variable*/
/* This question was only asked among people who are current drinkers (avedrnk3)*/
generate avedrnks = avedrnk3
replace avedrnks = . if avedrnk3 == 88 | avedrnk3 == 77 | avedrnk3 == 99
label variable avedrnks "Average Drinks in Past 30-days among Current Drinkers"


*** Drop observations for Washington, D.C., Guam and Puerto Rico
drop if fips == 11 | fips > 60

*** Sort by fips code
sort fips

*** Reorder variables with fips and state first in the dataset
order fips state region, first

*** Name dataset
label data "BRFSS 2019 Subset Clean 2021-07-30"

*** Save dataset
save "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\brfss2019_clean.dta", replace

clear

/*
	SECTION C: Codebook for cleanned dataset
*/

clear 

use "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\brfss2019_clean.dta"

log using "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\codebook_brfss2019_clean.log"

codebook, header

codebook, problems

log close

clear





log using "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\analyses_20210501.log", name(analyses) replace

/*
	SECTION D: Analyses
*/


/*
	BINARY OUTCOMES
*/

use "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\brfss2019_clean.dta"

*** Set survey design
svyset _psu, strata(_ststr) weight(_llcpwt) vce(linearized) singleunit(centered)

*** STATE LEVEL ANALYSES 

*** Unweighted state sample size for each population group
tabulate state sexvar if black == 1

*** Binary Outcomes: drnkany5, binge_current, hvy_current  

*** Black Women
foreach var of varlist drnkany5 binge_current hvy_current {
    svy linearized, subpop(if black == 1 & female == 1): tabulate state `var', row ci obs percent
}


*** Black Men
foreach var of varlist drnkany5 binge_current hvy_current {
    svy linearized, subpop(if black == 1 & male == 1): tabulate state `var', row ci obs percent
}
 
 
*** REGION LEVEL ANALYSES

*** Unweighted region sample size for each population group
tabulate region sexvar if black == 1 

*** Binary Outcomes: drnkany5, binge_current, hvy_current

*** Black Women
foreach var of varlist drnkany5 binge_current hvy_current {
    svy linearized, subpop(if black == 1 & female == 1): tabulate region `var', row ci obs percent
}

*** Black Men
foreach var of varlist drnkany5 binge_current hvy_current {
    svy linearized, subpop(if black == 1 & male == 1): tabulate region `var', row ci obs percent
}

/*

	Note: Given the difficulty of interpreting the drnk_occ measure, I have decided 
	not to use it. Instead, I will use the average drinks per day in the past 30 	
	days. Also, given the skewed nature of the bing_occ measure, I think 
	I will analyze as a categorical variable. See separate analyses below. 


*/

log off analyses

/*
	DRINKING FREQUENCY: CONTINUOUS -> CATEGORICAL OUTCOMES
	
	The information about the number of times per day that an individual drinks 	
	and the number of binge occassions per month is heavily skewed (See graphs 
	below). Therefore, I think it makes more sense to categorize these variables 	
	rather than presenting means. 
*/

*** Summary Statistics (unweighted) for Average Drinks and Binge Episodes in the Past 30-days
summarize avedrnks, detail
tab avedrnks
svy: tab avedrnks, percent

by state, sort : summarize avedrnks if black == 1 & female == 1

summarize bing_occ, detail
tab bing_occ
svy: tab bing_occ, percent
svy, subpop(if black == 1 & female == 1): tab bing_occ, percent


*** Histograms for Average Drinks and Binge Episodes (Unweighted)
histogram avedrnks, frequency normal title(Average Drinks per Day) name(avedrnks)
histogram bing_occ, frequency normal title(Past 30-day Binge Episodes among Current Drinkers) name(bing_occ) 

*** Create a categorical variable for average drinks in past 30 days among current drinkers
generate avedrnkscat = .
replace avedrnkscat = 1 if avedrnks == 1
replace avedrnkscat = 2 if avedrnks == 2
replace avedrnkscat = 3 if avedrnks == 3
replace avedrnkscat = 4 if avedrnks >= 4 & avedrnks < .
label define avedrnkscat_l 1 "1 drink" 2 "2 drinks" 3 "3 drinks" 4 "4 or more"
label values avedrnkscat avedrnkscat_l
label variable avedrnkscat "Avg Drinks per Day Past 30-d Cat"

*** Create a categorical variable for binge drinking episodes in past 30 days
generate bingeocc_cat = .
replace bingeocc_cat = 0 if bing_occ == 0					/* 0 times */
replace bingeocc_cat = 1 if bing_occ >= 1 & bing_occ <= 2 	/* 1-2 times */
replace bingeocc_cat = 2 if bing_occ >= 3 & bing_occ <= 4 	/* 3-4 times */
replace bingeocc_cat = 3 if bing_occ >= 5 & bing_occ < .	/* 5 or more times */
label define bing_occ_l 0 "0 times" 1 "1-2 times" 2 "3-4 times" 3 "5 or more"
label values bingeocc_cat bing_occ_l
label variable bingeocc_cat "Binge Drinking Occasions Past-30d Cat"
 


log on analyses

*** CATEGORICAL OUTCOMES: avedrnkscat bing_occ 

*** STATE 

*** Black Women
foreach var of varlist avedrnkscat bingeocc_cat {
    svy linearized, subpop(if black == 1 & female == 1): tabulate state `var', row ci obs percent
}

*** Black Men
foreach var of varlist avedrnkscat bingeocc_cat {
    svy linearized, subpop(if black == 1 & male == 1): tabulate state `var', row ci obs percent
}


*** REGION

*** Black Women
foreach var of varlist avedrnkscat bingeocc_cat {
    svy linearized, subpop(if black == 1 & female == 1): tabulate region `var', row ci obs percent
}

*** Black Men
foreach var of varlist avedrnkscat bingeocc_cat {
    svy linearized, subpop(if black == 1 & male == 1): tabulate region `var', row ci obs percent
}

log close analyses

*** Save dataset
save "C:\Users\18166\Dropbox\JHSPH\Post Doc\Alcohol Use Black Pops\output\brfss2019_clean.dta", replace