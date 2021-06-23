*global resilience anlaysis, all countries, 60 years, multiple crops*
*methods used - Superposed epoch analysis (SEA), Survival analysis, Machine learning (Random forest model) 

**processing raw FAO files**
clear all
import delimited "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020\Maize Data.csv", varnames(1) clear
drop domain areacode elementcode element unit flag flagdescription
merge m:m country using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\FAO-Names-ISO-Match.dta" 
drop _merge
**correcting country names pre-post split**
replace country = "Russian Federation" if country == "USSR"
replace country = "Czechoslovakia" if country == "Czechia"
replace country = "Ethiopia" if country == "Ethiopia PDR"
replace country = "Belgium-Luxembourg" if country == "Belgium"
replace country = "Sudan (former)" if country == "South Sudan"
replace country = "The former Yugoslav Republic of Macedonia" if country == "Yugoslav SFR"
replace country = "Serbia and Montenegro" if country == "Serbia"
replace country = "Micronesia (Federated States of)" if country == "Pacific Islands Trust Territory"
replace country = "The former Yugoslav Republic of Macedonia" if country == "North Macedonia"
drop if country == "Solomon Islands" | country == "China, Hong Kong SAR" | country == "Sudan"| country == "Montenegro" | country == "Luxembourg" | country == "Saint Lucia" | country == "China, Taiwan Province of" | country == "China, mainland"  
sort item country year
egen countrycode= group( country)
 
**generating yield dips**
by item country, sort: gen yielddip= yield < (yield[_n-1] + yield[_n-2] + yield[_n-3])/3
replace yielddip = 0 if year == 1961
replace yielddip = 0 if year == 1962
replace yielddip = 0 if year == 1963

**checking countries not starting at 1961, and removing yield dips at start year**
gen trial1 = 1 if country != country[_n-1] & year != 1961 & yielddip == 1
replace trial1 = 1 if country != country[_n-2] & year != 1961 & yielddip == 1
replace trial1 = 1 if country != country[_n-3] & year != 1961 & yielddip == 1
replace yielddip = 0 if trial1 == 1
drop trial1

**Calculating spells of dips and moving average**
**check if analysis is by country or cropwise-edit formula accordingly**
egen countryid= group( country)
tsset countryid year, yearly
tssmooth ma ma2 = yield, window(3 0 0)
gen iden= 1 if yielddip==1
replace ma2 = . if iden==.

**calculating normalized recovery and loss**
gen normalizedloss= yield/ma2 if iden ==1
gen normalizedrecovery= yield[_n+1]/ma2 if iden ==1
gen rateofrecovery= 1 if yield[_n+1] >= ma2 & iden ==1
replace rateofrecovery= 2 if yield[_n+2] >= ma2 & iden ==1 & rateofrecovery == .
replace rateofrecovery= 3 if yield[_n+3] >= ma2 & iden ==1 & rateofrecovery == .
replace rateofrecovery= 4 if yield[_n+4] >= ma2 & iden ==1 & rateofrecovery == .
replace rateofrecovery= 5 if iden ==1 & rateofrecovery == .
drop ma2 iden

**check if dips left**
count if yielddip == 1
count if normalizedrecovery != .
gen iden = 1 if yielddip == 1 & normalizedrecovery == .
replace iden = . if year == 2018 & iden ==1
count if iden == 1
**check iden values manually**
drop iden
**check for inconsistencies**
replace normalizedrecovery = . if year ==  2018 
replace normalizedloss = . if year ==  2018 
replace rateofrecovery = . if year ==  2018 
gen iden = 1 if normalizedloss > 1 & normalizedloss != .
*replace very high recovery values*
replace normalizedrecovery = 2 if normalizedrecovery > 2
replace normalizedrecovery = . if normalizedloss == .
*manually replace values where years are not continuous*
replace rateofrecovery = . if normalizedrecovery == .
replace normalizedrecovery = . if  normalizedloss == .

**check if countries not ending in 2018**
drop iden
gen iden = 1 if countrycode != countrycode[_n+1] & year != 2018
replace normalizedloss = . if iden ==1
replace normalizedrecovery = . if iden ==1
replace rateofrecovery = . if iden ==1
drop iden

**Merge other data**
merge m:m ISOCodes using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Country-ISO-WBRegions.dta"
drop if _merge == 2
sort item country year
drop _merge
merge m:m ISOCodes year using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Disaster-Information.dta"
drop if _merge == 2
sort item country year
drop _merge
rename rateofrecovery Lengthofrecovery
drop occurrence totaldeaths injured affected homeless totalaffected totaldamage
merge m:m ISOCodes using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\New Maize Analysis\Conflict-code file.dta"
drop if _merge == 2
drop _merge
sort item country year
merge m:m gwdcodes year using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\New Maize Analysis\ucdp-prio-acd-191.dta\UcdpPrioConflict_v19_1.dta"
gen conflictcode =1 if conflict_id != .
drop if _merge == 2
drop _merge
sort item country year
drop  itemcode yearcode gwdcodes countryname location side_a side_a_id side_a_2nd side_b side_b_id side_b_2nd incompatibility territory_name cumulative_intensity start_date start_prec start_date2 start_prec2 ep_end ep_end_date ep_end_prec gwno_a_2nd gwno_b gwno_b_2nd gwno_loc version
save "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020

**calculate frequencies and rates**
egen dipfreq = count( yielddip ) if yielddip == 1 , by( item country )
gen fullrecovery= 1 if normalizedrecovery > normalizedloss
egen fullrecovfreq = count( fullrecovery ) if fullrecovery == 1 , by( item country )
gen recoveryrate = (fullrecovfreq/dipfreq)*100
egen LORfreq1 = count(Lengthofrecovery ) if Lengthofrecovery == 1 , by( item country )
egen LORfreq2 = count(Lengthofrecovery ) if Lengthofrecovery == 2 , by( item country )
egen LORfreq3 = count(Lengthofrecovery ) if Lengthofrecovery == 3 , by( item country )
egen LORfreq4 = count(Lengthofrecovery ) if Lengthofrecovery == 4 , by( item country )
egen LORfreq5 = count(Lengthofrecovery ) if Lengthofrecovery == 5 , by( item country )
egen totlLORfreq= count(Lengthofrecovery ) if Lengthofrecovery != .  , by( item country )
**SAVE DATA**
save "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020\Combinedcrops_Recovery.dta", replace
collapse(mean) normalizedloss normalizedrecovery Lengthofrecovery dipfreq fullrecovery fullrecovfreq recoveryrate LORfreq1 LORfreq2 LORfreq3 LORfreq4 LORfreq5 totlLORfreq , by ( item country ISOCodes )
export excel using "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020\Allmainvariables.xls", firstrow(variables)
**DONT SAVE!!!!! MAKE SURE YOU DONT SAVE THE FILE-ALL DATA WILL BE LOST**
use "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020\Combinedcrops_Recovery.dta", clear
save "C:\Users\S.VYAS\Desktop\CIMMYT\Work\Recovery\Revised Working Folder-November 2018\Sept_2020\Combinedcrops_Recovery.dta", replace

*Survival analysis*
stset Lengthofrecovery, failure(normalizedrecovery)
sts test item , logrank
sts graph, by( item)
gen timeperiod = 1 if year >=1960 & year < 1970
replace timeperiod = 2 if year >=1970 & year < 1980
replace timeperiod = 3 if year >=1980 & year < 1990
replace timeperiod = 4 if year >=1990 & year < 2000
replace timeperiod = 5 if year >=2000 & year < 2010
replace timeperiod = 6 if year >=2010
sts test timeperiod , logrank
sts graph, by( timeperiod )
replace conflictcode = 0 if conflictcode == .
sts test conflictcode , logrank
sts graph, by( conflictcode )
egen incomeg = group(incomegroup)
replace incomegroup = "" if incomeg == 4
sts test incomegroup , logrank
sts graph, by( incomegroup )
sts test maindisastertype , logrank
sts graph, by( maindisastertype )
egen grpmaindisastertype = group( maindisastertype)
*Cox proportional hazards model to calculate hazard ratios for each region*
stcox i.grpmaindisastertype

**Random forest model**
*make sure data copy is saved- some data will be dropped in next step*
keep if normalizedrecovery != .
egen disastergroup = group( maindisastertype)
egen cropgroup = group(item)
egen regiongroup = group(region)
set seed 1
gen u = uniform()
sort u
rforest normalizedrecovery normalizedloss year conflictcode disastergroup cropgroup regiongroup , type(reg) iter(500)
ereturn list
predict p1
list p1 normalizedrecovery in 1/5
ereturn list
*see out of bag error and RMSE*
matrix importance = e(importance)
svmat importance
list importance in 1/5
gen id=""
local mynames : rownames importance
 local k : word count `mynames'
if `k'>_N {
   set obs `k'
  }
 forvalues i = 1(1)`k' {
local aword : word `i' of `mynames'
 local alabel : variable label `aword'
if ("`alabel'"!="") qui replace id= "`alabel'" in `i'
  else qui replace id= "`aword'" in `i'
 }
  graph bar (mean) importance, over(id) ytitle(Importance)
