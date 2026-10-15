version 18.0
clear all
set more off
set maxvar 10000
unicode encoding set utf8

* Data construction, stage 3: high-tech share + recorded crime + city statistics
* -> city-year panels (v02, v03). Construction only; no estimation.
* Set the project directory before running, e.g.  global root "path/to/project"
* Expected layout: $root/02_data/raw (read-only inputs) and $root/02_data/analysis_ready.
if `"$root"' == "" {
    display as error "Set global root to the local project directory first."
    exit 198
}
global raw  "$root/02_data/raw"
global out  "$root/02_data/analysis_ready"
global diag "$root/05_outputs/diagnostics"
global tax  "$raw/Tax_inves_data"
global treatment "$out/treatment_city_year_v01.dta"
global citydata "$raw/1999～2023年中国城市统计年鉴面板数据整理结果/1999～2023年中国城市统计年鉴面板数据整理结果/1998～2022年中国城市统计年鉴地级市面板数据.dta"

confirm file "$treatment"
capture mkdir "$out"
capture mkdir "$root/05_outputs"
capture mkdir "$diag"

tempfile crime cityfull legacy_processed sample_flow_summary sample_flow_dist urbanpop cpi

* -----------------------------------------------------------------------------
* 1. Crime and city-panel inputs; merges and winsorization in a fixed order.
* -----------------------------------------------------------------------------
use "$raw/crime_data.dta", clear
rename city city_code
keep if inrange(year,2007,2020)
isid city_code year
keep city_code year total ms_total xs_total xz_total
save `crime', replace

use 省 市 省代码 市代码 年份 年末户籍人口_万人_全市 第二产业占地区生产总值的比重_全市 城镇登记失业人员数_人_全市 地方一般公共预算支出_万元_全市 人均地区生产总值_元_全市 using "$citydata", clear
rename 市代码 city_code
rename 年份 year
isid city_code year
rename 年末户籍人口_万人_全市 pop_registered_10k
rename 第二产业占地区生产总值的比重_全市 secondary_source
rename 城镇登记失业人员数_人_全市 unemployment_count
rename 地方一般公共预算支出_万元_全市 gov_expenditure_10k
rename 人均地区生产总值_元_全市 gdp_per_capita_yuan
save `cityfull', replace

* Keep treatment-crime matches first, then merge the full city panel.
use "$treatment", clear
merge 1:1 city_code year using `crime', keep(match) nogen
isid city_code year
save `legacy_processed', replace
merge 1:1 city_code year using `cityfull'
drop _merge

gen double crime_rate_10k_raw=xs_total/pop_registered_10k if pop_registered_10k>0
gen double secondary_ind_candidate=secondary_source
replace secondary_ind_candidate=secondary_ind_candidate/100 if secondary_ind_candidate>1 & !missing(secondary_ind_candidate)
gen double ln_unemp_candidate=ln(unemployment_count+1) if unemployment_count>=0
gen double ln_gov_expen_candidate=ln(gov_expenditure_10k+1) if gov_expenditure_10k>=0
gen double ln_gdp_per_candidate=ln(gdp_per_capita_yuan+1) if gdp_per_capita_yuan>=0
gen byte candidate_low_gdp=(ln_gdp_per_candidate<5) if !missing(ln_gdp_per_candidate)
gen byte legacy_drop_sansha=(city_code==460300)

preserve
    drop if legacy_drop_sansha
    gen double crime_rate_10k=crime_rate_10k_raw
    gen double secondary_ind=secondary_ind_candidate
    gen double ln_unemp=ln_unemp_candidate
    gen double ln_gov_expen=ln_gov_expen_candidate
    gen double ln_gdp_per=ln_gdp_per_candidate
    replace ln_unemp=. if ln_unemp==0
    replace ln_gdp_per=. if ln_gdp_per<5
    winsor2 ln_unemp ln_gdp_per secondary_ind crime_rate_10k, cut(1 99) replace
    keep city_code year crime_rate_10k secondary_ind ln_unemp ln_gov_expen ln_gdp_per
    isid city_code year
    save `legacy_processed', replace
restore

keep city_code year crime_rate_10k_raw secondary_ind_candidate ln_unemp_candidate ln_gov_expen_candidate ln_gdp_per_candidate candidate_low_gdp legacy_drop_sansha
merge 1:1 city_code year using `legacy_processed', nogen
isid city_code year
save `legacy_processed', replace

* -----------------------------------------------------------------------------
* 2. Build baseline_city_year_v02 and the construction sample flow.
* -----------------------------------------------------------------------------
use "$treatment", clear
merge 1:1 city_code year using `crime', keep(master match) gen(merge_crime)
assert inlist(merge_crime,1,3)
merge 1:1 city_code year using `cityfull', keep(master match) gen(merge_city_panel)
assert inlist(merge_city_panel,1,3)
merge 1:1 city_code year using `legacy_processed', keep(master match) nogen
isid city_code year

gen long province_code=floor(city_code/10000)*10000 if !missing(city_code)
gen byte sample_00_all=1
gen byte sample_01_crime_match=(merge_crime==3)
gen byte sample_02_citypanel_match=sample_01_crime_match & merge_city_panel==3
gen byte sample_03_valid_population=sample_02_citypanel_match & pop_registered_10k>0 & !missing(pop_registered_10k,xs_total)
gen byte sample_04_valid_treatment=sample_03_valid_population & !missing(hitech_share_all)
gen byte sample_05_valid_legacy_controls=sample_04_valid_treatment & !legacy_drop_sansha & !missing(secondary_ind,ln_unemp,ln_gov_expen,ln_gdp_per)

sort city_code year
xtset city_code year
gen byte sample_06_valid_lags=!missing(crime_rate_10k,L.hitech_share_all,L.secondary_ind,L.ln_unemp,L.ln_gov_expen,L.ln_gdp_per) & !legacy_drop_sansha
gen byte sample_07_pre_reghdfe=sample_06_valid_lags

reghdfe crime_rate_10k L.hitech_share_all L.secondary_ind L.ln_unemp L.ln_gov_expen L.ln_gdp_per if sample_07_pre_reghdfe, absorb(city_code year) vce(cluster city_code)
gen byte sample_08_final_reghdfe=e(sample)
gen byte sample_07_pre_reghdfe_current=!missing(crime_rate_10k_raw,L.hitech_share_all,L.secondary_ind_candidate,L.ln_unemp_candidate,L.ln_gov_expen_candidate,L.ln_gdp_per_candidate)
reghdfe crime_rate_10k_raw L.hitech_share_all L.secondary_ind_candidate L.ln_unemp_candidate L.ln_gov_expen_candidate L.ln_gdp_per_candidate if sample_07_pre_reghdfe_current, absorb(city_code year) vce(cluster city_code)
gen byte sample_08_final_reghdfe_current=e(sample)

gen double L_hitech_share_all=L.hitech_share_all
gen double L_hitech_share_all_legacyraw=L.hitech_share_all_legacyraw
gen double L_secondary_ind=L.secondary_ind
gen double L_ln_unemp=L.ln_unemp
gen double L_ln_gov_expen=L.ln_gov_expen
gen double L_ln_gdp_per=L.ln_gdp_per
gen double L_secondary_ind_candidate=L.secondary_ind_candidate
gen double L_ln_unemp_candidate=L.ln_unemp_candidate
gen double L_ln_gov_expen_candidate=L.ln_gov_expen_candidate
gen double L_ln_gdp_per_candidate=L.ln_gdp_per_candidate
gen double L_gdp_per_capita_yuan=L.gdp_per_capita_yuan
gen byte singleton_legacy=sample_07_pre_reghdfe & !sample_08_final_reghdfe
gen byte singleton_current=sample_07_pre_reghdfe_current & !sample_08_final_reghdfe_current

label var crime_rate_10k_raw "Crime cases per 10k registered residents, untrimmed"
label var crime_rate_10k "Legacy baseline crime rate after archived 1/99 winsorization"
label var secondary_ind "Legacy second-industry GDP share after archived winsorization"
label var ln_unemp "Legacy ln(registered unemployed persons+1), archived cleaning"
label var ln_gov_expen "Legacy ln(local general public budget expenditure+1), nominal"
label var ln_gdp_per "Legacy ln(GDP per capita+1), values below 5 set missing and winsorized"

sort city_code year
isid city_code year
compress
save "$out/baseline_city_year_v02.dta", replace

postfile ps str20 record_type byte stage str40 stage_label long observations int cities int first_year int last_year long dropped_observations int dropped_cities long province_code int year long drop_count using `sample_flow_summary', replace
clear
set obs 0
gen str20 record_type=""
gen byte stage=.
gen str40 stage_label=""
gen long observations=.
gen int cities=.
gen int first_year=.
gen int last_year=.
gen long dropped_observations=.
gen int dropped_cities=.
gen long province_code=.
gen int year=.
gen long drop_count=.
save `sample_flow_dist', replace

use "$out/baseline_city_year_v02.dta", clear
local stages sample_01_crime_match sample_02_citypanel_match sample_03_valid_population sample_04_valid_treatment sample_05_valid_legacy_controls sample_06_valid_lags sample_07_pre_reghdfe sample_08_final_reghdfe
local labels "crime_match citypanel_match valid_population valid_treatment valid_legacy_controls valid_lags pre_reghdfe final_reghdfe"
local prev sample_00_all
local i=0
foreach s of local stages {
    local ++i
    local lab : word `i' of `labels'
    quietly count if `s'
    local nobs=r(N)
    preserve
        keep if `s'
        local ncities=0
        local ymin=.
        local ymax=.
        if _N>0 {
            quietly summarize year
            local ymin=r(min)
            local ymax=r(max)
            keep city_code
            duplicates drop
            local ncities=_N
        }
    restore
    quietly count if `prev' & !`s'
    local ndrop=r(N)
    preserve
        keep if `prev'
        bysort city_code: egen byte any_current=max(`s')
        keep if !any_current
        local cdrop=0
        if _N>0 {
            keep city_code
            duplicates drop
            local cdrop=_N
        }
    restore
    post ps ("stage_summary") (`i') ("`lab'") (`nobs') (`ncities') (`ymin') (`ymax') (`ndrop') (`cdrop') (.) (.) (.)
    preserve
        keep if `prev' & !`s'
        if _N>0 {
            collapse (count) drop_count=city_code, by(province_code year)
            gen str20 record_type="drop_distribution"
            gen byte stage=`i'
            gen str40 stage_label="`lab'"
            gen long observations=.
            gen int cities=.
            gen int first_year=.
            gen int last_year=.
            gen long dropped_observations=.
            gen int dropped_cities=.
            append using `sample_flow_dist'
            save `sample_flow_dist', replace
        }
    restore
    local prev `s'
}
postclose ps
use `sample_flow_summary', clear
append using `sample_flow_dist'
sort stage record_type province_code year
export delimited using "$diag/baseline_sample_flow.csv", replace

* -----------------------------------------------------------------------------
* 3. Standardized v03 fields used by the baseline regression (baseline_L1.do).
* -----------------------------------------------------------------------------
use 市代码 年份 年末城镇户籍人口_万人_全市 using "$citydata", clear
rename 市代码 city_code
rename 年份 year
rename 年末城镇户籍人口_万人_全市 urban_registered_population_10k
keep if inrange(year,2007,2020)
isid city_code year
save `urbanpop', replace

use "$out/baseline_city_year_v02.dta", clear
isid city_code year
merge 1:1 city_code year using `urbanpop', keep(master match) nogen

preserve
    clear
    set obs 14
    gen int year=2006+_n
    gen double cpi_yoy=.
    local cpivals "104.8 105.9 99.3 103.3 105.4 102.6 102.6 102.0 101.4 102.0 101.6 102.1 102.9 102.5"
    forvalues i=1/14 {
        local val : word `i' of `cpivals'
        replace cpi_yoy=`val' in `i'
    }
    gen double cpi_2020base=.
    replace cpi_2020base=100 if year==2020
    forvalues y=2019(-1)2007 {
        quietly summarize cpi_2020base if year==`=`y'+1'
        local next=r(mean)
        quietly summarize cpi_yoy if year==`=`y'+1'
        replace cpi_2020base=`next'/(r(mean)/100) if year==`y'
    }
    isid year
    save `cpi', replace
restore
merge m:1 year using `cpi', assert(3) nogen

gen double crime_rate_10k_corrected=crime_rate_10k_raw
gen double crime_rate_10k_legacy=crime_rate_10k
gen double secondary_ind_legacy=secondary_ind
gen double ln_unemp_legacy=ln_unemp
gen double ln_gov_expen_legacy=ln_gov_expen
gen double ln_gdp_per_legacy=ln_gdp_per
gen double secondary_va_share=secondary_source/100 if inrange(secondary_source,0,100)
replace secondary_va_share=secondary_source if inrange(secondary_source,0,1)
gen double registered_population=pop_registered_10k*10000 if pop_registered_10k>0
gen double urban_registered_population=urban_registered_population_10k*10000 if urban_registered_population_10k>0
gen double registered_unemployment_rate=100*unemployment_count/urban_registered_population if unemployment_count>=0 & urban_registered_population>0
gen double unemployed_per_10k=10000*unemployment_count/registered_population if unemployment_count>=0 & registered_population>0
gen double real_gdppc_2020yuan=gdp_per_capita_yuan/(cpi_2020base/100) if gdp_per_capita_yuan>0
gen double fiscal_pc_nominal_yuan=gov_expenditure_10k/pop_registered_10k if gov_expenditure_10k>0 & pop_registered_10k>0
gen double real_fiscal_pc_2020yuan=fiscal_pc_nominal_yuan/(cpi_2020base/100) if fiscal_pc_nominal_yuan>0
gen double ln_real_gdppc=ln(real_gdppc_2020yuan) if real_gdppc_2020yuan>0
gen double ln_real_fiscal_pc=ln(real_fiscal_pc_2020yuan) if real_fiscal_pc_2020yuan>0
gen double ln_registered_population=ln(registered_population) if registered_population>0
assert real_gdppc_2020yuan>0 if !missing(ln_real_gdppc)
assert real_fiscal_pc_2020yuan>0 if !missing(ln_real_fiscal_pc)
assert registered_population>0 if !missing(ln_registered_population)

sort city_code year
xtset city_code year
foreach v in hitech_share_all hitech_share_classified industry_code_coverage secondary_va_share registered_unemployment_rate unemployed_per_10k ln_real_gdppc ln_real_fiscal_pc ln_registered_population secondary_ind_legacy ln_unemp_legacy ln_gov_expen_legacy ln_gdp_per_legacy {
    gen double L1_`v'=L1.`v'
    gen double L2_`v'=L2.`v'
}
gen double L3_hitech_share_all=L3.hitech_share_all
gen double F1_hitech_share_all=F1.hitech_share_all
foreach v in hitech_share_all hitech_share_classified industry_code_coverage secondary_va_share unemployed_per_10k ln_real_gdppc ln_real_fiscal_pc ln_registered_population {
    bysort city_code: egen double base07_`v'=max(cond(year==2007,`v',.))
}
gen double coverage_abs_jump=abs(D.industry_code_coverage) if !missing(D.industry_code_coverage)
bysort city_code: egen double coverage_max_jump=max(coverage_abs_jump)
bysort city_code: egen int coverage_jump_pairs=total(!missing(coverage_abs_jump))
gen byte coverage_stable_city=(coverage_jump_pairs>=5 & coverage_max_jump<=0.10)
gen byte sample_preferred_core=!missing(crime_rate_10k_corrected,L1_hitech_share_all,L2_secondary_va_share,L2_unemployed_per_10k,L2_ln_real_gdppc,L2_ln_registered_population,province_code,registered_population) & registered_population>0
gen byte sample_ppml_candidate=sample_preferred_core & !missing(xs_total) & xs_total>=0
gen int preferred_year_count_0916=.
bysort city_code: egen preferred_year_count_0916_temp=total(sample_preferred_core & inrange(year,2009,2016))
replace preferred_year_count_0916=preferred_year_count_0916_temp
drop preferred_year_count_0916_temp
gen byte balanced_city=(preferred_year_count_0916==8)
gen byte balanced_sample_2009_2016=balanced_city & sample_preferred_core & inrange(year,2009,2016)
gen double D_crime_rate_10k=D.crime_rate_10k_corrected
gen double D_L1_hitech_share_all=D.L1_hitech_share_all

label var secondary_va_share "Second-industry value-added share of GDP, fraction"
label var registered_unemployment_rate "Registered unemployed / urban registered population, percent; unavailable"
label var unemployed_per_10k "Registered unemployed per 10,000 registered residents"
label var ln_real_gdppc "ln real GDP per capita, 2020 yuan, national CPI deflator"
label var ln_real_fiscal_pc "ln real general-budget expenditure per registered resident, 2020 yuan"
label var ln_registered_population "ln year-end registered population, persons"
label var coverage_stable_city "City max annual coverage jump <=10pp with >=5 observed jumps"
label var balanced_city "City has complete preferred-core observations for every 2009-2016 year"
label var balanced_sample_2009_2016 "Balanced preferred-core sample, 2009-2016"
label var sample_preferred_core "Preferred identification eligibility marker"

sort city_code year
isid city_code year
compress
save "$out/baseline_city_year_v03.dta", replace
display as result "CITY_YEAR_PANEL_COMPLETE v02_v03_written"
