version 18.0
clear all
set more off
set maxvar 10000

* Portable extraction of 03_code/stata/04_baseline_L1.do, inspected 2026-10-14.
* Only input/output plumbing and explanatory checks are adapted.
* The regression, sample flag, controls, FE, clustering and numerical anchors
* are retained. This is a frozen observational benchmark, not a causal design.
*
* From the repository root in Stata:
* do "high-tech-and-urban-crime/code/baseline_L1.do" "path/to/baseline_city_year_v03.dta" "high-tech-and-urban-crime/outputs/baseline"
args input_file output_dir
if `"`input_file'"'=="" | `"`output_dir'"'=="" {
    display as error "Supply the canonical v03 .dta input and a new output directory."
    exit 198
}
confirm file `"`input_file'"'
capture which reghdfe
if _rc {
    display as error "Install reghdfe and its ftools dependency explicitly before running."
    exit 499
}
capture mkdir `"`output_dir'"'
capture confirm file `"`output_dir'/baseline_L1.log"'
if !_rc {
    display as error "Output already has a run log; choose a new output directory."
    exit 602
}
log using `"`output_dir'/baseline_L1.log"', text
which reghdfe
which ftools
use `"`input_file'"', clear
isid city_code year
foreach v in city_code year province_code crime_rate_10k_corrected ///
    L1_hitech_share_all L1_secondary_ind_legacy L1_ln_unemp_legacy ///
    L1_ln_gov_expen_legacy L1_ln_gdp_per_legacy sample_preferred_core {
    confirm numeric variable `v'
}
assert inlist(sample_preferred_core,0,1)

reghdfe crime_rate_10k_corrected L1_hitech_share_all ///
    L1_secondary_ind_legacy L1_ln_unemp_legacy ///
    L1_ln_gov_expen_legacy L1_ln_gdp_per_legacy ///
    if sample_preferred_core, absorb(city_code year) vce(cluster city_code)

scalar baseline_b=_b[L1_hitech_share_all]
scalar baseline_se=_se[L1_hitech_share_all]
scalar baseline_p=2*ttail(e(df_r),abs(baseline_b/baseline_se))
scalar baseline_N=e(N)
quietly levelsof city_code if e(sample), local(base_cities)
scalar baseline_cities=wordcount("`base_cities'")
quietly summarize year if e(sample), meanonly
scalar baseline_y0=r(min)
scalar baseline_y1=r(max)
assert baseline_N==2464
assert baseline_cities==280
assert baseline_y0==2009
assert baseline_y1==2017
assert abs(baseline_b-0.482869678)<1e-7
assert abs(baseline_se-0.145299561)<1e-7
assert abs(baseline_p-0.001008548)<1e-7

* Private local diagnostic only: do not add these sample keys to the repository.
gen byte baseline_L1_esample=e(sample)
preserve
    keep if baseline_L1_esample
    keep city_code year province_code baseline_L1_esample
    compress
    isid city_code year
    save `"`output_dir'/baseline_L1_esample.dta"'
restore

tempfile result
postfile pr str24 model_id double coefficient std_error p_value ///
    N cities first_year last_year using `"`result'"'
post pr ("TABLE1_COL2_L1") (baseline_b) (baseline_se) (baseline_p) ///
    (baseline_N) (baseline_cities) (baseline_y0) (baseline_y1)
postclose pr
use `"`result'"', clear
format coefficient std_error p_value %18.12f
export delimited using `"`output_dir'/baseline_L1_results.csv"', datafmt
display as result "BASELINE_L1_COMPLETE: all frozen anchors passed."
log close
