version 18.0
clear all
set more off
set maxvar 10000
unicode encoding set utf8

* Data construction, stage 2: cleaned firm-year records -> city-year high-tech
* share (high-tech firms as a percentage of all deduplicated firms).
* Set the project directory before running, e.g.  global root "path/to/project"
* Expected layout: $root/02_data/raw (read-only inputs) and $root/02_data/analysis_ready.
if `"$root"' == "" {
    display as error "Set global root to the local project directory first."
    exit 198
}
global out  "$root/02_data/analysis_ready"
global diag "$root/05_outputs/diagnostics"
global clean "$out/clean_firm_year_v01.dta"
global treatment "$out/treatment_city_year_v01.dta"

confirm file "$clean"
capture mkdir "$root/05_outputs"
capture mkdir "$diag"

tempfile annual_audit dropped_audit legacy_city treatment_stats
use "$clean", clear

postfile pa str32 record_type str48 metric str16 statistic int year double value str200 note using `annual_audit', replace
forvalues y=2007/2020 {
    quietly count if year==`y'
    post pa ("annual_firm_audit") ("raw_rows") ("count") (`y') (r(N)) ("before firm-year dedup")
    quietly count if year==`y' & missing(sdid_str)
    post pa ("annual_firm_audit") ("sdid_missing_rows") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & dup_sdid_year>0
    post pa ("annual_firm_audit") ("sdid_duplicate_rows") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & missing(newgcid_str)
    post pa ("annual_firm_audit") ("newgcid_missing_rows") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & missing(city_code)
    post pa ("annual_firm_audit") ("city_code_missing_rows") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & !industry_code_ok
    post pa ("annual_firm_audit") ("industry_invalid_or_missing_rows") ("count") (`y') (r(N)) ("valid means four trailing digits")
    quietly count if year==`y' & dup_firmyear>0
    post pa ("annual_firm_audit") ("firmyear_duplicate_rows") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & tag_dup_group
    post pa ("annual_firm_audit") ("firmyear_duplicate_groups") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & city_conflict_group
    post pa ("annual_firm_audit") ("duplicate_city_conflict_groups") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & industry_conflict_group
    post pa ("annual_firm_audit") ("duplicate_industry_conflict_groups") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & !missing(city_code)
    post pa ("annual_firm_audit") ("total_before_dedup") ("count") (`y') (r(N)) ("record-count denominator")
    quietly count if year==`y' & !missing(city_code) & industry_code_ok
    post pa ("annual_firm_audit") ("classified_before_dedup") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & !missing(city_code) & hightech_old
    post pa ("annual_firm_audit") ("hitech_before_dedup") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & keep_current & !missing(city_code)
    post pa ("annual_firm_audit") ("total_after_dedup") ("count") (`y') (r(N)) ("prefixed firm_id-year denominator")
    quietly count if year==`y' & keep_current & !missing(city_code) & industry_code_ok
    post pa ("annual_firm_audit") ("classified_after_dedup") ("count") (`y') (r(N)) ("")
    quietly count if year==`y' & keep_current & !missing(city_code) & hightech_current
    post pa ("annual_firm_audit") ("hitech_after_dedup") ("count") (`y') (r(N)) ("")
}
quietly count if !industry_code_ok
post pa ("industry_code_audit") ("invalid_or_missing_rows") ("count") (.) (r(N)) ("valid means four trailing digits")
quietly count if code4_old!=code4_current & (industry_code_ok | !missing(code4_current))
post pa ("industry_code_audit") ("old_vs_current_extraction_mismatch") ("count") (.) (r(N)) ("old substr(-4,4); current trailing-four-digit regex")
post pa ("industry_code_audit") ("old_vs_current_list_difference") ("count") (.) (0) ("181 archived codes; current list identical")
post pa ("industry_code_audit") ("observed_storage_type") ("text") (.) (.) ("行业小类代码 is str5 in all 2007-2020 files; variable label GB_T4754_2017")
post pa ("industry_code_audit") ("classification_version") ("text") (.) (.) ("provider labels all years GB_T4754_2017; original 2007/2011 crosswalk not preserved")
postclose pa

preserve
    keep if !keep_current & dup_firmyear>0
    gen str32 record_type="dropped_duplicate"
    gen str48 metric=""
    gen str16 statistic=""
    gen double value=.
    gen str200 note="discarded after completeness score; sdid deterministic tie-break"
    keep record_type metric statistic year value note city_code firm_id sdid_str newgcid_str industry_raw code4_old completeness_score dup_firmyear min_city max_city min_ind max_ind
    save `dropped_audit', replace
restore

* An aggregation from raw records is kept as a cross-check of the treatment.
preserve
    keep if !missing(city_code)
    gen byte one=1
    collapse (sum) legacy_total_records=one legacy_classified_records=industry_code_ok legacy_hitech_records=hightech_old, by(city_code year)
    isid city_code year
    gen double hitech_share_all_legacyraw=100*legacy_hitech_records/legacy_total_records
    gen double hitech_share_class_legacyraw=100*legacy_hitech_records/legacy_classified_records if legacy_classified_records>0
    save `legacy_city', replace
restore

keep if keep_current
isid firm_id year
drop if missing(city_code)
gen byte one=1
collapse (sum) total_firms=one classified_firms=industry_code_ok hitech_firms=hightech_current, by(city_code year)
isid city_code year
gen long missing_industry_firms=total_firms-classified_firms
gen double hitech_share_all=100*hitech_firms/total_firms
gen double hitech_share_classified=100*hitech_firms/classified_firms if classified_firms>0
gen double industry_code_coverage=classified_firms/total_firms
gen double hitech_share_all_currentextract=hitech_share_all
merge 1:1 city_code year using `legacy_city', assert(3) nogen
isid city_code year

label var total_firms "Deduplicated firms in city-year"
label var classified_firms "Deduplicated firms with valid four-digit industry code"
label var missing_industry_firms "Deduplicated firms without valid four-digit industry code"
label var hitech_firms "Deduplicated high-tech-industry firms"
label var hitech_share_all "High-tech firms / all deduplicated firms, percentage points"
label var hitech_share_classified "High-tech firms / classified firms, percentage points"
label var industry_code_coverage "Classified firms / all deduplicated firms"
compress
save "$treatment", replace

postfile pt str32 record_type str48 metric str16 statistic int year double value str200 note using `treatment_stats', replace
foreach y of numlist 2007/2020 {
    foreach v in total_firms classified_firms missing_industry_firms hitech_firms hitech_share_all hitech_share_classified industry_code_coverage {
        quietly summarize `v' if year==`y', detail
        post pt ("treatment_year_stat") ("`v'") ("mean") (`y') (r(mean)) ("")
        post pt ("treatment_year_stat") ("`v'") ("median") (`y') (r(p50)) ("")
        post pt ("treatment_year_stat") ("`v'") ("sd") (`y') (r(sd)) ("")
        post pt ("treatment_year_stat") ("`v'") ("p1") (`y') (r(p1)) ("")
        post pt ("treatment_year_stat") ("`v'") ("p5") (`y') (r(p5)) ("")
        post pt ("treatment_year_stat") ("`v'") ("p95") (`y') (r(p95)) ("")
        post pt ("treatment_year_stat") ("`v'") ("p99") (`y') (r(p99)) ("")
        post pt ("treatment_year_stat") ("`v'") ("min") (`y') (r(min)) ("")
        post pt ("treatment_year_stat") ("`v'") ("max") (`y') (r(max)) ("")
    }
}
postclose pt

use `annual_audit', clear
gen long city_code=.
gen str90 firm_id=""
gen str32 sdid_str=""
gen str40 newgcid_str=""
gen str24 industry_raw=""
gen str4 code4_old=""
gen byte completeness_score=.
gen long dup_firmyear=.
gen double min_city=.
gen double max_city=.
gen double min_ind=.
gen double max_ind=.
append using `treatment_stats'
append using `dropped_audit'
order record_type metric statistic year value note city_code firm_id sdid_str newgcid_str industry_raw code4_old completeness_score dup_firmyear min_city max_city min_ind max_ind
sort record_type year metric statistic city_code firm_id sdid_str
export delimited using "$diag/treatment_construction_audit.csv", replace

display as result "HIGHTECH_TREATMENT_COMPLETE city_years=" _N
