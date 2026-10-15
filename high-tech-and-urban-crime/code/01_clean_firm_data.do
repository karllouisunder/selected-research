version 18.0
clear all
set more off
set maxvar 10000
unicode encoding set utf8

* Data construction, stage 1: annual tax-survey records (2007-2020) -> cleaned
* firm-year file. Raw files are read-only. Firms are classified as high-tech by
* their four-digit industry code.
* Set the project directory before running, e.g.  global root "path/to/project"
* Expected layout: $root/02_data/raw (read-only inputs) and $root/02_data/analysis_ready.
if `"$root"' == "" {
    display as error "Set global root to the local project directory first."
    exit 198
}
global raw  "$root/02_data/raw"
global tax  "$raw/Tax_inves_data"
global out  "$root/02_data/analysis_ready"

capture mkdir "$out"

use 年份 sdid newgcid 市代码 行业小类代码 using "$tax/2007sd_info.dta", clear
forvalues y=2008/2020 {
    append using "$tax/`y'sd_info.dta", keep(年份 sdid newgcid 市代码 行业小类代码)
}
rename 年份 year
assert inrange(year,2007,2020)

capture confirm numeric variable 市代码
if !_rc gen long city_code=市代码
else destring 市代码, gen(city_code) force

capture confirm string variable 行业小类代码
if !_rc gen str24 industry_raw=ustrtrim(行业小类代码)
else tostring 行业小类代码, gen(industry_raw) format(%18.0g) force

capture confirm string variable sdid
if !_rc gen str32 sdid_str=ustrtrim(sdid)
else tostring sdid, gen(sdid_str) format(%30.0g) force

capture confirm string variable newgcid
if !_rc gen str40 newgcid_str=ustrtrim(newgcid)
else tostring newgcid, gen(newgcid_str) format(%30.0g) force

* Two independent extractions of the trailing four-digit code must agree.
gen str4 code4_old=substr(ustrtrim(industry_raw),-4,4)
gen str4 code4_current=""
replace code4_current=ustrregexs(1) if ustrregexm(ustrtrim(industry_raw),"([0-9]{4})$")
gen byte industry_code_ok=ustrregexm(code4_old,"^[0-9][0-9][0-9][0-9]$")
assert code4_old==code4_current if industry_code_ok
gen double code4_num=real(code4_old) if industry_code_ok

* Prefixed firm ID: NGCID is cross-year; SDID fallback is year-local.
gen str90 firm_id="NGCID:"+newgcid_str if !missing(newgcid_str)
replace firm_id="SDID:"+string(year,"%04.0f")+":"+sdid_str if missing(firm_id)
assert !missing(firm_id)

* High-tech manufacturing and services list (181 four-digit codes).
gen byte hightech_old=0
replace hightech_old=1 if inlist(code4_old,"2710","2720","2730","2740","2750","2761","2762","2770","2780")
replace hightech_old=1 if inlist(code4_old,"3741","3742","3743","3744","3749","4343")
replace hightech_old=1 if inlist(code4_old,"3562","3563","3569","3832","3833","3841","3921","3922","3940")
replace hightech_old=1 if inlist(code4_old,"3931","3932","3933","3934","3939","3951","3952","3953","3971")
replace hightech_old=1 if inlist(code4_old,"3972","3973","3974","3975","3976","3979","3981","3982","3983")
replace hightech_old=1 if inlist(code4_old,"3984","3985","3989","3961","3962","3963","3969","3990")
replace hightech_old=1 if inlist(code4_old,"3911","3912","3913","3914","3915","3919","3474","3475")
replace hightech_old=1 if inlist(code4_old,"3581","3582","3583","3584","3585","3586","3589","4011","4012")
replace hightech_old=1 if inlist(code4_old,"4013","4014","4015","4016","4019","4021","4022","4023","4024")
replace hightech_old=1 if inlist(code4_old,"4025","4026","4027","4028","4029","4040","4090")
replace hightech_old=1 if inlist(code4_old,"2664","2665")
replace hightech_old=1 if inlist(code4_old,"6311","6312","6319","6321","6322","6331","6339","6410","6421")
replace hightech_old=1 if inlist(code4_old,"6490","6511","6512","6513","6519","6531","6532","6540","6560")
replace hightech_old=1 if inlist(code4_old,"6440","6450","6550","6520","6591","6599","6571","6572","6579")
replace hightech_old=1 if inlist(code4_old,"6422","6429","8625","8626","8710","8720","8740","8890")
replace hightech_old=1 if inlist(code4_old,"6431","6432","6433","6434","6439","6930")
replace hightech_old=1 if inlist(code4_old,"7451","7452","7453","7454","7455","7459")
replace hightech_old=1 if inlist(code4_old,"7410","7420","7431","7432","7439","7441","7449","7471","7472")
replace hightech_old=1 if inlist(code4_old,"7473","7474","7475","7481","7482","7483","7484","7485","7486")
replace hightech_old=1 if inlist(code4_old,"7310","7320","7330","7340","7491","7492")
replace hightech_old=1 if inlist(code4_old,"7511","7512","7513","7514","7515","7516","7517","7519")
replace hightech_old=1 if inlist(code4_old,"7530","7540","7590")
replace hightech_old=1 if inlist(code4_old,"7520","7231","7239")
replace hightech_old=1 if inlist(code4_old,"7461","7462","7463","7721","7722","7723","7724","7725")
replace hightech_old=1 if inlist(code4_old,"7726","7727","7729")
replace hightech_old=0 if !industry_code_ok

gen byte hightech_current=hightech_old
assert hightech_current==hightech_old

* Duplicate/conflict flags are retained for the treatment audit.
duplicates tag sdid_str year, gen(dup_sdid_year)
duplicates tag firm_id year, gen(dup_firmyear)
egen byte tag_firmyear=tag(firm_id year)
gen byte tag_dup_group=tag_firmyear & dup_firmyear>0
bysort firm_id year: egen double min_city=min(city_code)
bysort firm_id year: egen double max_city=max(city_code)
bysort firm_id year: egen double min_ind=min(code4_num)
bysort firm_id year: egen double max_ind=max(code4_num)
gen byte city_conflict_group=tag_dup_group & min_city<max_city if !missing(min_city,max_city)
replace city_conflict_group=0 if missing(city_conflict_group)
gen byte industry_conflict_group=tag_dup_group & min_ind<max_ind if !missing(min_ind,max_ind)
replace industry_conflict_group=0 if missing(industry_conflict_group)

gen byte completeness_score=(!missing(city_code))+industry_code_ok
gen byte completeness_sort=-completeness_score
duplicates tag firm_id year completeness_sort sdid_str, gen(tied_sort_key)
assert tied_sort_key==0
drop tied_sort_key
sort firm_id year completeness_sort sdid_str
bysort firm_id year: gen byte keep_current=(_n==1)
assert keep_current==1 if dup_firmyear==0

compress
save "$out/clean_firm_year_v01.dta", replace
display as result "CLEAN_FIRM_DATA_COMPLETE rows=" _N
