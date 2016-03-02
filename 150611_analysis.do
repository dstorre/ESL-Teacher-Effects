/***************************************ANALYTICAL RESULTS*************************************/
capture log close
sysdir set PERSONAL "/project/CIPP/Analyst_Working_Area/Analysts/dstorre"


estimates clear

cd /nas/depts/004/CIPP/Analyst_Working_Area/Analysts/dstorre

local c_date = c(current_date)
local c_time = c(current_time)
log using "analysis_results_`c_date'_`c_time'_.log", replace
set more off



/******************************************Switch board***********************************************************************/
local open=0
local regress=1 /*expose with 0 and 1, copmlete exp basegroup */
local describe=0
local graphs=0

/*****************************************************************************************************************************/
foreach subject in  la ma {
if `open'==1 {
set more off
set trace off

*identify variables you need for analysis to keep file small
local keep std_`subject' islep waslep grade yr hlmclassid t_cipp_id lea_sch exel2not lxd_esl  ///
	seg_w_islep pct_t_samerace s_everel_pct bilher_sch newdest  est_g est_s otherdest ///
	pre_`subject' peer_`subject' /*yrs_as_islep yrs_as_waslep */ male black asian white hispanic ex_aig ex_dis force_move movediy movedpy ///
	daysabs overage underage frpl exel2not exislep2notis num_students peer_`subject' curr_adv curr_rem  c_islep_pct ///
	c_waslep_pct c_frpl_pct c_hispanic_pct  c_black_pct c_white_pct c_islep c_waslep c_everel c_everel_pct ///
	c_male_pct c_islep90 c_waslep90 c_black90 c_hispanic90  c_white90 ///
	exp3orlessyr   praxis_esl_sc praxis_read_sc praxis_write_sc praxis_math_sc tlatino twhite tblack t_samerace ug_esl matesol ///
	seg_w_islep seg_w_waslep pct_t_samerace s_everel_pct bilher_sch newdest  est_g est_s otherdest ///
	rural topfive  adm tot_ppx lea_title3 full_licen s_lxd_esl_pct sch_nbc sch_advdeg ///
	s_islep_pct s_waslep_pct s_hispanic_pct s_black_pct s_white_pct s_frpl_pct s_everel90 s_hispanic90 s_black90 s_white90 desttype urb urban ///
	qtile_exel2not_s qtile_exislep2notis qtile_exislep2notis_s qtile_exel2not exwaslep2notwas adm_squ daysmem urban urban_label l_tot s_tot neverel ///
	everel lep_status otherurb qtile_seg_w_islep qtile_seg_w_waslep rc_grade  reclass school_name countyname prc111 prc54  ///
	 matesol mst_esl no_esl_ed dis_type wise_id t_cipp_id lea_sch lea con  class_num lowelcon highelcon medelcon  course_title

* append files to make longitudinal dataset, 0809-1213, 3-8
use `keep' using es_0809_`subject', clear
append using es_0708_`subject', keep(`keep')
append using es_0910_`subject', keep(`keep') 
append using es_1011_`subject', keep(`keep')
append using es_1112_`subject', keep(`keep')
append using es_1213_`subject', keep(`keep')
append using ms_0708_`subject', keep(`keep')
append using ms_0809_`subject', keep(`keep') 
append using ms_0910_`subject', keep(`keep') 
append using ms_1011_`subject', keep(`keep')
append using ms_1112_`subject', keep(`keep')
append using ms_1213_`subject', keep(`keep')

save append_`subject', replace

* merge testing
destring yr, replace
merge m:1 t_cipp_id yr using testing, gen(p)
drop if p==2

* append fixed reclass information 
drop reclass rc_grade
destring yr, replace
merge m:1 wise_id yr using reclass33_8, gen(r)
drop if r==2

*merge teacher experience
merge m:1 t_cipp_id yr using teacher_experience, gen(t)
drop if t==2 


*count number of years teacher is in dataset 
egen t_y_tag=tag(t_cipp_id yr) /*how many years the teacher is in the dataset*/
bysort t_cipp_id: egen n_yrs= total(t_y_tag) /*count that*/
tab n_yrs
*egen t_y_c_tag=tag(t_cipp_id yr hlmclassid) /*howmany classes*/
bysort t_cipp_id yr: gen eslexp= c_everel>1 /*mark if the class has moret han one el*/
gsort t_cipp_id yr -eslexp /*fill it in*/
replace eslexp=eslexp[_n-1] if eslexp[_n-1]==1 & eslexp==0 & t_cipp_id==t_cipp_id[_n-1] & yr==yr[_n-1]


bysort t_cipp_id: egen yrs_exp_el=total(t_y_tag) if eslexp==1
replace yrs_exp_el=0 if yrs_exp_el==.
*make last year expereince

gen prev_yr_eslexp=1 if yr!=yr[_n-1] & eslexp[_n-1]==1 & t_cipp_id==t_cipp_id[_n-1]
replace prev_yr_eslexp=1 if prev_yr_eslexp[_n-1]==1 & prev_yr_eslexp==. & t_cipp_id==t_cipp_id[_n-1] & yr==yr[_n-1]
bysort t_cipp_id: gen tnumyr=_n
replace tnumyr=0 if tnumyr>1
replace tnumyr=1 if tnumyr[_n-1]==1 & tnumyr==0 & t_cipp_id==t_cipp_id[_n-1] & yr==yr[_n-1]
replace prev_yr_eslexp=0 if prev_yr_eslexp==. & tnumyr==0
drop if yr==0708

* restrict to everels
drop if islep==0 & waslep==0


* replace 999 with missing
mvdecode  `keep', mv(999=.)

* drop duplicates
duplicates drop 
duplicates drop wise_id t_cipp_id lea_sch grade hlmclassid yr male black asian white hispanic std_`subject', force

* drop grade three, they dont have pre scores
drop if grade==3


*make year dummies
tab yr, gen(yr_)
rename yr_1 yr0809
rename yr_2 yr0910
rename yr_3 yr1011
rename yr_4 yr1112
rename yr_5 yr1213

*drop students without scores after comparing students with and without a score
gen missing_y=std_`subject'==.
local keepnum islep waslep grade yr0809 yr0910 yr1011 yr1112 yr1213 exel2not lxd_esl  ///
	seg_w_islep seg_w_waslep pct_t_samerace s_everel_pct bilher_sch newdest  est_g est_s otherdest ///
	pre_`subject' peer_`subject' yrs_as_islep yrs_as_waslep  male black asian white hispanic ex_aig ex_dis force_move movediy movedpy ///
	daysabs overage underage frpl exel2not exislep2notis num_students curr_adv curr_rem  c_islep_pct ///
	c_waslep_pct c_frpl_pct c_hispanic_pct  c_black_pct c_white_pct ///
	c_male_pct c_islep90 c_waslep90 c_black90 c_hispanic90  c_white90 ///
	  exp3orlessyr   praxis_esl_sc praxis_read_sc praxis_write_sc praxis_math_sc tlatino twhite tblack t_samerace ug_esl matesol ///
	rural topfive otherurb  adm tot_ppx lea_title3 full_licen s_lxd_esl_pct sch_nbc sch_advdeg ///
	s_islep_pct s_waslep_pct s_hispanic_pct s_black_pct s_white_pct s_frpl_pct s_everel90 s_hispanic90 s_black90 s_white90  ///
	adm daysmem   l_tot s_tot neverel experience eslexp yrs_exp_el prev_yr_eslexp ///
	everel lep_status  rc_grade  reclass  prc111 prc54  ///
	  mst_esl no_esl_ed  wise_id class_num lowelcon highelcon medelcon

#delimit ;
eststo ttest:
  quietly estpost ttest `keepnum',
  by(missing_y) unequal
;

#delimit ;
esttab ttest using  "missing_`subject'_ttest.rtf",
        main(b 2) /* Use mean as main report */ 
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
	nogap
        title("Diffrence in Means for Students Missing Dep. Var")
        ;
#delimit cr

*the ttest is missing-not missing
drop if std_`subject'==. 


* Create tags
egen student=tag(yr wise_id)
egen teacher=tag(yr t_cipp_id)
egen school=tag(yr lea_sch)
egen district=tag(yr lea)
egen class=tag(yr hlmclassid)

*clean up exposure variables
 replace exel2not=0 if c_everel_pct==1
 drop if c_everel_pct==0



*alt three. Im going to use this one because it has the fewest groups, so will be simplest. 
xtile exp_y= exel2not if exel2no>0 & exel2not<=1 & yr==1213, nq(2 )
replace exp_y =0 if exel2not==0
replace exp_y=3 if exel2not==1
sum exel2not if exp_y==1
replace exp_y=1 if exel2not >0 & exel2not < r(max)
replace exp_y=2 if exel2not >=r(max) & exel2not <1
tab exp_y, miss

drop x1 x2 no_exp low_exp high_exp all_exp 
tab exp_y, gen(x) 
 gen no_exp= x1==1
 gen low_exp= x2==1
 gen high_exp=x3==1
 gen all_exp=x4==1

 gen expose=.
replace expose=0 if no_exp==1
replace expose=1 if low_exp==1
replace expose=1 if high_exp==1
replace expose=3 if all_exp==1
bysort expose: sum class_num c_everel exel2not  

*transform exposure variable to percent
gen exp= exel2not*100

*Lable values
label define lxd 0 "No ESL Cert" 1 "ESL Cert"
label values lxd_esl lxd
label define last 0 " No El Exp" 1 "El Exp"
label values last_year_el last


*center variables to help interpretation
foreach var in seg_w_waslep s_lxd_esl_pct s_everel_pct seg_w_islep  pct_t_samerace {
	replace `var'=`var'*10
	summarize `var', meanonly
	gen cen_`var' = `var' - r(mean)
}

*make other race
gen otherrace= black==0 &  asian==0 & white==0 &  hispanic==0

*gen classminority
gen c_minority_pct=c_hispanic_pct  +  c_black_pct 

*gen school minority
gen s_minority_pct= s_hispanic_pct+ s_black_pct 

*transform title_3
gen title3=lea_title3/1000000

*make school composite 
bysort lea_sch: egen sch_comp=mean(std_`subject')

*gen quadratic for exp
gen exp_sq= exp*exp

*make es ms markers
gen es=grade<6
gen ms=grade>5

*gen other vars
egen leasch=group(lea_sch)
gen newteach=experience<=3
gen all=1

*rename esl exp
rename prev_yr_eslexp last_year_el

*gen esl class

  gen esl_flag=0
  *replace esl_flag=1 if course_num=="1038" & esl_flag==0
  ***self_contained not a good indicator
  
  replace esl_flag= regexm(course_title, "ESL") if  esl_flag==0
   replace esl_flag= regexm(course_title, "SIOP") if  esl_flag==0
   replace esl_flag= regexm(course_title, "esl") if  esl_flag==0
   replace esl_flag= regexm(course_title, "siop") if  esl_flag==0
   replace esl_flag= regexm(course_title, "AS A") if  esl_flag==0
   replace esl_flag= regexm(course_title, "As a") if  esl_flag==0
   replace esl_flag= regexm(course_title, "as a") if  esl_flag==0
   replace esl_flag= regexm(course_title, "ELL") if  esl_flag==0
   replace esl_flag= regexm(course_title, "2ND LANGUAGE") if  esl_flag==0
   replace esl_flag= regexm(course_title, "Esl") if  esl_flag==0
   replace esl_flag= regexm(course_title, "NEWCOMER") if  esl_flag==0
   replace esl_flag= regexm(course_title, "newcomer") if  esl_flag==0
   replace esl_flag= regexm(course_title, "Newcomer") if  esl_flag==0
   replace esl_flag= regexm(course_title, "LEP") if  esl_flag==0
   replace esl_flag= regexm(course_title, "Pullouts") if  esl_flag==0
   replace esl_flag= regexm(course_title, "INDIV") if  esl_flag==0
   replace esl_flag= regexm(course_title, "INDIVIDUAL") if  esl_flag==0
   replace esl_flag= regexm(course_title, "IND") if  esl_flag==0
   replace esl_flag= regexm(course_title, "PULL OUT") if  esl_flag==0
   replace esl_flag= regexm(course_title, "ASST") if  esl_flag==0
   replace esl_flag= regexm(course_title, "ENGLISH AS SECOND") if  esl_flag==0
   replace esl_flag= regexm(course_title, "SHELTERED") if  esl_flag==0
   replace esl_flag= regexm(course_title, "ACADEMIC ENGLISH") if  esl_flag==0
   
   
*find missing
local subject ma
set more off

local student  pre_`subject' peer_`subject' yrs_as_islep yrs_as_waslep  male black asian white hispanic ex_aig ex_dis ///
	force_move movediy movedpy daysabs overage underage frpl

local class exel2not exislep2notis num_students peer_`subject' curr_adv curr_rem  c_islep_pct c_waslep_pct c_frpl_pct ///
	c_hispanic_pct  c_black_pct c_white_pct c_male_pct c_islep90 c_waslep90 c_black90 c_hispanic90  c_white90 ///
	experience  exp3orlessyr    tlatino twhite tblack tstavg_all experience ///
	t_samerace ug_esl matesol last_year_el

local school  cen_seg_w_islep cen_seg_w_waslep pct_t_samerace s_everel_pct bilher_sch newdest  est_g est_s otherdest ///
	rural topfive  adm tot_ppx lea_title3 full_licen s_lxd_esl_pct sch_nbc sch_advdeg title3 ///
	s_islep_pct s_waslep_pct s_hispanic_pct s_black_pct s_white_pct s_frpl_pct s_everel90 s_hispanic90 s_black90 s_white90 

local treat exp lxd_esl low_exp  high_exp last_year_el


foreach var of local student  {
display as text %12s "`var'"
qui count if `var'==.
scalar miss=r(N)
quietly count
scalar denom=r(N)
scalar pmiss= scalar(miss)/scalar(denom)
scalar list pmiss
}
* only replacing with zero if percetn missing is <10%. All of the teacher ed variables are more than 50%. wont use them in analysis. 
* 12% of new dest varaibles are missing- going to keep and replace with zeros
local missing pre_`subject' peer_`subject' yrs_as_islep yrs_as_waslep  ex_aig ex_dis force_move  movedpy daysabs overage underage ///
num_students  tlatino twhite tblack t_samerace exp3orlessyr adm_squ newdest  cen_seg_w_islep cen_seg_w_waslep est_g est_s  tot_ppx full_licen ///
sch_advdeg sch_nbc tstavg_all experience last_year_el
*make missing data flags, zero replacement

foreach var of local missing { 
display as text %12s "`var'"
gen `var'_mf=missing(`var')
gen `var'_with_miss=`var'
replace `var'=0 if `var'==.
}
local subject ma
label var es "Elementary"
label var otherrace "Other Race"
label var islep "Current El"
label var waslep "Former El"
label var std_`subject' "Std.Score"
label var pre_`subject' "Prior Achievement"
label var yrs_as_islep "Years CEL"
label var yrs_as_waslep "Years FEL"
label var male "Male"
label var  black "Black"
label var asian "Asian"
label var white "White"
label var hispanic "Latino"
label var ex_aig "Gifted"
label var ex_dis "Special Needs"
label var force_move "Forced Move"
label var  movediy "Moved in Year"
label var movedpy "Summer Move"
label var daysabs "Days Absent"
label var  overage "Over Age"
label var underage "Under Age"
label var frpl "Free Lunch"
label var exel2not "Exposure" 
label var num_students "Class Size"
label var class_num "Class Size"
label var peer_`subject' "Peer Achievment"
label var curr_adv "Advanced"
label var curr_rem  "Remedial"
label var c_islep_pct "Class % CEL"
label var low_exp "Low Exposure"
label var no_exp "No Exposure"
label var high_exp "High Exposure"
label var all_exp "Complete Exposure"
label var lxd_esl "ESL Credential"
label var last_year_el "El Experience"
label var cen_seg_w_islep "Evenness of CEL" 
label var seg_w_islep "Evenness of CEL" 
label var cen_seg_w_waslep "Evenness of FEL"
label var seg_w_waslep "Evenness of FEL"
label var cen_pct_t_samerace  "% Teacher Same Race"
label var pct_t_samerace "%Teacher Same Race"
label var cen_s_everel_pct "School % El" 
label var s_everel_pct "School % El"
label var cen_s_lxd_esl_pct "School % ESL Cred"
label var s_lxd_esl_pct "School % ESL Cred"
label var bilher_sch "Bilingual/Heritage"
label var newdest  "New Destination School"
label var est_g "Established and Growing"
label var est_s "Established and Stable"
label var rural "Rural"
label var topfive "Urban"
label var otherurb "Town/Suburb"
label var otherdest "Low Incidence" 
label var experience "Years Experience"
label var exp3orlessyr  "New Teacher"
label var esl_flag "ESL Course"
label var  tstavg_all "Teacher Avg. Test Score" 
label var adm "School Size"
label var adm_squ "School Size Sq"
label var tot_ppx "PPE"
label var title3 "Title 3 Funds"
label var full_licen  "% Full Credential"
label var sch_nbc "% NBC"
label var sch_advdeg  "% Adv Degree"
label var s_minority_pct  "School % Minority"
label var  s_frpl_pct "School % Free Lunch"
label var  yr1011 "2010-2011"
label var yr1112 "2011-2012"
label var yr1213 "2012-2013"
label var yr0910 "2009-2010"
label var yr0809 "2008-2009"

* make districts whatever urbanicity they are in 12 13 so it is a constant
gsort lea -yr dis_type
replace dis_type= dis_type[_n-1] if lea==lea[_n-1]
replace dis_type=1 if urb=="Rural"
drop rural topfive otherurb
gen rural=urb==3
gen topfive=lea=="600" | lea=="920" | lea== "410" | lea=="340" | lea=="320" 
gen otherurb=rural==0 & topfive==0


foreach var in cen_seg_w_islep cen_seg_w_waslep cen_pct_t_samerace  cen_s_everel_pct cen_s_lxd_esl_pct {
replace `var'=`var'/10
}

drop expose
gen expose=.
replace expose=0 if no_exp==1
replace expose=1 if low_exp==1
replace expose=1 if high_exp==1
replace expose=2 if all_exp==1xtile ach_q= std_ma, nq(4)
egen sbysch= group(wise_id lea_sch)
gen both=1
label define expose 0 "Segregated" 1 "Mainstream" 2 "Singleton" 
label values expose expose
label var no_exp "Extreme Isolation"
label var low_exp "Low Exposure"
label var high_exp "High Exposure"

save esms_all_`subject', replace


}

else {
use esms_all_`subject', clear

}

if `regress'== 1 {
 /********************************************************/
 /*Model 0: Regression, Fixed Effects- Student, Interactions   */
 /********************************************************/
 
 set more off
 local subject la
*Locals for regresions 
local y  std_`subject' // outcome

local treat b1.expose i.lxd_esl i.last_year_el // treatment

local int b1.expose#lxd_esl b1.expose#last_year_el // interactions
	
local mod cen_seg_w_islep cen_seg_w_waslep cen_pct_t_samerace  cen_s_everel_pct cen_s_lxd_esl_pct bilher_sch newdest  est_g est_s  rural topfive // modifiers

local student  pre_`subject' yrs_as_islep  islep male white asian black otherrace  ex_dis ex_aig  movediy movedpy force_move daysabs underage ///
	overage frpl num_students_mf   daysabs_mf movedpy_mf  ex_dis_mf  yrs_as_waslep_mf   pre_`subject'_mf  //student characteristics


local studentfe pre_`subject' yrs_as_islep  islep  ex_dis ex_aig  movediy movedpy force_move daysabs underage ///
	overage  num_students_mf   daysabs_mf movedpy_mf  ex_dis_mf  yrs_as_waslep_mf   pre_`subject'_mf  //for fixed effects models

local class num_students peer_`subject'  experience newteach   esl_flag curr_adv curr_rem   tstavg_all tstavg_all_mf exp3orlessyr_mf  experience_mf last_year_el_mf  // class characteristics

local school   adm adm_squ tot_ppx title3 full_licen  sch_nbc sch_advdeg  s_minority_pct  s_frpl_pct sch_nbc_mf full_licen_mf es //school characteristics
local fe yr1011 yr1112 yr1213 yr0910 // year dummies 

*create esample
 reg `y' `treat' `int' `mod'  `student' `class' `school' `fe' //full model
 *gen sample=e(sample)
di "Doing Sample Reg `subject'"


*output regression tables 

#delimit ;
esttab m0`var'`lev' m1`var'`lev' m2`var'`lev' m3`var'`lev' m4`var'`lev' m5`var'`lev' using models1__`var'_`subject'_top_`lev'.csv,  
        /* estout command: * indicates all estimates in memory. csv specifies comma sep, best for excel */
               label                          /*Use labels for models and variables */
               nodepvars                      /* Use my model titles */
               b(2)                           /* b= coefficients , this gives two sig digits */
               not                            /* I don't want t statistics */
               se(2)                         /* I do want standard errors */
               r2 (2)                      /* R squared */
               sfmt (2  0)               /* format for stats*/
               replace                   /* replace existing file */
               nogap 
			   nonumbers
			   interaction("*")
               nobaselevels
               noomitted 
               s( cont inte fixed  fixedst  r2 F N ,label("Controls" "Interactions" "School FE" "Student FE" "R^2" "F" "Observations" ))
			   ;

#delimit cr
 

*do regressios by el status and grade level

foreach var in  waslep islep   {
foreach lev in  both es ms { 

qui reg `y' `treat'  `int' if sample==1 & `var'==1 & `lev' ==1, vce(cluster lea_sch)
eststo m0`var'`lev', title("Model 0")
estadd local fixed " " , replace
estadd local fixedst " " , replace
estadd local inte " "
estadd local cont " "
di "Doing Model 0 `subject' `var'"


qui reg `y' `treat'  `student' `mod' `class' `school' `fe'  if sample==1 & `var'==1 & `lev' ==1, vce(cluster lea_sch)
eststo m1`var'`lev', title("Model 1")
estadd local fixed " " , replace
estadd local fixedst " " , replace
estadd local inte " "
estadd local cont "X"
di "Doing Model 1 `subject' `var'"



qui reg `y' `treat' `int' `student' `mod' `class' `school' `fe'  if sample==1 & `var'==1 & `lev' ==1, vce(cluster lea_sch)
eststo m2`var'`lev', title("Model 2")
estadd local fixed " " , replace
estadd local fixedst " " , replace
estadd local inte "X"
estadd local cont "X"
di "Doing Model 2 `subject' `var'"
}
}
xtset  leasch
qui xtreg `y' `treat' `int' `student' `mod' `class' `school'   if  `var'==1 & `lev' ==1,  fe vce(robust)
eststo m3`var'`lev', title("Model 3")
estadd local fixed "X" , replace
estadd local fixedst " " , replace
estadd local inte "X"
estadd local cont "X"
di "Doing Model 3 `subject' `var'"


xtset  wise_id
qui xtreg `y' `treat' `int' `studentfe' `mod' `class' `school'  if `var'==1 & `lev' ==1 ,  fe vce(robust)
eststo m4`var'`lev', title("Model 4")
estadd local fixed "" , replace
estadd local fixedst "X" , replace
estadd local inte "X"
estadd local cont "X"
di "Doing Model 4 `subject' `var'"


xtset  sbysch 
qui xtreg `y' `treat' `int' `studentfe' `mod' `class' `school'  if `var'==1 & `lev' ==1 ,  fe vce(robust)
eststo m5`var'`lev', title("Model 5")
estadd local fixed "X" , replace
estadd local fixedst "X" , replace
estadd local inte "X"
estadd local cont "X"
di "Doing Model 5 `subject' `var'"


*output regression tables 

#delimit ;
esttab m0`var'`lev' m1`var'`lev' m2`var'`lev' m3`var'`lev' m4`var'`lev' m5`var'`lev' using models1__`var'_`subject'_top_`lev'.csv,  
        /* estout command: * indicates all estimates in memory. csv specifies comma sep, best for excel */
               label                          /*Use labels for models and variables */
               nodepvars                      /* Use my model titles */
               b(2)                           /* b= coefficients , this gives two sig digits */
               not                            /* I don't want t statistics */
               se(2)                         /* I do want standard errors */
               r2 (2)                      /* R squared */
               sfmt (2  0)               /* format for stats*/
               replace                   /* replace existing file */
               nogap 
			   nonumbers
			   interaction("*")
               nobaselevels
               noomitted 
               s( cont inte fixed  fixedst  r2 F N ,label("Controls" "Interactions" "School FE" "Student FE" "R^2" "F" "Observations" ))
			   ;

#delimit cr
 
 ****fixed effects no interactions
 
xtset  leasch
qui xtreg `y' `treat'  `student' `mod' `class' `school'   if  `var'==1 & `lev' ==1,  fe vce(robust)
eststo m3`var'`lev'2, title("Model 1A")
estadd local fixed "X" , replace
estadd local fixedst " " , replace
estadd local inte ""
estadd local cont "X"

di "Doing MOdel 1A `subject' `var'"


xtset  wise_id
qui xtreg `y' `treat' `studentfe' `mod' `class' `school'  if `var'==1 & `lev' ==1 ,  fe vce(robust)
eststo m4`var'`lev'2, title("Model 1B")
estadd local fixed "" , replace
estadd local fixedst "X" , replace
estadd local inte ""
estadd local cont "X"

di "Doing MOdel 1B `subject' `var'"


xtset  sbysch 
qui xtreg `y' `treat'  `studentfe' `mod' `class' `school'  if `var'==1 & `lev' ==1 ,  fe vce(robust)
eststo m5`var'`lev'2, title("Model 1C")
estadd local fixed "X" , replace
estadd local fixedst "X" , replace
estadd local inte ""
estadd local cont "X"
di "Doing MOdel 1C `subject' `var'"


*output tables for no interactions 


#delimit ;
esttab  m1`var'`lev' m3`var'`lev'2   m4`var'`lev'2  m5`var'`lev'2 ///
 m2`var'`lev' m3`var'`lev' m4`var'`lev' m5`var'`lev'  using modelsfenoint__`var'_`subject'_top_`lev'.csv,  
        /* estout command: * indicates all estimates in memory. csv specifies comma sep, best for excel */
               label                          /*Use labels for models and variables */
               nodepvars                      /* Use my model titles */
               b(2)                           /* b= coefficients , this gives two sig digits */
               not                            /* I don't want t statistics */
               se(2)                         /* I do want standard errors */
               r2 (2)                      /* R squared */
               sfmt (2  0)               /* format for stats*/
               replace                   /* replace existing file */
               nogap 
               nonumbers
			   interaction("*")
               nobaselevels
               noomitted 
               s( cont inte fixed  fixedst  r2 F N ,label("Controls" "Interactions" "School FE" "Student FE" "R^2" "F" "Observations" ))
			   ;

#delimit cr
 
 }
 }
 



} /*close switch*/



if `describe'==1 {

set more off
local subject la
* locals for descriptive tables
local y  std_`subject'
local treat no_exp low_exp high_exp all_exp lxd_esl last_year_el 
local mod seg_w_islep seg_w_waslep s_everel_pct pct_t_samerace  s_lxd_esl_pct bilher_sch newdest  est_g est_s otherdest rural topfive otherurb
local student  pre_`subject' yrs_as_islep  islep male white asian black hispanic otherrace  ex_dis ex_aig  movediy movedpy force_move daysabs underage ///
	overage frpl 
local class num_students peer_`subject'   esl_flag curr_adv curr_rem      c_islep_pct c_waslep_pct c_hispanic_pct c_black_pct c_white_pct c_frpl_pct c_male_pct
local school es adm tot_ppx title3 full_licen  sch_nbc sch_advdeg  s_minority_pct  s_frpl_pct  
local fe yr1011 yr1112 yr1213 yr0910
  

  
*make descriptives by el status and grade level
 local subject la
foreach var in  lep_status {
foreach lev in  both { 

#delimit ;
eststo descriptives:
  qui estpost tabstat `y' `treat' `mod' `student' `class' `school' `fe'  if sample==1 & `lev'==1,
  by(`var')
  statistics(mean sd) /* Can add any stats here */
  columns(statistics) /*Each stat gets its own column */
;

*variables by el status 
#delimit ;
eststo descriptives:
  qui estpost tabstat `y' `treat' `mod' `student' `class' `school' `fe'  if sample==1 & `lev'==1,
  by(`var')
  statistics(mean sd) /* Can add any stats here */
  columns(statistics) /*Each stat gets its own column */
;

* ouput descriptives by el status
#delimit ; 
esttab descriptives using  "desc_analysis_`var'_`subject'_`lev'.rtf",
        main(mean 2) /* Use mean as main report */ 
        aux(sd 2) /*SD is reported underneath */
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
		nogap
		nobaselevels
		noomitted       
        title("Descriptives for Variables in Analysis")
        ;

#delimit cr   

}
}
* descrive classrooms with different levels of exposure
local class lxd_esl last_year_el num_students peer_`subject' es ms   esl_flag curr_adv curr_rem    experience newteach tstavg_all
#delimit ;
	eststo descriptives:
	qui estpost tabstat  `class' if sample==1 & class==1 & `lev'==1 & `var'==1,
	by(expose)
	statistics(mean sd) /* Can add any stats here */
	columns(statistics) /*Each stat gets its own column */
	;

* describe teachers in clssrooms with different levels of exposure 
#delimit ;
	eststo teach:
	qui estpost tabstat   lxd_esl last_year_el experience newteach tstavg_all if sample==1 & teacher==1 & `lev'==1 & `var'==1,
	by(expose)
	statistics(mean sd) /* Can add any stats here */
	columns(statistics) /*Each stat gets its own column */
	;

	
* output classroom descriptives 
#delimit ;

esttab descriptives teach using  "class_desc_`subject'_`lev'_`var'.rtf",
        main(mean 2) /* Use mean as main report */ 
        aux(sd 2) /*SD is reported underneath */
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
	nogap
	nobaselevels
	noomitted       
        title("Descriptives for Variables in Analysis")
        ;
		#delimit cr 
graph box std_`subject', over(expose, label(angle(45))) over(lep_status) nooutsides 
graph export ach_lxd_`lev'_`var'_`subject'.png, replace 


  
	} /* close level loop*/ 
	} /* close el status loop*/
 
 *teacher descriptives, student  unit 
 
 #delimit ;
	eststo teach:
	qui estpost tabstat   num_students peer_`subject'   esl_flag curr_adv curr_rem     ach_1 ach_2 ach_3 ach_4 c_everel_pct
	c_islep_pct c_waslep_pct c_hispanic_pct c_black_pct c_white_pct c_frpl_pct c_male_pct,
	by(lep_status)
	statistics(mean sd) /* Can add any stats here */
	columns(statistics) /*Each stat gets its own column */
	;

*output teacher descriptives 	
	esttab  teach using  "class_desc_`subject'_.rtf",
        main(mean 2) /* Use mean as main report */ 
        aux(sd 2) /*SD is reported underneath */
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
	nogap
	nobaselevels
	noomitted       
        title("Descriptives for Variables in Analysis")
        ;

		
#delimit cr 
local subject la
local class lxd_esl last_year_el experience newteach num_students peer_`subject' es   esl_flag curr_adv curr_rem    
#delimit ;
	eststo descriptives:
	qui estpost tabstat  `class' if sample==1 & class==1,
	by(expose)
	statistics(mean sd) /* Can add any stats here */
	columns(statistics) /*Each stat gets its own column */
	;
esttab descriptives  using  "class_desc_`subject'__all.rtf",
        main(mean 2) /* Use mean as main report */ 
        aux(sd 2) /*SD is reported underneath */
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
	nogap
	nobaselevels
	noomitted       
        title("Descriptives for Variables in Analysis")
        ;
		#delimit cr 
		
#delimit cr 


# delimit ;
  matrix t1_= J(27,4,.) ;
  matrix colnames t1_ = 
 "REL ELA"
 "REL Math"
 "CEL ELA"
 "CEL Math"
 ;
  matrix rownames t1_ = 
 "Pooled" "_"
 "ES" "_"
 "MS" "_"
 "Rural" "_"
 "Urban" "_"
 "Suburban" "_"
 "New Destination" "_"
 "Est Growing" "_"
 "Est Stable" "_"
 "Low Incidence" "_"
 "High Concentration" "_"
 "Medium Concentration" "_"
 "Low Concentration" "_"
 "N" 
 ;
  #delimit cr
set more off
  local j=1

foreach lep in  waslep islep{

foreach subject in  la ma {
  
  
  local i=1
  local k=2
    
  foreach year in /*"0506" "0607" "0708" "0809" "0910" "1011" "1112" */ "1213" {
    
  use esms_all_`subject', clear
  tab dis_type, gen(d)
  local achieve both es ms d1 d2 d3 newdest  est_g est_s otherdest  highelcon medelcon lowelcon    

  
  foreach x of local achieve {
          qui sum(std_`subject') if `x'==1 & `lep'==1 
           matrix t1_[`i',`j']= r(mean) 
           matrix t1_[`k',`j']= r(sd) 
           
   local i=`i'+2
   local k=`k'+2
   qui count if `x'==1 & `lep'==1 
           matrix t1_[27,`j']=r(N)
           
  }
           
  local i=1
   local k=2       
   local j=`j'+1
   
  } /*close year loop*/
  } /*close subject*/
  } /*close lep*/ 
   putexcel A1=matrix(t1_, names) using achieve_desc.csv, modify 
    } /*close table*/

  


#delimit ;
	eststo descriptives:
	 estpost tabstat  std_la ,
	by(lep_status)
	statistics(mean sd) /* Can add any stats here */
	columns(statistics) /*Each stat gets its own column */
	;
esttab descriptives  using  "class_desc_`subject'__all.rtf",
        main(mean 2) /* Use mean as main report */ 
        aux(sd 2) /*SD is reported underneath */
        unstack /* Want them in one column */
        nonote /*No note */
        label /*Use value labels */
        replace /*Replace if it exists */  
	nogap
	nobaselevels
	noomitted       
        title("Descriptives for Variables in Analysis")
        ;
		#delimit cr 		
		
} /*close descriptives*/

if `graphs'==1 {

******GRAPHS*********
* plot betas islep both
	est restore m2islepboth
	#delimit;

	plotbeta cen_pct_t_samerace  | cen_s_everel_pct | newdest |  est_g | est_s |
	cen_s_lxd_esl_pct | cen_seg_w_islep | cen_seg_w_waslep | bilher_sch,
          xtitle (Parameter Estimates)                 /*Label of x axis*/
          xline(0,lp(dash)) /* Line at 0: if95% ci crosses, not stat sig */
          xscale(range(-6 6)) /* Range of X axis*/
          xtick(-6 (.5) 6)
	  xlabel(-6 (2) 6)
          labels
          ;
	  
	  graph export plotbeta_school_islepla.png, replace

*plot beta waslep both	  
	est restore m2waslepboth
	#delimit;

	plotbeta cen_pct_t_samerace  | cen_s_everel_pct | newdest |  est_g | est_s |
	cen_s_lxd_esl_pct | cen_seg_w_islep | cen_seg_w_waslep | bilher_sch,
          xtitle (Parameter Estimates)                 /*Label of x axis*/
          xline(0,lp(dash)) /* Line at 0: if95% ci crosses, not stat sig */
          xscale(range(-6 6)) /* Range of X axis*/
          xtick(-6 (.5) 6)
		  xlabel(-6 (2) 6)
          labels
          ;
	  graph export plotbeta_school_waslepla.png, replace

	  
* make line graph of std_subject and exposure
  # delimit ;
  
  graph twoway qfit std_`subject' exp if islep==1 ||  qfit std_`subject' exp if waslep==1, 
	ytitle("Std. Score")
	xtitle("Exposure")
	legend(label(1 "Current Els") label(2 "Former Els"))	
	;
  
  graph export exp_ach_`subject'.png, replace ;


*Make box grap of std ach lxd esl 
	graph box std_`subject', over(lxd_esl) over(lep_status) ;
	graph export ach_lxd_`subject'.png, replace ;

	graph box std_`subject', over(last_year_el) over(lep_status) ;
	graph export ach_last_`subject'.png, replace ;

	graph box std_`subject', over(ach_q) 
	;
	graph export ach__`subject'.png, replace ;

	#delimit cr

* plot of relationship ebtwee epposure and achievment

twoway lfit std_la exp if islep==1 || lfit std_la exp if islep==0, ///
	legend(label(1 "Current Els") label(2 "Reclassified Els"))  ///
	xtitle(Exposure) ytitle(Std Achievment) 
	graph export la_achieve_leps.png, replace

* make kdesnity plot of exposure
	kdensity exel2not, nograph generate(x fx)
	kdensity exel2not if islep==0, nograph generate(fx0) at(x)
	kdensity exel2not if islep==1, nograph generate(fx1) at(x)
	label var fx0 "Former Els"
	label var fx1 "Current Els"
	line fx0 fx1 x, sort ytitle(Density) xtitle(Exposure) xline(.1) xline(.75) xline(.9)
	graph export exp_dens_`subject'_alt.png, replace

*peer acheivement over time

gen peerlais=.
gen peerlawas=.
forval num  = 4/8 {
sum peer_la if islep==1 & grade==`num'
replace peerlais=r(mean) if grade==`num'
sum peer_la if islep==0 & grade==`num'
replace peerlawas=r(mean) if grade==`num'
}

graph twoway connected peerlais grade ||   connected peerlawas grade  , sort 


graph pie islep if es==0 & islep==1 & expose==1, ///
over(yrs_as_islep) ///
plabel(_all percent, format("%2.0f") gap(5)) ///
legend(position(9) cols(1)  title("Years as CEL") label(6 "5+"))

*school historgram
 histogram s_everel_pct, percent ///
ytitle("Percent of NC Schools") xtitle("% of Els in School") color(ltbluishgray) xline(.10, lpattern(dash)) ///
 xline(.30,lpattern(dash)) xtick(0(.1)1) ///
  lcolor(gray) title("Proportion of Els in NC Schools")


} /*close graphs switch */


} /*close big loop*/



set more off
 local subject ma
*Locals for regresions 
local y  std_`subject' // outcome

local treat b1.expose i.lxd_esl i.last_year_el // treatment

local int b1.expose#lxd_esl b1.expose#last_year_el // interactions
	
	
local mod cen_seg_w_islep cen_seg_w_waslep cen_pct_t_samerace  cen_s_everel_pct cen_s_lxd_esl_pct bilher_sch newdest  est_g est_s  rural topfive // modifiers

local student  yrs_as_islep  islep male white asian black otherrace   movedpy force_move daysabs underage ///
	overage frpl num_students_mf   daysabs_mf movedpy_mf  ex_dis_mf  yrs_as_waslep_mf   pre_`subject'_mf  //student characteristics


local studentfe pre_`subject' yrs_as_islep  islep  ex_dis ex_aig  movediy movedpy force_move daysabs underage ///
	overage  num_students_mf   daysabs_mf movedpy_mf  ex_dis_mf  yrs_as_waslep_mf   pre_`subject'_mf  //for fixed effects models

local class num_students peer_`subject'  experience newteach   esl_flag  tstavg_all tstavg_all_mf exp3orlessyr_mf  experience_mf last_year_el_mf  // class characteristics

local school   adm adm_squ tot_ppx title3 full_licen  sch_nbc sch_advdeg  s_minority_pct  s_frpl_pct sch_nbc_mf full_licen_mf es //school characteristics
local fe yr1011 yr1112 yr1213 yr0910 // year dummies 

*create esample
 reg `y' `treat' `mod'  `student' `class' `school' `fe' //full model
 *gen sample=e(sample)
 estimates store base


*output regression tables 

#delimit ;
esttab base using base_`subject'_.csv,  
        /* estout command: * indicates all estimates in memory. csv specifies comma sep, best for excel */
               label                          /*Use labels for models and variables */
               nodepvars                      /* Use my model titles */
               b(2)                           /* b= coefficients , this gives two sig digits */
               not                            /* I don't want t statistics */
               se(2)                         /* I do want standard errors */
               r2 (2)                      /* R squared */
               sfmt (2  0)               /* format for stats*/
               replace                   /* replace existing file */
               nogap 
		nonumbers
		interaction("*")
               nobaselevels
               noomitted 
               s( cont inte fixed  fixedst  r2 F N ,label("Controls" "Interactions" "School FE" "Student FE" "R^2" "F" "Observations" ))
			   ;

#delimit cr
 
