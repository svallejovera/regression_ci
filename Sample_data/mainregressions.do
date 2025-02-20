** ===================================================================
**
** MainRegression.do
** 
** This do file produces the majority of the results in the paper:
** "Would People Behave Differently If They Better Understood Social 
**  Security? Evidence From a Field Experiment" **** by Jeffrey B. Liebman and Erzo F.P. Luttmer
**
** To be published in AEJ-Policy 2014/2015.
**
** The raw data is in socsec_data.dta
**
** The results relating to the family-wise error rate (FWER) are 
** created by fwer_shell.do  (which calls on fwer_core.do)
**
** The CPS summary stats data reported in Appendix Table 2 are 
** calculated in cps_sumstats.do
**
** p-values on differences between non-overlapping samples are
** generated within the Excel table file
**
** ===================================================================


clear
set more 1
capture log close
log using mainregressions.log, replace

** ===============================================================
** HOW TO QUICKLY FIND THE REGRESSION FOR A PARTICULAR TABLE ENTRY
** ===============================================================
**
** To quickly find the code that belongs to a certain table entry:
** search: 'tabXXrYY'
**
** where XX is the two-digit table number
** and YY is the two-digit row number
** e.g. search 'tab04r11'  for table 4 row 11


** ================
** 1. PRELIMINARIES
** ================

use socsec_data.dta

** 1.A Data Cleaning of Study-Related Characteristics
** ==================================================

** create a dummy variable for treatment group
gen treat=(treatgroup==1)
label var treat "1=received information treatment"

** create a dummy variable for having completed the follow-up survey
gen byte completed_yes=completed==1

** create variables for study completion status
** 1 -- no longer part of KN panel as of April 2010
** 2 -- part of KN panel as of April 2010, but not invited 
**          to take 2010 follow-up survey
** 3 -- invited to take 2010 follow-up survey, but did not respond at all
** 4 -- started the 2010 follow-up survey, but did not complete it
** 5 -- started the 2010 follow-up survey, but did not qualify (was  
**           directed to standard close due to the if statement in Q1.8)
** 6 -- completed the 2010 follow-up survey.

gen byte cs_1=completion_status==1
gen byte cs_2=completion_status==2
gen byte cs_3=completion_status==3
gen byte cs_4=completion_status==4
gen byte cs_5=completion_status==5
gen byte cs_6=completion_status==6

** create dummy variable for members invited to the follow-up survey
gen byte invited=completion_status>=3&completion_status<=6

** create dummy variable for active members invited to the follow-up survey 
gen active_invited=active*invited

** create dummy variables for treatment recollection (brochure and module)
gen recall_b_yes=recall_b==1 if recall_b<.
gen recall_m_yes=recall_m==1 if recall_m<.

** rename variables and recode responses
** to be consistent with the names in the 2008 survey
rename ext_incent_mc q3_1
rename ext_incent_amt q3_2
rename ext_incent2_amt q3_2a
rename ext_incent3_amt q3_2b
recode q3_1  -1=.
recode q3_2  -1=.
recode q3_2a -1=.
recode q3_2b -1=.
label val q3_1 samehighlow

** create variable marking incentive to work based upon responses
gen q3_1posincent=q3_1==3 if q3_1<.

** recode and label variables for perceived incentive to work longer
** considering social security
recode incent_wk_lng -1=.
gen incentwork=4-incent_wk_lng
gen incentworkpos=incentwork==3 if incentwork<.
label var incentwork "Incentives for working more years"
label def incentwork 1 "SS worse deal if working longer"  2 "SS same deal if working longer"  3 "SS better deal if working longer"
label val incentwork incentwork
gen    incentworkpos2=incentworkpos
replace incentworkpos2=0 if q3_1posincent==0

** Knowledge of the 35-year rule
** Many people filled in a number in exactly one of the options, but forgot to press the radio button.
** If exactly one blank is filled out, infer their choice of option from that.
recode rule35_choice -1=.
gen rule35_mc = rule35_choice
replace rule35_mc = 1 if rule35_choice==. & rule35_amount1~=-1 & rule35_amount2==-1 & rule35_amount3==-1 & rule35_amount4==-1
replace rule35_mc = 2 if rule35_choice==. & rule35_amount1==-1 & rule35_amount2~=-1 & rule35_amount3==-1 & rule35_amount4==-1
replace rule35_mc = 3 if rule35_choice==. & rule35_amount1==-1 & rule35_amount2==-1 & rule35_amount3~=-1 & rule35_amount4==-1
replace rule35_mc = 4 if rule35_choice==. & rule35_amount1==-1 & rule35_amount2==-1 & rule35_amount3==-1 & rule35_amount4~=-1

** create a variable that measures whether the respondents know the 35-year rule
gen byte rule35_mc_correct=rule35_mc==2 if completed==1
label var rule35_mc_correct "Respondent knows SS benefits are based on X number of highest earning years"

** create variables to indicate for qualitatively correct answer on q5_2
** (What happens to SS benefits by delaying claiming).
gen delayqual62_66 = 1 + (agedep_rate2 >= agedep_rate1) + (agedep_rate2 > agedep_rate1) if agedep_rate2<. & agedep_rate1<.
gen delayqual66_70 = 1 + (agedep_rate3 >= agedep_rate2) + (agedep_rate3 > agedep_rate2) if agedep_rate3<. & agedep_rate2<.
gen delayqual70_74 = 1 + (agedep_rate4 >= agedep_rate3) + (agedep_rate4 > agedep_rate3) if agedep_rate4<. & agedep_rate3<.
label val delayqual62_66 lowsamehigh
label val delayqual66_70 lowsamehigh
label val delayqual70_74 lowsamehigh
gen byte delay62_66correct=delayqual62_66==3 if delayqual62_66<.
gen byte delay66_70correct=delayqual66_70==3 if delayqual66_70<.
gen byte delay70_74correct=delayqual70_74==2 if delayqual70_74<.



** Create a variable for planned or realized claim age based on the 
** point-estimate version of the question
** Also add the claim month if given.
recode claim_age_orig -1=.
recode claim_mth -1=.
gen ageclaim=claim_age_orig
replace ageclaim=ageclaim+claim_mth/12 if claim_mth>=0&claim_mth<=12

** Those who were already claiming prior to our intervention cannot possibly have been
** affected by our intervention.  Exclude those from the analysis by setting the
** claim age to missing.
replace ageclaim=. if  m_ss_status==1       /* those claiming at the baseline survey */
replace ageclaim=. if ageclaim<m_ppage      /* claim age less than age at the baseline survey */


** --------- Create expected values and standard deviations for Bin/Ball questions -----------

** In Section 1B of the post-intervention survey (2010) we elicit subjective probability
** distribution of own/spousal social security claim ages and retirement ages. These 
** questions provide the respondents with 20 balls which they can assign to 9 different age bins, based
** on the likelihood that they (and/or their spouse) will claim social security and/or retire at that age. 
**
** In the code below, we create variables to hold the expected value of claim/retirement age
** given the "subjective probability distribution" of the age at which they plan to retire and/or 
** claim social security benefits. 

** first check that probabilities add up to one (so that 20 balls were allocated)
egen fullpdf_claim=rowtotal(pdf_claim_age_bin*)
tab fullpdf_claim, m

egen fullpdf_ret=rowtotal(pdf_ret_age_bin*)
tab fullpdf_ret, m


** create weight*X and weight*X^2 if all 20 balls were allocated
forvalues i = 1/9 {
	gen pdf_claim_age_binX_`i'  = (60 + `i')            * pdf_claim_age_bin`i'/20 if fullpdf_claim==20
	gen pdf_claim_age_binX2_`i' = (60 + `i')^2          * pdf_claim_age_bin`i'/20 if fullpdf_claim==20
	gen pdf_ret_age_binX_`i'    = (bin_min + `i' - 1)   * pdf_ret_age_bin`i'/20   if fullpdf_ret  ==20
	gen pdf_ret_age_binX2_`i'   = (bin_min + `i' - 1)^2 * pdf_ret_age_bin`i'/20   if fullpdf_ret  ==20
}	

** Generate the expected value of expected retirement/claim ages based on subjective distribution
** Algebraically: Expected Age = Sum Over k [[(prob. at age k)*(age k)]] where k in [61,69]
** for claim age and k in [bin_min, bin_min+8] for retirement age, where bin_min is known.
** Now (prob at age k) = (balls in bin at age k)/(total balls)
** Thus, the full formula is: Expected Age = 1/20*(balls in bin at age k)*(age k)

** Do this for claim age
egen expval_claim_age = rowtotal(pdf_claim_age_binX_*)
label var expval_claim_age "Exp.val. claim age based on self-reported dist. of claim ages"
recode expval_claim_age 0=.
sum expval_claim_age, d

** Do this for retirement age
egen expval_ret_age = rowtotal(pdf_ret_age_binX_*)
label var expval_ret_age "Exp. val. of ret. age based on self-reported dist. of ret. ages"
recode expval_ret_age 0=. 
sum expval_ret_age, d

** Now calculate the standard deviation of responses
** SQRT(SUM w*X^2  - E[X]^2)
egen sd_claim_age    = rowtotal(pdf_claim_age_binX2_*)
replace sd_claim_age = sqrt(sd_claim_age - expval_claim_age^2)
label var sd_claim_age "SD of claim age based on self-reported dist. of claim ages"
sum sd_claim_age, d

egen sd_ret_age    = rowtotal(pdf_ret_age_binX2_*)
replace sd_ret_age = sqrt(sd_ret_age - expval_ret_age^2)
label var sd_ret_age "SD of ret age based on self-reported dist. of claim ages"
sum sd_ret_age, d

drop pdf_claim_age_binX* pdf_ret_age_binX*

** Now similar to above, we caclulate the expected retirement/claim age (and S.D.) for spouse 
** first check that probabilities add up to one (so that 20 balls were allocated)
egen fullpdf_claim_s=rowtotal(pdf_claim_age_s_bin*)
tab fullpdf_claim_s, m

egen fullpdf_ret_s=rowtotal(pdf_ret_age_s_bin*)
tab fullpdf_claim_s, m

** create weight*X and weight*X^2 if all 20 balls were allocated
forvalues i = 1/9 {
	gen pdf_claim_age_s_binX_`i'  = (60 + `i')            * pdf_claim_age_s_bin`i'/20 if fullpdf_claim_s==20
	gen pdf_claim_age_s_binX2_`i' = (60 + `i')^2          * pdf_claim_age_s_bin`i'/20 if fullpdf_claim_s==20
	gen pdf_ret_age_s_binX_`i'    = (bin_min + `i' - 1)   * pdf_ret_age_s_bin`i'/20   if fullpdf_ret_s  ==20
	gen pdf_ret_age_s_binX2_`i'   = (bin_min + `i' - 1)^2 * pdf_ret_age_s_bin`i'/20   if fullpdf_ret_s  ==20
 }	

** Generate the expected value of expected retirement/claim ages based on subjective distribution
** Algebraically: Expected Age = Sum Over k [[(prob. at age k)*(age k)]] where k in [61,69]
** for claim age and k in [bin_min, bin_min+8] for retirement age, where bin_min is known.
** Now (prob at age k) = (balls in bin at age k)/(total balls)
** Thus, the full formula is: Expected Age = 1/20*(balls in bin at age k)*(age k)

** Do this for claim age
egen expval_claim_age_s = rowtotal(pdf_claim_age_s_binX_*)
label var expval_claim_age_s "Exp.val. claim age spouse based on self-reported dist. of claim ages"
recode expval_claim_age_s 0=.
sum expval_claim_age_s, d

** Do this for retirement age
egen expval_ret_age_s = rowtotal(pdf_ret_age_s_binX_*)
label var expval_ret_age_s "Exp. val. of ret. age spousebased on self-reported dist. of ret. ages"
recode expval_ret_age_s 0=. 
sum expval_ret_age_s, d

** Now calculate the standard deviation of responses
** SQRT(SUM w*X^2  - E[X]^2)

egen sd_claim_age_s    = rowtotal(pdf_claim_age_s_binX2_*)
replace sd_claim_age_s = sqrt(sd_claim_age_s - expval_claim_age_s^2)
label var sd_claim_age_s "SD of claim age spouse based on self-reported dist. of claim ages"
sum sd_claim_age_s, d

egen sd_ret_age_s    = rowtotal(pdf_ret_age_s_binX2_*)
replace sd_ret_age_s = sqrt(sd_ret_age_s - expval_ret_age_s^2)
label var sd_ret_age_s "SD of ret age spouse based on self-reported dist. of claim ages"
sum sd_ret_age_s, d

drop pdf_claim_age_s_binX* pdf_ret_age_s_binX*



** Expectation and S.D. of claim age based on the bins/balls version of the question.
** Create alternative versions of these variables that replaces the expectation 
** with the realization for those that have already started claiming, and sets
** the S.D. for these people.
gen     expval_claim_age2 = expval_claim_age
replace expval_claim_age2 = ageclaim if ageclaim<=ppage  
gen     sd_claim_age2 = sd_claim_age
replace sd_claim_age2 = 0 if ageclaim<=ppage  /* no uncertainty about the claim date if already claiming */

** Those who were already claiming prior to our intervention cannot possibly have been
** affected by our intervention.  Exclude those from the analysis by setting the
** claim age to missing.
replace expval_claim_age2=. if  m_ss_status==1  /* those claiming at the baseline survey */
replace expval_claim_age2=. if ageclaim<m_ppage /* claim age less than age at the baseline survey */
replace sd_claim_age2=. if  m_ss_status==1
replace sd_claim_age2=. if ageclaim<m_ppage

** recode missings in questions concerning longevity expectations
recode prob_liv_ind -1=.
recode prob_liv_cpl -1=.

** adjust q3_2 if R indicated in the follow-up (q3_2a or q3_2b) that they had made a mistake
replace q3_2 = ben_level + q3_2 if q3_2a==1
replace q3_2 = ben_level - q3_2 if q3_2b==1

** calculate the perceived percentage increase to benefits for working one FEWER year
gen     q3_1_ben = ben_level        if q3_1 == 1
replace q3_1_ben = q3_2             if q3_1 == 2 | q3_1 == 3
gen     q3_1_per = 100*(q3_1_ben - ben_level)/ben_level

** adjust for the number of years of earlier retirement asked in the question
gen      q3_1_ppy = q3_1_per     if xret_chg==1
replace  q3_1_ppy = q3_1_per/2   if xret_chg==2
replace  q3_1_ppy = q3_1_per/5   if xret_chg==5

** for ease of readability in the Table reverse code
** So that is now becomes the perceived percentage increase to benefits for working one MORE year 
gen q3_1_incentpy=-q3_1_ppy

** top and bottom code this variable at +/- 25% so that outliers cannot drive it
recode q3_1_incentpy min/-25=-25 25/max=25

** format responses and variables concerning claim age
recode agedep_ny -1=.
label def alt_when 1 "1yr later" 2 "1yr earlier", modify
label val alt_when alt_when

** create a variable for the effect of later claiming on benefits
gen claimlater = agedep_ny
label var claimlater "Effect of delayed claiming on benefits"
label val claimlater samehighlow
recode claimlater 2=3 3=2 if alt_when==2
gen byte claimlater_pos=claimlater==2 if claimlater<.

** create a variable for the percentage of SS benefit increase by age
** as a fraction of benefits at age 66 (is roughly FRA for most of our sample)
** top and bottom code this variable at +/- 25% so that outliers cannot drive it
** All percentages as expressed as percentages of benefits at the full-retirement age
** (following the convention of the SSA)
gen delayrat62_66 = 25  *(agedep_rate2 - agedep_rate1) / agedep_rate2  
gen delayrat66_70 = 25  *(agedep_rate3 - agedep_rate2) / agedep_rate2  
gen delayrat70_74 = 25  *(agedep_rate4 - agedep_rate3) / agedep_rate2  
recode delayrat62_66 min/-25=-25 25/max=25   
recode delayrat66_70 min/-25=-25 25/max=25   
recode delayrat70_74 min/-25=-25 25/max=25  


** Knowledge about existence of earningstest at $20,000 p.y.
**
** Unlike the 2008 survey, xearntest=1: So we ask about earnings at age 64 for
** everyone. This means that for all respondents so that the correct answer 
** is always that benefits would be cut if earnings while claiming are $20,000
** (In contrast, in the 2008 survey, xearntest=2 (asking about earnings test at age 68) for 50% of Rs).
recode earntest_thresa -1=.
recode earntest_thresb -1=.
recode earntest_thresa_sp -1=.
recode earntest_thresb_sp -1=.
recode earntest_exist -1=.
label val earntest_exist samehighlow
gen byte earntest_exist_correct=earntest_exist==3 if earntest_exist<.

** Those who think that benefits wouldn't get cut at 20,000
** but that they would get cut at a higher amount are also aware of the earnings test 
** (even if they don't have the cutoff exactly correct.  Include those in this measure.
gen byte earntest_exist_correct2=earntest_exist==3|(earntest_exist==1&(earntest_thresa<999999|earntest_thresa_sp==1)) if earntest_exist<.

** Knowledge that benefits cut due to the earnings test are returned as higher future benefits
recode earntest_return -1=.
label val earntest_return samehighlow
gen byte earntest_return_correct=earntest_return==2 if earntest_return<.

** Format responses concerning expected retirement age (point estimate)
** and bottomcode retirement age at the age at the time of the baseline
** survey because everyone at the baseline survey in the treatment group
** was working then.
recode ret_age_orig -1=.
recode ret_mth -1=.
gen ageret=ret_age_orig
replace ageret=ageret+ret_mth/12 if ret_mth>=0&ret_mth<=12
replace ageret=m_ppage if ageret<m_ppage

** Expectation and S.D. of retirement age based on the bins/balls version of the question.
** Create alternative versions of these variables that replaces the expectation 
** with the realization for those that have already retired.
gen     expval_ret_age2 = expval_ret_age
replace expval_ret_age2 = ageret if ageret<=ppage
gen     sd_ret_age2 = sd_ret_age
replace sd_ret_age2 = 0 if ageret<=ppage   /* no uncertainty about retirement age if already retired */

** format responses for likelihood of working after claiming SS
recode ss_work -1=.
gen ss_work_yes=ss_work==1|ss_work==2 if ss_work<.



** 1.B Data Cleaning of Demographic Characteristics
** ================================================

** All demographics are taken from the baseline survey
** (so variables with prefix m_*) to rule out the possibility
** that the treatment might have affected their values.
** The exception is age, which is taken from the follow-up
** survey for expositional clarity (and because the treatment
** logically could not affect age).


** Age
assert ppage<.
gen ppage_sq=ppage^2
gen byte agedum_60m= ppage<=60
gen byte agedum_61 = ppage==61
gen byte agedum_62 = ppage==62
gen byte agedum_63 = ppage==63
gen byte agedum_64 = ppage==64
gen byte agedum_65 = ppage==65
gen byte agedum_66 = ppage==66
gen byte agedum_67p= ppage>=67

** Race/ethnicity of respondent
gen byte white=m_ppethm==1
gen byte black=m_ppethm==2
gen byte other=1-black-white
label var white "Non-Hispanic White"
label var black "Non-Hispanic Black"
label var other "Panelist is not Non-Hispanic white or black"

** Education level
gen byte edudo  = m_ppeducat==1
gen byte eduhs  = m_ppeducat==2
gen byte edusc  = m_ppeducat==3
gen byte educp  = m_ppeducat==4
assert (edudo+eduhs+edusc+educp)==1

** Marital status and gender
gen byte xmarried  = m_ppmarit==1
gen byte xwidow    = m_ppmarit==2
gen byte xdivorce  = m_ppmarit==3
gen byte xseparate = m_ppmarit==4
gen byte xnevmarr  = m_ppmarit==5
gen byte xlivpart  = m_ppmarit==6
assert (xmarried+xwidow+xdivorce+xseparate+xnevmarr+xlivpart)==1

gen byte female  = m_ppgender==2
gen byte male    = m_ppgender==1
assert male+female==1

** Region of residence
gen byte nrtheast = m_ppreg4==1
gen byte midwest  = m_ppreg4==2
gen byte south    = m_ppreg4==3
gen byte west     = m_ppreg4==4
assert (nrtheast+midwest+south+west)==1

** Household size
gen xhhsize1=m_pphhsize==1
gen xhhsize2=m_pphhsize==2
gen xhhsize3=m_pphhsize>=3&m_pphhsize<.

** HH Income dummies
assert m_ppincimp<.
gen inc00_25  = (m_ppincimp>=1  & m_ppincimp<=7)
gen inc25_50  = (m_ppincimp>=8  & m_ppincimp<=11)
gen inc50_75  = (m_ppincimp>=12 & m_ppincimp<=13)
gen inc75_100 = (m_ppincimp>=14 & m_ppincimp<=15)
gen inc100p   = (m_ppincimp>=16 & m_ppincimp<=.)

** Put all standard controls for the baseline regression in the global "xbasic"
global xbasic black other edudo eduhs edusc xwidow xdivorce xseparate xnevmarr xlivpart female midwest south west xhhsize2 xhhsize3  

** The four key outcome variables

** 1. Did any paid work in the past month
gen paid_work_yes = paid_work==1 if paid_work<. & paid_work~=-1
label var paid_work_yes "Did any paid work in the past month"

** 2. Working hours in the past month
gen work_hrs=paidwork_hrs if paidwork_hrs~=-1
recode work_hrs 1=5 2=20 3=40 4=60 5=80 6=100 7=120 8=140 9=160 10=180 11=200 12=220 13=240 14=260
replace work_hrs=0 if paid_work_yes==0
label var work_hrs "Number of hours worked for pay in the past month"

** 3. Earnings in the past month
gen work_amt=paidwork_amt if paidwork_amt~=-1
recode work_amt 1=125 2=375 3=625 4=875 5=1125 6=1375 7=1750 8=2250 9=2750 10=3500 11=4500 12=5500 13=6500 14=7500 15=8500 16=9500 17=11250 18=13750 19=30000
replace work_amt=0 if paid_work_yes==0

** 4. Dummy for claiming SS
** create dummy variable for claiming SS for the sample that:
**   - did the followup
**   - did not claim in 2008  (otherwise intervention could not possibly have an effect)
**   - is 60 or older in 2010 (otherwise intervention could not possibly have an effect)
gen ss_claim_no=ss_status==2 if ss_status<. & m_ss_status~=1 & ppage>=60
label var ss_claim_no "Not claiming Social Security Benefits (if age>60 and did not claim at baseline)"

** As a robustness check, create an unconditional dummy for claiming benefits
gen ss_claim_no2 = ss_status==2 if ss_status<.
label var ss_claim_no2 "Not claiming Social Security Benefits (unconditional)"


** Drop the observations where the age variable changed more than is logically possible 
**
** Logically, people can only have aged 1 or 2 years between nov/dec 2008 and mar/may 2010
** Drop those with larger or smaller age differences
gen agedif=ppage-m_ppagegen agekeep=agedif==1|agedif==2 if agedif<.
tab agekeep

** There are 7 cases in the entire (2483 obs) sample where age changed more than is logically
** possible, but only one such case among those who completed the follow up survey
list caseid completed treat m_ppage ppage agedif m_ppeducat ppeducat if agekeep==0

tab agedif completed, m

** save a version with the age inconsistencies to allow us to accurately measures
** response rates and attrition
save Data_WithAgeInconsistency, replace

keep if agekeep


** ------------ END PRELIMINARIES ------------ **


** Note: all p-values on coefficeint differences between subsamples (e.g., differences between the
**       male and female coefficient) were calculated in Excel based on the p-value of
**       the z-score = (coef1 - coef2)/sqrt(se1^2+se2^2), where coef1 and se1 are the 
**       coefficient and standard error in the first subsample and coef2 and se2 are 
**       the coefficient and standard error in the second subsample.  All subsamples
**       are independent (and don't overlap) so that the estimates are uncorrelated 
**       across the subsamples.


** ==========================
** 2. Sample Statistics
** ==========================

** ---------------------------------------------------------------------------
** Table 1: Summary Statistics
** ---------------------------------------------------------------------------
** tab01r00

** Full Sample Demographic Summary Statistics
sum paid_work_yes work_hrs work_amt ss_claim_no ppage female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p if completed==1, sep(0)

** Column 1: Women's Demographic Summary Statistics
sum paid_work_yes work_hrs work_amt ss_claim_no ppage female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p if completed==1&female==1, sep(0)

** Column 2: Men's Demographic Summary Statistics
sum paid_work_yes work_hrs work_amt ss_claim_no ppage female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p if completed==1&female==0, sep(0)



** ---------------------------------------------------------------------------
** Table 2: Manipulation Checks
** ---------------------------------------------------------------------------

** Recollection of treatment, the social security web tutorial and brochure
** among the treated sample


** Row 1: Recollection of Social Security Brochure
** -----------------------------------------------
** tab02r01

** Control Means:
tab recall_b_yes female if treat==0, col

** Column 1: Among the entire treated sample
reg recall_b_yes treat  ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg recall_b_yes treat  ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg recall_b_yes treat  ppage ppage_sq $xbasic if female==0, robust


** Row 2: Recollection of Social Security Web-Tutorial
** ---------------------------------------------------
** tab02r02

** Control Means:
tab recall_m_yes female if treat==0, col

** Column 1: Among the entire sample
reg recall_m_yes treat  ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg recall_m_yes treat  ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg recall_m_yes treat  ppage ppage_sq $xbasic if female==0, robust



** ==========================
** 3. Main Results
** ==========================

** ---------------------------------------------------------------------------
** Table 3: Treatment Effects on Behavior
** ---------------------------------------------------------------------------

** PANEL A: Rows 1-3: Primary outcome measures
** -------------------------------------------

** Row 1: Did paid work last month (2010)
** ---------------------------------------
** tab03r01

** Control Means:
tab paid_work_yes female if treat==0, col

** Column 1: Among the entire sample
reg paid_work_yes treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg paid_work_yes treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg paid_work_yes treat ppage ppage_sq $xbasic if female==0, robust


** Row 2: Hours worked for last month (2010)
** -----------------------------------------
** tab03r02

** Column 1: Among entire sample
sum work_hrs if treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic, robust

** Column 2: Among female responents
sum work_hrs if female==1 & treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum work_hrs if female==0 & treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic if female==0, robust


** Row 3: Earnings in last month, in dollars/month (2010)
** ------------------------------------------------------
** tab03r03

** Column 1: Among entire sample
sum work_amt if treat==0, d
reg work_amt treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum work_amt if female==1 & treat==0, d
reg work_amt treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum work_amt if female==0 & treat==0, d
reg work_amt treat ppage ppage_sq $xbasic if female==0, robust


** Row 4: Not receiving Social Security benefis in 2010
** ----------------------------------------------------
** tab03r04

** Control Means:
tab ss_claim_no female if treat==0, col

** Column 1: Among entire sample
reg ss_claim_no treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ss_claim_no treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg ss_claim_no treat ppage ppage_sq $xbasic if female==0, robust


** PANEL B: Rows 5-6: Indicies of outcome measures
** -----------------------------------------------
** tab03r05 

** Create standardized index of the three labor market outcome variables.

** drop observations that have all missings outcome vbls (or any missing controls)
capture drop allmiss
gen byte total=1
gen byte allmiss=1
foreach lhsvar in paid_work_yes work_hrs work_amt {
    
    ** the original regression (double check that it corresponds to our table)
    quietly reg `lhsvar' treat ppage ppage_sq $xbasic, robust
    ** note whether observation is used in the regression
    quietly replace allmiss=0 if e(sample)
    }


foreach smp in total female male {
  foreach vbl of varlist paid_work_yes work_hrs work_amt {
    
    ** standardize
    sum `vbl' if treat==0 & `smp' & allmiss==0
    gen std_`vbl'_`smp' = (`vbl' - r(mean))  / r(sd) if `smp' & allmiss==0

  }
  ** create the index
  egen ix3_`smp'=rowmean(std_*_`smp') if allmiss==0
  
  ** standardize index so that it is more easily interpretable
  sum ix3_`smp' if treat==0 & `smp' & allmiss==0
  replace ix3_`smp' = (ix3_`smp' - r(mean))  / r(sd) if `smp' & allmiss==0
  
  di ""
  di "Sum stats for `smp'"
  sum ix3_`smp' if `smp'

  di ""
  di "Sum stats for `smp' and control (should be mean 0, std 1 for std_*)"
  sum ix3_`smp' if `smp' & treat==0

  di ""
  di "Sum stats for `smp' and treatment"
  sum ix3_`smp' if `smp' & treat==1
  drop std_*_`smp' 
}
drop allmiss

** Index of three labor market outcomes

** Column 1: Among entire sample
reg ix3_total  treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ix3_female treat ppage ppage_sq $xbasic, robust

** Column 3: Among male respondents
reg ix3_male   treat ppage ppage_sq $xbasic, robust





** tab03r06

** Create standardized index of all four outcome variables.

** drop observations that have all missings outcome vbls (or any missing controls)
capture drop allmiss
gen byte allmiss=1
foreach lhsvar in paid_work_yes work_hrs work_amt ss_claim_no {
    
    ** the original regression (double check that it corresponds to our table)
    quietly reg `lhsvar' treat ppage ppage_sq $xbasic, robust
    ** note whether observation is used in the regression
    quietly replace allmiss=0 if e(sample)
    }


foreach smp in total female male {
  foreach vbl of varlist paid_work_yes work_hrs work_amt ss_claim_no {
    
    ** standardize
    qui sum `vbl' if treat==0 & `smp' & allmiss==0
    qui gen std_`vbl'_`smp' = (`vbl' - r(mean))  / r(sd) if `smp' & allmiss==0

  }
  ** create the index
  qui egen ix4_`smp'=rowmean(std_*_`smp') if allmiss==0
  
  ** standardize index so that it is more easily interpretable
  qui sum ix4_`smp' if treat==0 & `smp' & allmiss==0
  qui replace ix4_`smp' = (ix4_`smp' - r(mean))  / r(sd) if `smp' & allmiss==0
  
  di ""
  di "Sum stats for `smp'"
  sum ix4_`smp' if `smp'

  di ""
  di "Sum stats for `smp' and control (should be mean 0, std 1 for std_*)"
  sum ix4_`smp' if `smp' & treat==0

  di ""
  di "Sum stats for `smp' and treatment"
  sum ix4_`smp' if `smp' & treat==1
  drop std_*_`smp' 
}
drop allmiss

** Index of three labor market outcomes

** Column 1: Among entire sample
reg ix4_total  treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ix4_female treat ppage ppage_sq $xbasic, robust

** Column 3: Among male respondents
reg ix4_male   treat ppage ppage_sq $xbasic, robust





** ---------------------------------------------------------------------------
** Table 4: Robustness of Treatment effects on objective outcome variables
** ---------------------------------------------------------------------------

** PANEL A: Rows 1-3: Did paid work in last month (2010)
** -----------------------------------------------------

** Control Means:
tab paid_work_yes female if treat==0, col

** Row 1: Baseline
** ---------------
** tab04r01

** Column 1: Among entire sample
reg paid_work_yes treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg paid_work_yes treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg paid_work_yes treat ppage ppage_sq $xbasic if female==0, robust


** Row 2: No controls
** ------------------
** tab04r02

** Column 1: Among entire sample
reg paid_work_yes treat, robust

** Column 2: Among female respondents
reg paid_work_yes treat if female==1, robust

** Column 3: Among male respondents
reg paid_work_yes treat if female==0, robust


** Row 3: Probit
** -------------
** tab04r03

** Column 1: Among entire sample
dprobit paid_work_yes treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
dprobit paid_work_yes treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
dprobit paid_work_yes treat ppage ppage_sq $xbasic if female==0, robust


** PANEL B: Row 4-6: Hours worked for pay last month (2010)
** --------------------------------------------------------

** Row 4: Baseline
** --------------
** tab04r04

** Column 1: Among entire sample
sum work_hrs if treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum work_hrs if female==1 & treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum work_hrs if female==0 & treat==0, d
reg work_hrs treat ppage ppage_sq $xbasic if female==0, robust


** Row 5: No controls
** ------------------
** tab04r05

** Column 1: Among entire sample
reg work_hrs treat, robust

** Column 2: Among female respondents
reg work_hrs treat if female==1, robust

** Column 3: Among male respondents
reg work_hrs treat if female==0, robust


** Row 6: Median regression
** ------------------------
** tab04r06

** Column 1: Among entire sample
qreg work_hrs treat

** Column 2: Among female respondents
qreg work_hrs treat if female==1

** Column 3: Among male respondents
qreg work_hrs treat if female==0


** PANEL C: Rows 7-9: Earnings last month, in dollars/month (2010)
** ---------------------------------------------------------------

** Row 7: Baseline
** ---------------
** tab04r07

** Column 1: Among entire sample
sum work_amt if treat==0, d
reg work_amt treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum work_amt if female==1 & treat==0, d
reg work_amt treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum work_amt if female==0 & treat==0, d
reg work_amt treat ppage ppage_sq $xbasic if female==0, robust


** Row 8: No controls
** ------------------
** tab04r08

** Column 1: Among entire sample
reg work_amt treat, robust

** Column 2: Among female respondents
reg work_amt treat if female==1, robust

** Column 3: Among male respondents
reg work_amt treat if female==0, robust


** Row 9: Median regression
** ------------------------
** tab04r09

** Column 1: Among entire sample
qreg work_amt treat

** Column 2: Among female respondents
qreg work_amt treat if female==1

** Column 3: Among male respondents
qreg work_amt treat if female==0


** PANEL D: Rows 10-12: Not receiving Social Security benefits in 2010
** -------------------------------------------------------------------

** Row 10: Baseline
** ----------------
** tab04r10

** Control Means:
tab ss_claim_no female if treat==0, col

** Column 1: Among entire sample
reg ss_claim_no treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ss_claim_no treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg ss_claim_no treat ppage ppage_sq $xbasic if female==0, robust


** Row 11: No controls
** -------------------
** tab04r11

** Column 1: Among entire sample
reg ss_claim_no treat, robust

** Column 2: Among female respondents
reg ss_claim_no treat if female==1, robust

** Column 3: Among male respondents
reg ss_claim_no treat if female==0, robust


** Row 12: Probit
** --------------
** tab04r12

** Column 1: Among entire sample
dprobit ss_claim_no treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
dprobit ss_claim_no treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
dprobit ss_claim_no treat ppage ppage_sq $xbasic if female==0, robust


** Row 13: Including observations under 60
**		   or that were already claiming Social Security in 2008
** -------------------------------------------------------------
** tab04r13

** Control Means:
tab ss_claim_no2 female if treat==0, col

** Column 1: Among entire sample
reg ss_claim_no2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ss_claim_no2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg ss_claim_no2 treat ppage ppage_sq $xbasic if female==0, robust





** ---------------------------------------------------------------------------
** Table 5: Selected Treatment Effects on Knowledge about Social Security
**          and on Expected Future Behavior
** ---------------------------------------------------------------------------

** PANEL A: Rows 1-2: Knowledge about incentives for working more years
** --------------------------------------------------------------------

** Row 1: SS a better deal if working more years
** ---------------------------------------------
** tab05r01

tab incentworkpos2 female if treat==0, col

** Column 1: Among entire sample
reg incentworkpos2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg incentworkpos2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg incentworkpos2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 2: Aware that SS benefits are based on some number of 
**        years with the highest earnings
** ---------------------------------------------------------
** tab05r02

tab rule35_mc_correct female if treat==0 & rule35_mc<., col

** Column 1: Among entire sample
reg rule35_mc_correct treat ppage ppage_sq $xbasic if rule35_mc<., robust

** Column 2: Among female respondents
reg rule35_mc_correct treat ppage ppage_sq $xbasic if female==1 & rule35_mc<., robust

** Column 3: Among male respondents
reg rule35_mc_correct treat ppage ppage_sq $xbasic if female==0 & rule35_mc<., robust


** PANEL B: Rows 3-5: Knowledge about incentives for claiming later
** ----------------------------------------------------------------

** Row 3: SS increases for a typical worker for delaying claiming
**        between the ages 62 and 66
** --------------------------------------------------------------
** tab05r03

** Control Means:
tab delay62_66correct female if treat==0, col

** Column 1: Among entire sample
reg delay62_66correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay62_66correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay62_66correct treat ppage ppage_sq $xbasic if female==0, robust


** Row 4: SS increases for a typical worker for delaying claiming
**        between the ages 66 and 70
** --------------------------------------------------------------
** tab05r04

** Control Means:
tab delay66_70correct female if treat==0, col

** Column 1: Among entire sample
reg delay66_70correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay66_70correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay66_70correct treat ppage ppage_sq $xbasic if female==0, robust


** Row 5: SS increases for a typical worker for delaying claiming
**        between the ages 70 and 74
** --------------------------------------------------------------
** tab05r05

** Control Means:
tab delay70_74correct female if treat==0, col

** Column 1: Among entire sample
reg delay70_74correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay70_74correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay70_74correct treat ppage ppage_sq $xbasic if female==0, robust


** PANEL C: Rows 6-7: Expected future claiming behavior
** ----------------------------------------------------

** Row 6: R's point estimate of the expected or realized SS
**        claim age (if not claiming in 2008)
** --------------------------------------------------------
** tab05r06

** Column 1: Among entire sample
sum ageclaim if treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum ageclaim if female==1 & treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum ageclaim if female==0 & treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic if female==0, robust


** Row 7: The mean of R's pdf of the expected or realized SS
**        claim age (if not claiming in 2008)
** ---------------------------------------------------------
** tab05r07

** Column 1: Among entire sample
sum expval_claim_age2 if treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum expval_claim_age2 if female==1 & treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum expval_claim_age2 if female==0 & treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic if female==0, robust


** ==========================
** 4. Appendices
** ==========================

** ---------------------------------------------------------------------------
** Appendix Table A1: Representativeness
** ---------------------------------------------------------------------------
** tabA1

** For column 1: See CPS_Sumstats.do

** For column 2: This includes everyone in the experimental group, 
**               regardless of completion of the followup survey or age inconsistencies
use Data_WithAgeInconsistency, clear
sum m_ppage female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p, sep(0)

** back to the baseline sample that excludes age inconsistancies
keep if agekeep


** For column 4: experimental group members who completed the followup survey
**               and had consistent age information
sum m_ppage female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p if completed==1, sep(0)

** Columns 3 and 5 (differences and p-values) are calculated within Excel


** ---------------------------------------------------------------------------
** Appendix Table A2: Balance of Treatment and Control Group
** ---------------------------------------------------------------------------
** tabA2

** Panel A
** Are demographics jointly significant
** Joint test
reg treat ppage agedum_* female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p if completed==1, robust


** Panel B
** Column 1: treatment group
** tabA2c1
sum  paid_work_yes work_hrs work_amt ppage agedum_* female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p  if completed==1&treat==1, sep(0)

** Column 2: control group
** tabA2c2
sum  paid_work_yes work_hrs work_amt ppage agedum_* female white black other edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr xlivpart nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p  if completed==1&treat==0, sep(0)

** The p-value on the pairwise differences between treatment and control group is calculated within
** the Excel file with the Tables


** ---------------------------------------------------------------------------
** Appendix Table A3: Treatment EFfects on Knowledge about Social Security
** ---------------------------------------------------------------------------

** PANEL A: Rows 1-2: Knowledge about longevity
** ----------------------------------------------------

** Row 1: Subjectivity probability that a typical 65 y.o.
**        [man/woman] ives to age 90 or beyond
** --------------------------------------------------------
** tabA3r01

** Column 1: Among entire sample
sum prob_liv_ind if treat==0, d
reg prob_liv_ind treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum prob_liv_ind if female==1 & treat==0, d
reg prob_liv_ind treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum prob_liv_ind if female==0 & treat==0, d
reg prob_liv_ind treat ppage ppage_sq $xbasic if female==0, robust


** Row 2: Subjective probability that at least one member of a
**        typical 65 y.o couple lives to age 90 or beyond
** --------------------------------------------------------
** tabA3r02

** Column 1: Among entire sample
sum prob_liv_cpl if treat==0, d
reg prob_liv_cpl treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum prob_liv_cpl if female==1 & treat==0, d
reg prob_liv_cpl treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum prob_liv_cpl if female==0 & treat==0, d
reg prob_liv_cpl treat ppage ppage_sq $xbasic if female==0, robust


** PANEL B: Rows 3-6: Knowledge about incentives for working more years
** --------------------------------------------------------------------

** Row 3: SS increases with more years worked (R believes that SS
**        would be lower if R had worked fewer years)
** --------------------------------------------------------------
** tabA3r03

** Control Means:
tab q3_1posincent female if treat==0, col

** Column 1: Among entire sample
reg q3_1posincent treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg q3_1posincent treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg q3_1posincent treat ppage ppage_sq $xbasic if female==0, robust


** Row 4: Percent increase in SS per additional year worked
** --------------------------------------------------------
** tabA3r04

** Column 1: Among entire sample
sum q3_1_incentpy if treat==0, d
reg q3_1_incentpy treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum q3_1_incentpy if female==1 & treat==0, d
reg q3_1_incentpy treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum q3_1_incentpy if female==0 & treat==0, d
reg q3_1_incentpy treat ppage ppage_sq $xbasic if female==0, robust


** Row 5: SS a better dea if working more years
** --------------------------------------------
** tabA3r05

** Control Means:
tab incentworkpos2 female if treat==0, col

** Column 1: Among entire sample
reg incentworkpos2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg incentworkpos2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg incentworkpos2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 6: Aware that SS benefits are based on some number
**        of years with the highest earnings
** ------------------------------------------------------
** tabA3r06

** Control Means:
tab rule35_mc_correct female if treat==0 & rule35_mc<., col

** Column 1: Among entire sample
reg rule35_mc_correct treat ppage ppage_sq $xbasic if rule35_mc<., robust

** Column 2: Among female respondents
reg rule35_mc_correct treat ppage ppage_sq $xbasic if female==1 & rule35_mc<., robust

** Column 3: Among male respondents
reg rule35_mc_correct treat ppage ppage_sq $xbasic if female==0 & rule35_mc<., robust


** PANEL C: Rows 7-13: Knowledge about incentives for claiming later
** ------------------------------------------------------------------

** Row 7: SS increases with own claim age
** --------------------------------------
** tabA3r07

** Control Means:
tab claimlater_pos female if treat==0, col

** Column 1: Among entire sample
reg claimlater_pos treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg claimlater_pos treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg claimlater_pos treat ppage ppage_sq $xbasic if female==0, robust



** Row 8: SS increases for a typical worker for delaying claiming
**        between the ages 62 and 66
** --------------------------------------------------------------
** tabA3r08

** Control Means:
tab delay62_66correct female if treat==0, col

** Column 1: Among entire sample
reg delay62_66correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay62_66correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay62_66correct treat ppage ppage_sq $xbasic if female==0, robust


** Row 9: SS increases for a typical worker for delaying claiming
**        between the ages 66 and 70
** --------------------------------------------------------------
** tabA3r09

** Control Means:
tab delay66_70correct female if treat==0, col

** Column 1: Among entire sample
reg delay66_70correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay66_70correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay66_70correct treat ppage ppage_sq $xbasic if female==0, robust


** Row 10: SS increases for a typical worker for delaying claiming
**         between the ages 70 and 74
** --------------------------------------------------------------
** tabA3r10

** Control Means:
tab delay70_74correct female if treat==0, col

** Column 1: Among entire sample
reg delay70_74correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg delay70_74correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg delay70_74correct treat ppage ppage_sq $xbasic if female==0, robust


** Row 11: Percent increase in SS per year of delay in claiming
**         between the ages of 62 and 66 for a typical worker
** --------------------------------------------------------------
** tabA3r11

** Column 1: Among entire sample
sum delayrat62_66 if treat==0, d
reg delayrat62_66 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum delayrat62_66 if female==1 & treat==0, d
reg delayrat62_66 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum delayrat62_66 if female==0 & treat==0, d
reg delayrat62_66 treat ppage ppage_sq $xbasic if female==0, robust


** Row 12: Percent increase in SS per year of delay in claiming
**         between the ages of 66 and 70 for a typical worker
** --------------------------------------------------------------
** tabA3r12

** Column 1: Among entire sample
sum delayrat66_70 if treat==0, d
reg delayrat66_70 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum delayrat66_70 if female==1 & treat==0, d
reg delayrat66_70 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum delayrat66_70 if female==0 & treat==0, d
reg delayrat66_70 treat ppage ppage_sq $xbasic if female==0, robust

** Row 13: Percent increase in SS per year of delay in claiming
**         between the ages of 70 and 74 for a typical worker
** --------------------------------------------------------------
** tabA3r13

** Column 1: Among entire sample
sum delayrat70_74,d
reg delayrat70_74 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum delayrat70_74 if female==1,d
reg delayrat70_74 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum delayrat70_74 if female==0,d
reg delayrat70_74 treat ppage ppage_sq $xbasic if female==0, robust


** Row 14: Aware of earnings test
** ------------------------------
** tabA3r14

** Control Means:
tab earntest_exist_correct2 female if treat==0, col

** Column 1: Among entire sample
reg earntest_exist_correct2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg earntest_exist_correct2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg earntest_exist_correct2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 15: Aware that a reduction in SS from earnings
**         test leads to higher benefits later
** --------------------------------------------------
** tabA3r15

** Control Means:
tab earntest_return_correct female if treat==0, col

** Column 1: Among entire sample
reg earntest_return_correct treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg earntest_return_correct treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg earntest_return_correct treat ppage ppage_sq $xbasic if female==0, robust



** ---------------------------------------------------------------------------
** Appendix Table A4: Treatment Effects on Planned or Expected Variables
** ---------------------------------------------------------------------------

** Row 1: R doe not currently work and does not plan to work in the future
** -----------------------------------------------------------------------
** tabA4r01

** Control Means
tab retired female if treat==0, col

** Column 1: Among entire sample
reg retired treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg retired treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg retired treat ppage ppage_sq $xbasic if female==0, robust


** Row 2: R's point estimate of the expeced or realized retirement age
** -------------------------------------------------------------------
** tabA4r02

** Column 1: Among entire sample
sum ageret if treat==0, d
reg ageret treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum ageret if female==1 & treat==0, d
reg ageret treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum ageret if female==0 & treat==0, d
reg ageret treat ppage ppage_sq $xbasic if female==0, robust


** Row 3: The mean of R's pdf of expected or realized retirement age
** -----------------------------------------------------------------
** tabA4r03

** Column 1: Among entire sample
sum expval_ret_age2 if treat==0, d
reg expval_ret_age2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum expval_ret_age2 if female==1 & treat==0, d
reg expval_ret_age2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum expval_ret_age2 if female==0 & treat==0, d
reg expval_ret_age2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 4: The standard deviation of R's pdf of expected
**        or realized retirement age
** -------------------------------------------------------------------
** tabA4r04

** Column 1: Among entire sample
sum sd_ret_age2 if treat==0, d
reg sd_ret_age2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum sd_ret_age2 if female==1 & treat==0, d
reg sd_ret_age2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum sd_ret_age2 if female==0 & treat==0, d
reg sd_ret_age2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 5: R reports being likely or very likely to work for pay at least 
**        part-time after starting to collect Social Security
** ---------------------------------------------------------------------
** tabA4r05

** Control Means
tab ss_work_yes female if treat==0, col

** Column 1: Among entire sample
reg ss_work_yes treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
reg ss_work_yes treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
reg ss_work_yes treat ppage ppage_sq $xbasic if female==0, robust


** Row 6: R's point estimate of the expected or realized SS claim age
** ------------------------------------------------------------------
** tabA4r06

** Column 1: Among entire sample
sum ageclaim if treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum ageclaim if female==1 & treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum ageclaim if female==0 & treat==0, d
reg ageclaim treat ppage ppage_sq $xbasic if female==0, robust


** Row 7: The mean of R's pdf of the expected or realized SS claim age
** -------------------------------------------------------------------
** tabA4r07

** Column 1: Among entire sample
sum expval_claim_age2 if treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum expval_claim_age2 if female==1 & treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum expval_claim_age2 if female==0 & treat==0, d
reg expval_claim_age2 treat ppage ppage_sq $xbasic if female==0, robust


** Row 8: The S.D. of R's pdf of the expected or realized SS claim age
** -------------------------------------------------------------------
** tabA4r08

** Column 1: Among entire sample
sum sd_claim_age2 if treat==0, d
reg sd_claim_age2 treat ppage ppage_sq $xbasic, robust

** Column 2: Among female respondents
sum sd_claim_age2 if female==1 & treat==0, d
reg sd_claim_age2 treat ppage ppage_sq $xbasic if female==1, robust

** Column 3: Among male respondents
sum sd_claim_age2 if female==0 & treat==0, d
reg sd_claim_age2 treat ppage ppage_sq $xbasic if female==0, robust


** ==========================================================================
** 5. Results that were mentioned in the text, but that are not in the tables
** ==========================================================================
** These are in the order in which they appear in the text.
** The page number refers to the page number in the draft text (prior to AEJ formatting)

** For completion rate statistics, we need the full sample that includes observations
** where the age change is inconsistent
use Data_WithAgeInconsistency, clear


** p. 9 "The completion rate of the web-tutorial was 76.8 percent for the treatment 
**       group as a whole and 91.6 percent for treatment group members who completed
**       the follow-up survey." 
tab completed_tutorial treat, col m
tab completed_tutorial treat if completed==1, col m


** p. 12 "?and the median time to complete it was 18 minutes.
tab duration

** p. 12 "While the vast majority (88.5%) of respondents completed thesurvey in April, ?"
tab tm_finish


** p. 12 "Of the 2483 members of the experimental sample, 1596 completed the follow-up
**        survey for an overall response rate of 64.3 percent."

** Coding for completion status
** 1 -- no longer part of KN panel as of April 2010
** 2 -- part of KN panel as of April 2010, but not invited 
**          to take 2010 follow-up survey
** 3 -- invited to take 2010 follow-up survey, but did not respond at all
** 4 -- started the 2010 follow-up survey, but did not complete it
** 5 -- started the 2010 follow-up survey, but did not qualify (was  
**           directed to standard close due to the if statement in Q1.8)
** 6 -- completed the 2010 follow-up survey.
tab completion_status


** p. 13 "First, only 89 percent of experimental sample members were invited to take the
**        follow-up survey because 11.4 percent of the sample had permanently left the 
**        Knowledge Networks panel or had informed Knowledge Networks that they were 
**        temporarily unavailable for surveys. The attrition rate at this first step 
**        was similar between the treatment group (11.0 percent) and control group 
**        (11.7 percent)."
tab completion_status treat, col



** p. 13 "In particular, 78.3 percent of invited treatment group members and 74.2 
**        percent of invited control group members clicked on the link."
tab completion_status treat if completion_status>=3, col



** p. 13  "Moreover, the overwhelming majority (86 percent) of invited sample members 
**         who failed to click on the link had been identified by Knowledge Networks as
**         inactive panelists prior to the date of our survey invitation. Thus over 90 
**         percent of the combined attrition that occurred in these first and second 
**         steps occurred because of respondents who were no longer actively 
**         participating in Knowledge Networks surveys, rather than from a decision 
**         to skip our particular survey."
tab active if completion_status==3

** p. 13  "Third, conditional on clicking on the link and thereby learning the topic of
**         the survey, 3.8 percent of treatment group members and 4.0 percent of control
**         group members failed to complete the survey."
tab completion_status treat if completion_status>=4, col


** p. 14  "Despite the blinded nature of the survey invitation, the overall response rate 
**         to the follow-up survey was 4.4 percentage points higher in the treatment group 
**         than the control group, and this difference is significant at the 5-percent level."
reg completed_yes treat, robust


** p. 14  "As described above, this difference can be explained by the fact that members of 
**         the treatment group were more likely to still be active Knowledge Networks panelists 
**         at the April 1st date of the survey invitation.
reg active treat, robust

** p. 14  "Among panelists active as of the invitation date, the response rates for the treatment
**         group and for the control group were not significantly different from each other. And 
**         nearly all of the differential attrition occurred before sample members were aware of
**         the survey topic. Thus, the differential response rate is unrelated to the topic or 
**         content of the follow-up survey.
reg completed_yes treat if active, robust

** FYI:
reg completed_yes treat active, robust


** Done with attrition-related statistics, so we move back to our baseline sample,
** which excludes age inconsistancies
keep if agekeep



** p. 16  "Overall, about forty percent of sample members were receiving Social Security benefits
**         in 2010, which implies that a nonnegligible fraction was combining work and benefit receipt."
tab ss_status

** p. 17  "However, because we conditioned our sample on the respondent being in a narrow age range
**         (90% of our panel is between 60 and 65 in November 2008)"
tab m_ppage
gen m_ppage6065=m_ppage>=60 & m_ppage<=65
tab m_ppage6065



** p. 20  "We know that 76 percent of individuals in the treatment group in fact participated in
**         the web-tutorial, and even among the participants the recall rate is only 10.2 percent."
tab completed_tutorial if treat 
tab recall_m_yes if completed_tutorial==1



** p. 22  "If we redefine labor force participation to include just individuals who work at least 20 
**         hours per month, we continue to find significant effects of the information treatment on 
**         labor force participation in the entire sample and in the subsample of female respondents."
gen work_hrs20=work_hrs
recode work_hrs20 min/19=0 20/max=1
reg work_hrs20 treat ppage ppage_sq $xbasic, robust
reg work_hrs20 treat ppage ppage_sq $xbasic if female==1, robust


** p. 22  "If we redefine labor force participation to only include individuals with at least $500 in
**         monthly earnings, we continue to find significant effects of the information treatment on 
**         female labor force participation, though the effect for the entire sample is not statistically 
**         significant (p-value 0.110)."
gen work_amt500=work_amt
recode work_amt500 min/499=0 500/max=1
reg work_amt500 treat ppage ppage_sq $xbasic, robust
reg work_amt500 treat ppage ppage_sq $xbasic if female==1, robust

** p. 28   "While 36 percent of controls are aware that Social Security benefits are based on the
**          years with the highest earnings, very few know that the correct number of years is 35. 
**          The median answer is 9 years and only 7 percent give the correct answer of 35."

** Only 7% know that that it takes 35 years (counting missing as wrong)
tab rule35_amount2 if rule35_mc==2

** Median answer (not using the missings): 
tab rule35_amount2 if rule35_mc==2 & rule35_amount2~=-1




** p. 31  "Only 8.7% of the men in our sample have a work history of less than 35 years, whereas 
**         this figure is 36.5% for women."
gen yrs_lt35=m_q11_4<35 if completed_yes & m_q11_4<.
tab yrs_lt female, col



** p. 32  "The treatment effect on perceived labor supply incentives, as measured by the “better deal”
**         question, is 0.119 (s.e. 0.064) for women with a work history less than 35 years but only 
**         0.033 (s.e. 0.046) for those with a work history of 35 years or more."

** Women with work history < 15 years
reg incentworkpos2 treat ppage ppage_sq $xbasic if female & yrs_lt35==1, robust
** Women with work history >= 15 years
reg incentworkpos2 treat ppage ppage_sq $xbasic if female & yrs_lt35==0, robust


** p. 32  "Though this difference is not statistically significant (p-value 0.203), ?"
gen treat_lt35 = treat * yrs_lt35
reg incentworkpos2 treat_lt35 treat yrs_lt35 ppage ppage_sq $xbasic if female, robust


** p. 32  "The treatment effect on labor force participation is 0.099 (s.e. 0.056) for women with less
**         than 35 years of work but is 0.057 (s.e. 0.041) for those with a longer work history"
** Women with work history < 15 years
reg paid_work_yes treat ppage ppage_sq $xbasic if female & yrs_lt35==1, robust
** Women with work history >= 15 years
reg paid_work_yes treat ppage ppage_sq $xbasic if female & yrs_lt35==0, robust



** p. 32  "While the difference in treatment effects is again not statistically 
**         significant (p-value 0.560), ?."
reg paid_work_yes treat_lt35 treat yrs_lt35 ppage ppage_sq $xbasic if female, robust



** p. 33. "Moreover, the intervention did not change the fraction of women who expected
**         to receive benefits on their own records (83percent)"
gen own_rec=whose_record==1 if whose_record<.
tab own_rec female, col
reg own_rec treat ppage ppage_sq $xbasic if female, robust


** save the data file for the Family-wise Error Rate Calculations
save ss_fwer, replace

log close

