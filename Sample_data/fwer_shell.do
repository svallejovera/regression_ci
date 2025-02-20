** FWER_shell.do
**
** Shell file that runs all the calculations for rows 5 and 6 for Table 3
** as well as results mentioned in the text for the family-wise error rate 
** in the article:
** "Would People Behave Differently If They Better Understood Social 
**  Security? Evidence From a Field Experiment" **** by Jeffrey B. Liebman and Erzo F.P. Luttmer
**
** To be published in AEJ-Policy 2014/2015.
**
** ===================================================================

set more 1
clear
capture log close
log using fwer.log, replace



** Set the number of iterations
global n_iter=100000

** control variables (assumed to be constant across dependent variables)
global controls ppage ppage_sq black other edudo eduhs edusc xwidow xdivorce xseparate xnevmarr xlivpart female midwest south west xhhsize2 xhhsize3



** A. Family consists of all four main outcomes (so including claiming behavior)
global family paid_work_yes work_hrs work_amt ss_claim_no

** Column 1: Entire sample
global runname "FWER_A_All"  
use ss_fwer, clear
keep treat $family $controls
do fwer_core.do

** Column 2: Females only
global runname "FWER_A_Female"
use ss_fwer, clear
keep treat $family $controls
keep if female
do fwer_core.do

** Column 3: Males only
global runname "FWER_A_Male"
use ss_fwer, clear
keep treat $family $controls 
keep if ~female
do fwer_core.do




** A. Family consists of (i) index of the three work behaviors and (ii) claiming behavior

** Column 1: Entire sample
global family ix3_total ss_claim_no
global runname "FWER_B_All" 
use ss_fwer, clear
keep treat $family $controls
do fwer_core.do

** Column 2: Females only
global family ix3_female ss_claim_no
global runname "FWER_B_Female"
use ss_fwer, clear
keep treat $family $controls 
keep if female
do fwer_core.do

** Column 3: Males only
global family ix3_male ss_claim_no
global runname "FWER_B_Male"
use ss_fwer, clear
keep treat $family $controls 
keep if ~female
do fwer_core.do


log close
