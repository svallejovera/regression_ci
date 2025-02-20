** fwer_core.do
**
** Calculate the FamilyWise Error Rate for SS experiment
** This do file must be called from a shell file that sets the following globals:
**
**
**   
** n_iter  : Number of iterations (set to 100,000 for final run)    
**           e.g. global n_iter=10000 
**
** family  : family of dependent variables   
**           e.g. global family paid_work_yes work_hrs work_amt ss_claim_no
**
** controls: control variables in the regression (assumed to be constant across dependent variables)
**           e.g global controls ppage ppage_sq black other edudo eduhs edusc xwidow xdivorce xseparate xnevmarr xlivpart female midwest south west xhhsize2 xhhsize3
** 
** Note: treatment status must be in a 0/1 variable named "treat"
**
** Data should be in memory already.
**
** Code based on Stata code by Michael Anderson
**
** Adapted by Jeff Liebman and Erzo Luttmer, 6/1/11


/*  FWE notes from Michael Anderson:
    Bonferroni is too conservative because:
        (a) No adjustment for double-counting the overlapping sets
        (b) No adjustment for dependence
        (c) No adjustment for evaluating all p-vals against the min p-val, even if the min p-val is rejected
    Sidak: Solves (a), not (b) or (c)
    Holms: Solves (a) and (c) (by removing hypotheses from the family once they are rejected), but not (b)
    Free step down resampling: Should solve (a), (b), and (c)
*/
    

** generate variables to store actual and simulated t-stats/p-vals 
** They are stored in rows.  The variable name to which they correspond is given by varname
local counter = 1
gen str20 varname = ""
gen float tstat = .
gen float act_pval = .
gen float tstatsim = .
gen float pvalsim = .
gen float pval_fwer = .

** remove observations that are excluded from all regressions so that we can sample treatment without replacement
gen byte allmiss=1
foreach lhsvar in $family {
    
    ** the original regression (double check that it corresponds to our table)
    quietly reg `lhsvar' treat $controls, robust
    ** note whether observation is used in the regression
    quietly replace allmiss=0 if e(sample)
    local counter = `counter' + 1
    }
drop if allmiss==1



** run the original regressions for all of the different outcomes tested and store the actual (observed) p-vals/t-stats
local counter = 1
foreach lhsvar in $family {
    
    ** the original regression (double check that it corresponds to our table)
    reg `lhsvar' treat $controls, robust
    quietly replace tstat = abs(_b[treat]/_se[treat]) in `counter'
    quietly replace act_pval = 2*ttail(e(N),abs(tstat)) in `counter'
    
    ** store the variable name for which the pvalues etc are stored in that row (so that data can be resorted)
    quietly replace varname = "`lhsvar'" in `counter'
    
    ** note whether observation is used in the regression
    replace allmiss=0 if e(sample)
    
    local `lhsvar'_ct_0 = 0
    local counter = `counter' + 1
    
    ** list act_pval varname if varname~=""
    
    }

** count the number of treated observations 
** to be used later when sampling with replacement
sum treat if treat
local ntreat = r(N)


** The number of variables in the family
local endvar = `counter' - 1
di "Number of hypotheses in family: `endvar'"

** generate the variable that will contain the simulated (placebo) treatments

gen byte simtreatment = .gen float simtreatment_uni = .local count = 1

** Set seed for replicability
set seed 1234567890

** run at least 100,000 iterations of the simulation; record results in p-val storage counters
while `count' <= $n_iter {
    /* in this section we assign the placebo treatments and run regressions using the placebo treatments */
	
	/* use the actual fraction treated; simulate without replacement */	quietly replace simtreatment_uni = uniform()
	gsort simtreatment_uni
	quietly replace simtreatment = (_n <= `ntreat')  
    
        
    /* a check; can be removed after testing */
    ** quietly sum simtreatment if simtreatment
    ** assert `ntreat' == r(N)

    /* sort the p-vals by the actual (observed) p-vals 
    ** (this will reorder some of the obs in your data set, but that shouldn't matter)
    */
    gsort act_pval

	quietly replace tstatsim = .
	quietly replace pvalsim = .
	
	
	foreach lhsvar of numlist 1/`endvar' {
	    local depvar = varname[`lhsvar']        quietly reg `depvar' simtreatment $controls, robust    	quietly replace tstatsim = abs(_b[simtreatment]/_se[simtreatment]) in `lhsvar'
        quietly replace pvalsim = 2*ttail(e(N),abs(tstatsim)) in `lhsvar'
    	}

    /* in this section we perform the "step down" procedure that replaces simulated p-vals with the 
    ** minimum of the set of simulated p-vals associated with outcomes that had actual p-vals greater 
    ** than or equal to the one being replaced.  For each outcome, we keep count of how many times 
    ** the ultimate simulated p-val is less than the actual observed p-val.
    */
    local countdown = `endvar'
    while `countdown' >= 1 {
        quietly replace pvalsim = min(pvalsim,pvalsim[_n+1]) in `countdown'
        local depvar = varname[`countdown']
        if pvalsim[`countdown'] <= act_pval[`countdown'] {
            local `depvar'_ct_0 = ``depvar'_ct_0' + 1
            }
        local countdown = `countdown' - 1
    	}
    
    
    /* End of similation loop */
    local count = `count' + 1
    if mod(`count',1000)==0 {
        di "Runname: $runname -- Iteration# `count' / $n_iter" 
        }
    
    }

** perform the final adjustment that ensures that the ordering to adjusted p-vals is the same as 
** the original ordering of actual p-vals.  Note that this is actually a "step up" procedure rather than a "step down"
foreach lhsvar of numlist 1/`endvar' {
    local depvar = varname[`lhsvar']
    quietly replace pval_fwer = max(round(``depvar'_ct_0'/$n_iter,.0001), pval_fwer[`lhsvar'-1]) in `lhsvar'    }

di "================================================= "
di "Results for $runname:"
di ""
list varname act_pval pval_fwer if pval_fwer<.
di ""
di "================================================= "


