Programs and data files
=======================

"Would People Behave Differently If They Better Understood Social Security? 
 Evidence From a Field Experiment" by Jeffrey B. Liebman and Erzo F.P. Luttmer

To be published in AEJ-Policy 2014/2015.


Data files:
-----------
socsec_data.dta    -  Raw data from Knowledge Networks containing the responses to the follow-up     
                      survey (2010) as well as demographics measured at the time of the baseline
                      (2008) survey. All variables from the baseline survey start with the prefix _m
                     
cps.dta            - CPS extract of workers age 60-65 in 2008.  Extracted using IPUMScps (http://cps.ipums.org/cps/) 


Do files (run in the following order):
--------------------------------------                      
mainregressions.do - Does all the data cleaning and runs all regressions and tabulations reported in 
                     the paper except for the family-wise error rate (fwer) statistics reported and for 
                     column 1 in appendix table 1.
                     Mainregressions.do uses socsec_data.dta as an input  
                     Mainregressions.do also creates ss_fwer.dta, the data file that serves as input to fwer_shell.do

cps_sumstats.do    - Creates CPS summary statistics for column 1 in appendix table 1.
                     Uses data from cps_00002.dta  

fwer_shell.do      - Shell do file that calculates family-wise error rate (fwer) statistics
                     This shell file calls on fwer_core.do
                     This shell file uses ss_fwer.dta                
                     
fwer_core.do       - Do not run directly; will be called by fwer_shell. Does the general part of the fwer calculations.                     