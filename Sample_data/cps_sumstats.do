capture log close
log using cps_sumstats.log, replace
clear
set more 1

** Dataset extracted from CPS using IPUMScps (http://cps.ipums.org/cps/)  
use cps

d


** Summary stastics for CPS abstract from the IPUMScps (http://cps.ipums.org/cps/)
**
** Year: 2008
** Selection: age 60-65 and working  (to match our experimental sample)
**
** Jeff Liebman and Erzo Luttmer, 10/20/2010, updated 7/16/2011


** Recode variables to match our demographic categories

** gender
tab sex
gen female=sex==2 if sex<.


** age
sum age, d

** race/ethnicity
tab race, m
tab race, sum(race)

tab hispan, m
tab hispan, sum(hispan)

gen race3=racerecode race3 100=1 200=2 300/830=3replace race3=3 if hispan>0 & hispan<.

tab race3
label def race3 1 "Non-H Wh" 2 "Non-H Bl" 3 "Other" 
label val race3 race3

gen byte white=race3==1
gen byte black=race3==2
gen byte other=1-black-white
label var white "Non-Hispanic White"
label var black "Non-Hispanic Black"
label var other "Panelist is not Non-Hispanic white or black"


** education
tab educ, mtab educ, sum(educ)

gen educ4=educrecode educ4 2/71=1 73=2 81/92=3 111/115=4
label def educ4 1 "HS dropout" 2 "HS" 3 "Some College"  4 "College plus"label val educ4 educ4tab educ4

gen byte edudo  = educ4==1
gen byte eduhs  = educ4==2
gen byte edusc  = educ4==3
gen byte educp  = educ4==4
assert (edudo+eduhs+edusc+educp)==1



** marital statustab marst, m
tab marst, sum(marst)

gen byte xmarried  = marst==1|marst==2
gen byte xwidow    = marst==5
gen byte xdivorce  = marst==4
gen byte xseparate = marst==3
gen byte xnevmarr  = marst==6
assert (xmarried+xwidow+xdivorce+xseparate+xnevmarr)==1


                  
                  
** region                  
tab region, mtab region, sum(region)gen region4=regionrecode region4 11 12=1 21 22=2 31 32 33=3 41 42=4label def region4 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label val region4 region4      
gen byte nrtheast = region4==1
gen byte midwest  = region4==2
gen byte south    = region4==3
gen byte west     = region4==4
assert (nrtheast+midwest+south+west)==1

      
          
** hhsize
tab numprec, m gen hhsize3=numprecrecode hhsize3 3/max=3

gen xhhsize1=hhsize3==1
gen xhhsize2=hhsize3==2
gen xhhsize3=hhsize3==3
assert (xhhsize1+xhhsize2+xhhsize3)==1


          
** hhincome
gen hhinc5=hhincomerecode hhinc5 min/24999=1 25000/49999=2 50000/74999=3 75000/99999=4 100000/max=5tab hhinc5

gen inc00_25  = hhinc5==1
gen inc25_50  = hhinc5==2
gen inc50_75  = hhinc5==3
gen inc75_100 = hhinc5==4
gen inc100p   = hhinc5==5
assert (inc00_25+inc25_50+inc50_75+inc75_100+inc100p)==1


** Summary statistics, complete and by gender

sum female white black other age edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p [aw=perwt]

sum female white black other age edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p [aw=perwt] if female==1

sum female white black other age edudo eduhs edusc educp xmarried xwidow xdivorce xseparate xnevmarr nrtheast midwest south west xhhsize1 xhhsize2 xhhsize3 inc00_25-inc100p [aw=perwt] if female==0




log close

