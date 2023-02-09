******************************************************************************************
*Wellcome Mental Health Data Prize analysis - Sleep and parent reported SMFQ trajetcories*
******************************************************************************************

********************************************************************************
*DATA PREPARATION AND CLEANING*
********************************************************************************

*Set the working directory (where all your estimates and graphs will be saved)
cd "TO YOUR WORKING DIRECTORY"

*Call the sleep and parent reported dataset into stata
import delimited "WHEREVER YOU HAVE SAVED THE FILE/simulated data/sleep_use_dep_simulated.csv", clear 


*Convert the dataset from wide to long format
reshape long mfq_t, i(subject) j(timepoint)

*Order the datset
order ///
subject /// Subject identifier
timepoint /// Occasion number
mfq_t /// SMFQ depression response
    
*Creating a new variable called age and replacing the 'wide' values into a long format
generate age = .
replace age = age_t1 if timepoint==1
replace age = age_t2 if timepoint==2
replace age = age_t3 if timepoint==3
replace age = age_t4 if timepoint==4

*Format the new age variable to have less decimal places
format %9.2f age

*Order the new age variable to come after the mfq variable
order age, after(mfq_t)

*You check to make sure the data look like they are in the correct format
*Here we can explore 4 people
list subject timepoint mfq_t age ///
  if inlist(subject,5,7,30,34), noobs sepby(subject)
  
*General tidying of the data
rename timepoint occ // rename the timepoint variable to be occasion
rename mfq_t dep // rename the mfq_t variable to be depression

*Check the same 4 people with the new labels
list subject occ dep age ///
  if inlist(subject,5,7,30,34), noobs sepby(subject)

/*  The results will look something like this
  
  | subject   occ   dep     age |
  |-----------------------------|
  |       5     1     0    9.58 |
  |       5     2     1   11.67 |
  |       5     3     1   13.17 |
  |       5     4     1   17.00 |
  |-----------------------------|
  |       7     1     0    9.58 |
  |       7     2     1   11.67 |
  |       7     3     0   13.08 |
  |       7     4     0   16.50 |
  |-----------------------------|
  |      30     1     0    9.67 |
  |      30     2     1   11.67 |
  |      30     3     0   13.17 |
  |      30     4     3   16.50 |
  |-----------------------------|
  |      34     1     3    9.58 |
  |      34     2     2   11.83 |
  |      34     3     2   13.08 |
  |      34     4     3   16.50 |

*/

*Sort to that your data are in the right order
sort subject occ

*Create a new variable which is the number of occasions a person have completed 
by subject: egen numocc =count(dep)

*You can then choose to include or exclude people based upon the number of assessments they have completed.
*drop if numocc ==1 // e.g. you might choose this if you wanted to drop people with only one assessment
*Going forward, this example does not drop anyone, but if you choose to do this, then your results may vary.

*Order the key variables needed here
order subject occ age dep sleep_bin

********************************************************************************
*DESCRIPTIVE INFORMATION & GROWTH CURVE PREP*
********************************************************************************

* Declare data to be panel for use in later analysis
xtset subject occ

* Describe the missing depression data patterns
xtdes if dep~=.

/* You will see patterns like this - the 1st row shows that 4506 have completed every measure

     Freq.  Percent    Cum. |  Pattern
 ---------------------------+---------
     4506     48.64   48.64 |  1111
     1211     13.07   61.71 |  111.
      834      9.00   70.71 |  1...
      523      5.65   76.36 |  11..
      301      3.25   79.61 |  1.1.
      276      2.98   82.59 |  1.11
      259      2.80   85.38 |  11.1
      252      2.72   88.10 |  .1..
      239      2.58   90.68 |  .11.
      863      9.32  100.00 | (other patterns)
 ---------------------------+---------
     9264    100.00         |  XXXX

*/

*To get the depression and age stats by occassion
bysort occ: sum dep age

/* The results will show the mean depression and age scores, along with the SDs and ranges

-> occ = 1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         dep |      8,032    2.581673    3.250823          0         24
         age |      8,130    9.650738    .1303814        9.5         11

-> occ = 2

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         dep |      7,276    2.342771    3.235183          0         26
         age |      7,421    11.71823    .1360203   11.41667   13.83333

-> occ = 3

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         dep |      7,089    2.392721    3.348433          0         26
         age |      7,123    13.16057    .1828979   12.83333   16.08333

-> occ = 4

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         dep |      5,673    2.149833    3.401615          0         26
         age |      5,701    16.83833    .3608515       16.5   18.33333

*/

*Create a graph showing mean depression scores over time by age - to guide how to model the trajectories
egen mean_age = mean(age), by(occ)
egen tag = tag(occ)
egen depression = mean(dep), by(occ)
twoway connected depression mean_age if tag, sort

*The figure shows the depression scores decreasing over time, but the decline is quite small.
*We can explore whether this decline is meaningful
*And how it might vary by childhood sleep disorders

********************************************************************************
*CREATING GROWTH CURVES
********************************************************************************

*First, explore the age variable in your dataset
sum age // in this example the mean age is 12.51664

/* Your results will look this where the age is 12.51664

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         age |     28,375    12.51664    2.533991        9.5   18.33333

*/

*We will center the age variable to allow for improved convergence and interprerbility
*The new age variable (agemc) is just the age variable subtracted by the mean age
gen agemc = age-r(mean) // this creates a new variable which is the mean of the previous command
*gen agemc = age-12.51664 would also do the same thing, but the above command is used to allow for flexibility

*Then you can run the growth curve using the mixed command in stata
*First we will specify a random intercept and random slope model first

*LINEAR MODEL (one age term)

mixed dep agemc || subject: agemc, cov(uns)

/* Your results will look something like this:

--------------------------------------------------------------------------------
Mixed-effects ML regression                     Number of obs     =     28,070
Group variable: subject                         Number of groups  =      9,264
                                                Obs per group:
                                                              min =          1
                                                              avg =        3.0
                                                              max =          4
                                                Wald chi2(1)      =      51.37
Log likelihood = -70682.386                     Prob > chi2       =     0.0000

------------------------------------------------------------------------------
         dep | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
       agemc |  -.0478449   .0066756    -7.17   0.000    -.0609289   -.0347609
       _cons |   2.429048   .0280085    86.73   0.000     2.374153    2.483944
------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
subject: Unstructured        |
                  var(agemc) |   .0816241     .00607      .0705536    .0944317
                  var(_cons) |   5.113185   .1101228      4.901841    5.333642
            cov(agemc,_cons) |  -.0109771   .0176257     -.0455227    .0235686
-----------------------------+------------------------------------------------
               var(Residual) |   5.433481   .0692875      5.299363    5.570993
------------------------------------------------------------------------------
LR test vs. linear model: chi2(3) = 5369.78               Prob > chi2 = 0.0000
--------------------------------------------------------------------------------

*/

*Locally store the model estimates as m1 (for comparing aginst later models)
estimates store m1

*You can then make predictions from this model to plot figures
predict yhat_linear // predictions are often called yhat - in this case yhat_linear for the lineal model

*You can then plot the trajectory from the linear model
twoway line yhat_linear age, sort 

*One way of examining whether your model fits the data is to visually explore how the predicted line fits on the mean data
*You can explore the prediction with the mean plotted data from earlier
twoway ///
(line yhat_linear age, sort lcolor(dkorange)) ///
(connected depression mean_age if tag, sort lcolor(dknavy) mcolor(dknavy)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(1 "Linear Prediction" 2 "Descriptive") size(medium) row(1))

*NON LINEAR MODEL - QUADRATIC (two age terms)

*create a quadratic agemc term to allow for the non-linearity
gen agemc_2 = agemc^2

*We then run a similar model to the above
mixed dep agemc agemc_2 || subject: agemc agemc_2, cov(uns)

/* Your results will look something like this:

--------------------------------------------------------------------------------
Mixed-effects ML regression                     Number of obs     =     28,070
Group variable: subject                         Number of groups  =      9,264
                                                Obs per group:
                                                              min =          1
                                                              avg =        3.0
                                                              max =          4
                                                Wald chi2(2)      =      54.53
Log likelihood = -70621.116                     Prob > chi2       =     0.0000

------------------------------------------------------------------------------
         dep | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
       agemc |  -.0514857   .0073666    -6.99   0.000     -.065924   -.0370473
     agemc_2 |   .0019991   .0024927     0.80   0.423    -.0028866    .0068848
       _cons |   2.416149   .0332431    72.68   0.000     2.350994    2.481304
------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
subject: Unstructured        |
                  var(agemc) |   .0847196   .0087756      .0691534    .1037897
                var(agemc_2) |   .0053829    .000991      .0037524    .0077219
                  var(_cons) |   6.055375   .1578943      5.753683    6.372887
          cov(agemc,agemc_2) |  -.0010207   .0021312     -.0051978    .0031564
            cov(agemc,_cons) |   .0532958   .0247761      .0047356    .1018561
          cov(agemc_2,_cons) |  -.0852788   .0096971     -.1042847   -.0662728
-----------------------------+------------------------------------------------
               var(Residual) |   5.079526   .0897989      4.906537    5.258613
------------------------------------------------------------------------------
LR test vs. linear model: chi2(6) = 5491.71               Prob > chi2 = 0.0000

--------------------------------------------------------------------------------

*/

*Locally store the model estimates as m2 (for comparing aginst m1)
estimates store m2

*Like above you can then make predictions from this model to plot figures
predict yhat_quad 

*You can then plot the trajectory from the quadratic model
twoway line yhat_quad age, sort 

*One way of examining whether your model fits the data is to visually explore how the predicted line fits on the mean data
*You can explore the prediction with the mean plotted data from earlier
twoway ///
(line yhat_quad age, sort lcolor(dkorange)) ///
(connected depression mean_age if tag, sort lcolor(dknavy) mcolor(dknavy)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(1 "Quadratic Prediction" 2 "Descriptive") size(medium) row(1))

*We can also statistically compare the linear and the quadratic model using a likelihood ratio test
lrtest m1 m2

/* The likelihood ratio test suggests that m2 is better than model 1

Likelihood-ratio test
Assumption: m1 nested within m2

 LR chi2(4) = 122.54
Prob > chi2 = 0.0000

*/

*NON LINEAR MODEL - CUBIC (three age terms)

*create a cubic agemc term to allow for the further non-linearity
gen agemc_3 = agemc^3

*We then run a similar model to the above - but not allowed for a random cubic term due to convergence issues
mixed dep agemc agemc_2 agemc_3 || subject: agemc agemc_2, cov(uns)

/* Your results will look something like this:

--------------------------------------------------------------------------------
Mixed-effects ML regression                     Number of obs     =     28,070
Group variable: subject                         Number of groups  =      9,264
                                                Obs per group:
                                                              min =          1
                                                              avg =        3.0
                                                              max =          4
                                                Wald chi2(3)      =      60.80
Log likelihood = -70617.978                     Prob > chi2       =     0.0000

------------------------------------------------------------------------------
         dep | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
       agemc |   .0049005   .0236672     0.21   0.836    -.0414864    .0512875
     agemc_2 |   .0100974   .0040804     2.47   0.013        .0021    .0180949
     agemc_3 |  -.0047885   .0019102    -2.51   0.012    -.0085324   -.0010446
       _cons |   2.409988   .0333326    72.30   0.000     2.344657    2.475319
------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
subject: Unstructured        |
                  var(agemc) |   .0850892   .0087723      .0695216    .1041428
                var(agemc_2) |   .0054501   .0009916      .0038154    .0077852
                  var(_cons) |   6.058604   .1578719      5.756949    6.376065
          cov(agemc,agemc_2) |  -.0011244   .0021313     -.0053017     .003053
            cov(agemc,_cons) |   .0539662   .0247757      .0054067    .1025257
          cov(agemc_2,_cons) |  -.0855644    .009699     -.1045742   -.0665546
-----------------------------+------------------------------------------------
               var(Residual) |   5.072636    .089716      4.899808    5.251559
------------------------------------------------------------------------------
LR test vs. linear model: chi2(6) = 5496.06               Prob > chi2 = 0.0000
--------------------------------------------------------------------------------

*/

*Locally store the model estimates as m3 (for comparing aginst m2)
estimates store m3

*Like above you can then make predictions from this model to plot figures
predict yhat_cubic

*You can then plot the trajectory from the quadratic model
twoway line yhat_cubic age, sort 

*One way of examining whether your model fits the data is to visually explore how the predicted line fits on the mean data
*You can explore the prediction with the mean plotted data from earlier
twoway ///
(line yhat_cubic age, sort lcolor(dkorange)) ///
(connected depression mean_age if tag, sort lcolor(dknavy) mcolor(dknavy)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(1 "Cubic Prediction" 2 "Descriptive") size(medium) row(1))

*We can compare the quadratic model to the cubic model using a likelihood ratio test
lrtest m2 m3

/* The likelihood ratio test suggests that m3 is better than model 2

Likelihood-ratio test
Assumption: m2 nested within m3

 LR chi2(1) =   6.28
Prob > chi2 = 0.0122

*/

*The cubic model looks to be the best so we will proceed with this model going forward

********************************************************************************
*EXPLORING GROWTH CURVE INTERACTIONS - AKA HOW DO FACTORS INFLUENCE TRAJECTORIES
*EXPLORING THE IMPACT OF CHILDHOOD SLEEP PROBLEMS ON TRAJECTORIES
********************************************************************************

*We will explore interactions in the cubic model 
*This involves creating new variables which are the three agemc variables interacted with sleep problems

gen agemcxsleep = agemc*sleep_bin
gen agemc_2xsleep = agemc_2*sleep_bin
gen agemc_3xsleep = agemc_3*sleep_bin

*We then run a similar model to the above, with the main effects and age interacted with time
*But again we have not allowed for a random cubic term due to convergence issues
mixed dep agemc agemc_2 agemc_3 sleep_bin agemcxsleep agemc_2xsleep agemc_3xsleep || subject: agemc agemc_2,  cov(uns)

/* Your results will look something like this:

--------------------------------------------------------------------------------
Mixed-effects ML regression                     Number of obs     =     24,123
Group variable: subject                         Number of groups  =      7,525
                                                Obs per group:
                                                              min =          1
                                                              avg =        3.2
                                                              max =          4
                                                Wald chi2(7)      =     228.07
Log likelihood = -60159.813                     Prob > chi2       =     0.0000

-------------------------------------------------------------------------------
          dep | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
--------------+----------------------------------------------------------------
        agemc |   .0035905   .0258072     0.14   0.889    -.0469906    .0541716
      agemc_2 |   .0081841   .0043908     1.86   0.062    -.0004217      .01679
      agemc_3 |  -.0038761    .002083    -1.86   0.063    -.0079587    .0002066
    sleep_bin |   1.664222   .1690119     9.85   0.000     1.332964    1.995479
  agemcxsleep |    .128786   .1224767     1.05   0.293    -.1112639    .3688359
agemc_2xsleep |    .041132   .0208038     1.98   0.048     .0003573    .0819067
agemc_3xsleep |  -.0218509   .0098466    -2.22   0.026    -.0411498    -.002552
        _cons |   2.262234   .0366331    61.75   0.000     2.190434    2.334033
-------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
subject: Unstructured        |
                  var(agemc) |   .0865831   .0089519      .0707011    .1060327
                var(agemc_2) |   .0056455   .0010206      .0039611     .008046
                  var(_cons) |   5.825229   .1637335      5.512997    6.155145
          cov(agemc,agemc_2) |  -.0017464   .0021884     -.0060356    .0025428
            cov(agemc,_cons) |   .0977436   .0255687      .0476298    .1478574
          cov(agemc_2,_cons) |   -.094398    .010031     -.1140584   -.0747376
-----------------------------+------------------------------------------------
               var(Residual) |   4.908938    .092509      4.730931    5.093642
------------------------------------------------------------------------------
LR test vs. linear model: chi2(6) = 4767.63               Prob > chi2 = 0.0000
--------------------------------------------------------------------------------

*/

*Now we can also make predictions from this kind of model as well
*And make specific trajetcories for specifc groups
*In this case = depression trajetcories for children with sleep problems
*or = depression trajectories for children without sleep problems

*Create the predictions for each group
predictnl no_sleep_traj = /// 
  _b[dep:_cons] ///
  + _b[dep:sleep_bin]*0 /// because no sleep problems are coded as 0, we multiply this parameter by 0
  + _b[dep:agemc]*agemc ///            
  + _b[dep:agemc_2]*agemc_2 ///
  + _b[dep:agemc_3]*agemc_3 ///
  + _b[dep:agemcxsleep]*0*agemc /// because no sleep problems are coded as 0, we multiply this parameter by 0            
  + _b[dep:agemc_2xsleep]*0*agemc_2 /// because no sleep problems are coded as 0, we multiply this parameter by 0
  + _b[dep:agemc_3xsleep]*0*agemc_3 // because no sleep problems are coded as 0, we multiply this parameter by 0
 
predictnl yes_sleep_traj = /// 
  _b[dep:_cons] ///
  + _b[dep:sleep_bin]*1 /// because no sleep problems are coded as 1, we multiply this parameter by 1
  + _b[dep:agemc]*agemc ///            
  + _b[dep:agemc_2]*agemc_2 ///
  + _b[dep:agemc_3]*agemc_3 ///
  + _b[dep:agemcxsleep]*1*agemc /// because no sleep problems are coded as 1, we multiply this parameter by 1           
  + _b[dep:agemc_2xsleep]*1*agemc_2 /// because no sleep problems are coded as 1, we multiply this parameter by 1
  + _b[dep:agemc_3xsleep]*1*agemc_3 // because no sleep problems are coded as 1, we multiply this parameter by 1

*Plot these two trajectories 
twoway ///
(line no_sleep_traj age, sort lcolor(dkorange) lpattern(solid)) ///
(line yes_sleep_traj age, sort lcolor(dknavy) lpattern(solid)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(1 "No Sleep Problems" 2 "Yes Sleep Problems") size(medium) row(1))

********************************************************************************
*EXPLORING WITHIN INDIVIUDAL VARIABILITY BETWEEN THOSE WITH & WITHOUT SLEEP PROBLEMS
********************************************************************************

*We can edit the model above to show differences in within indiviudal variability between groups
*In this case is there more variability between those in the childhood sleep  disorder group
*Compared to childhood non sleep problems group 

mixed dep agemc agemc_2 agemc_3 sleep_bin agemcxsleep agemc_2xsleep agemc_3xsleep ///
|| subject: agemc agemc_2,  cov(uns) residuals(ind, by(sleep_bin)) 

/* Your results will look something like this:

--------------------------------------------------------------------------------
Mixed-effects ML regression                     Number of obs     =     24,123
Group variable: subject                         Number of groups  =      7,525
                                                Obs per group:
                                                              min =          1
                                                              avg =        3.2
                                                              max =          4
                                                Wald chi2(7)      =     175.52
Log likelihood = -60045.151                     Prob > chi2       =     0.0000

-------------------------------------------------------------------------------
          dep | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
--------------+----------------------------------------------------------------
        agemc |    .003912   .0253718     0.15   0.877    -.0458158    .0536399
      agemc_2 |   .0082057    .004309     1.90   0.057    -.0002399    .0166513
      agemc_3 |  -.0038956    .002049    -1.90   0.057    -.0079116    .0001204
    sleep_bin |    1.62188   .1999382     8.11   0.000     1.230009    2.013752
  agemcxsleep |   .1263926   .1691911     0.75   0.455    -.2052158    .4580011
agemc_2xsleep |   .0441734   .0290942     1.52   0.129    -.0128501    .1011968
agemc_3xsleep |  -.0222865   .0136324    -1.63   0.102    -.0490056    .0044326
        _cons |   2.262438   .0360588    62.74   0.000     2.191764    2.333112
-------------------------------------------------------------------------------

------------------------------------------------------------------------------
  Random-effects parameters  |   Estimate   Std. err.     [95% conf. interval]
-----------------------------+------------------------------------------------
subject: Unstructured        |
                  var(agemc) |    .080409   .0087263      .0650024    .0994673
                var(agemc_2) |   .0053002   .0009973      .0036654     .007664
                  var(_cons) |   5.669055   .1612614      5.361638    5.994099
          cov(agemc,agemc_2) |  -.0002332   .0021248     -.0043978    .0039313
            cov(agemc,_cons) |   .0976938   .0249464      .0487998    .1465878
          cov(agemc_2,_cons) |  -.0913663   .0098387     -.1106498   -.0720828
-----------------------------+------------------------------------------------
Residual: Independent,       |
    by sleep_bin             |
                   0: var(e) |   4.723356   .0904928      4.549282     4.90409
                   1: var(e) |   10.70495   .6069202      9.579114     11.9631
------------------------------------------------------------------------------
LR test vs. linear model: chi2(7) = 4996.95               Prob > chi2 = 0.0000
--------------------------------------------------------------------------------

*/

*The results show greater within individual variability in depression for people
*with childhood sleep disorders
*We can test this formally by comapring the variability scores
*We can get these from the model estimated above
matrix list e(b)

*lnsig_e:_cons is the variance for non sleep problems
*lnsig_e:_cons] + r_lns2ose:_cons is the variance for sleep problems

*We can compare them with nlcom
nlcom (exp(_b[lnsig_e:_cons])^2) - (exp(_b[lnsig_e:_cons] + _b[r_lns2ose:_cons])^2) 

*we will also now save the mixed results for later analysis
*these will be stored wherever you have set your working directory

estimates save "mixed_cubic_sleep", replace

********************************************************************************
*PLOTTING THE FINALISED TRAJECTORIES
********************************************************************************

*We can plot the finalised trajectories from the final - now with confidence intervals

drop no_sleep_traj yes_sleep_traj // from the non penultimate model

estimate use "mixed_cubic_sleep" // from the final model

*Create the predictions for each group
predictnl no_sleep_traj = /// 
  _b[dep:_cons] ///
  + _b[dep:sleep_bin]*0 /// because no sleep problems are coded as 0, we multiply this parameter by 0
  + _b[dep:agemc]*agemc ///            
  + _b[dep:agemc_2]*agemc_2 ///
  + _b[dep:agemc_3]*agemc_3 ///
  + _b[dep:agemcxsleep]*0*agemc /// because no sleep problems are coded as 0, we multiply this parameter by 0            
  + _b[dep:agemc_2xsleep]*0*agemc_2 /// because no sleep problems are coded as 0, we multiply this parameter by 0
  + _b[dep:agemc_3xsleep]*0*agemc_3 /// because no sleep problems are coded as 0, we multiply this parameter by 0
  , se(no_sleep_se) // creates the standard errors for the trajectory
 
predictnl yes_sleep_traj = /// 
  _b[dep:_cons] ///
  + _b[dep:sleep_bin]*1 /// because no sleep problems are coded as 1, we multiply this parameter by 1
  + _b[dep:agemc]*agemc ///            
  + _b[dep:agemc_2]*agemc_2 ///
  + _b[dep:agemc_3]*agemc_3 ///
  + _b[dep:agemcxsleep]*1*agemc /// because no sleep problems are coded as 1, we multiply this parameter by 1           
  + _b[dep:agemc_2xsleep]*1*agemc_2 /// because no sleep problems are coded as 1, we multiply this parameter by 1
  + _b[dep:agemc_3xsleep]*1*agemc_3 /// because no sleep problems are coded as 1, we multiply this parameter by 1
  , se(yes_sleep_se) // creates the standard errors for the trajectory

*Create confidence intervals for a more detailed trajectroy figure
gen no_sleep_lo = no_sleep_traj - 1.96*no_sleep_se // lower confidence intervals
gen no_sleep_hi = no_sleep_traj + 1.96*no_sleep_se // higher confidence intervals

gen yes_sleep_lo = yes_sleep_traj - 1.96*yes_sleep_se // lower confidence intervals
gen yes_sleep_hi = yes_sleep_traj + 1.96*yes_sleep_se // higher confidence intervals

*Plot these two trajectories with confidence intervals
twoway ///
(rarea no_sleep_lo no_sleep_hi age, sort color(gs10)) ///
(rarea yes_sleep_lo yes_sleep_hi age , sort color(gs10)) ///
(line no_sleep_traj age, sort lcolor(dkorange) lpattern(solid)) ///
(line yes_sleep_traj age, sort lcolor(dknavy) lpattern(solid)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(3 "No Sleep Problems" 4 "Yes Sleep Problems") size(medium) row(1))

********************************************************************************
*COMPARING SCORES AT DIFFERENT AGES BETWEEN TRAJECTORIES
********************************************************************************

*We can also compare what the depression scores look like at different time points
*and how these vary by the two sleep disorder groups

*For example, let's say we cant to compare scores between the two trajetcories
*at age 14

*First we get the mean of age 
sum age

/* Which will look something like this:

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
         age |     28,375    12.51664    2.533991        9.5   18.33333

*/

*Then we subtract that age from the mean age of the age variable
di 14 - 12.51664 // which is 1.48336
*Subtracting 10 from the stored mean of age would also give you the same thing
sum age
di 14 - r(mean) // which is 1.48336

*You can then sub in the age of 1.48336 to get an estimate of what the score
*at age 14 should be

*Call the saved estimates from the mixed model
estimates use "mixed_cubic_sleep"

*Then use nlcom to compare the two groups 
nlcom ///
(no_sleep_age14: _b[dep:_cons] ///
  + _b[dep:sleep_bin]*0 ///
  + _b[dep:agemc]*1.4833627 ///            
  + _b[dep:agemc_2]*1.4833627^2 ///
  + _b[dep:agemc_3]*1.4833627^3 ///
  + _b[dep:agemcxsleep]*0*1.4833627 ///            
  + _b[dep:agemc_2xsleep]*0*1.4833627^2 ///
  + _b[dep:agemc_3xsleep]*0*1.4833627^3 ///		
  )	///
(yes_sleep_age14: _b[dep:_cons] ///
  + _b[dep:sleep_bin]*1 ///
  + _b[dep:agemc]*1.4833627 ///            
  + _b[dep:agemc_2]*1.4833627^2 ///
  + _b[dep:agemc_3]*1.4833627^3 ///
  + _b[dep:agemcxsleep]*1*1.4833627 ///            
  + _b[dep:agemc_2xsleep]*1*1.4833627^2 ///
  + _b[dep:agemc_3xsleep]*1*1.4833627^3 ///			
  )	///
	, cformat(%9.3f) post // creates the scores for each group
nlcom (difference: _b[no_sleep_age14] - _b[yes_sleep_age14]), cformat(%9.2f) post // calculates the difference between them
est tab, p(%12.10g) // gets you exact p values for multiple comparison testing etc

*It is also possible to compare multiple ages quickly using loops
*Please note, this part may require further testing and is considered a beta function

sum age // sum age like before
*And then create new variables to be used in loops
gen sc10 = 10 -r(mean)
gen sc12 = 12 -r(mean)
gen sc14 = 14 -r(mean)
gen sc16 = 16 -r(mean)
gen sc18 = 18 -r(mean)

estimates use "mixed_cubic_sleep.ster" // use the estimates from the mixed command

foreach var of varlist sc10 sc12 sc14 sc16 sc18 {
nlcom ///
(no_sleep_`var': _b[dep:_cons] ///
  + _b[dep:sleep_bin]*0 ///
  + _b[dep:agemc]*`var' ///            
  + _b[dep:agemc_2]*`var'^2 ///
  + _b[dep:agemc_3]*`var'^3 ///
  + _b[dep:agemcxsleep]*0*`var' ///            
  + _b[dep:agemc_2xsleep]*0*`var'^2 ///
  + _b[dep:agemc_3xsleep]*0*`var'^3 ///		
  )	///
(yes_sleep_`var': _b[dep:_cons] ///
  + _b[dep:sleep_bin]*1 ///
  + _b[dep:agemc]*`var' ///            
  + _b[dep:agemc_2]*`var'^2 ///
  + _b[dep:agemc_3]*`var'^3 ///
  + _b[dep:agemcxsleep]*1*`var' ///            
  + _b[dep:agemc_2xsleep]*1*`var'^2 ///
  + _b[dep:agemc_3xsleep]*1*`var'^3 ///		
  )	
} // this will give you scores for each trajectory at a given age

foreach var of varlist sc10 sc12 sc14 sc16 sc18 {
nlcom (diff_`var': ///
(_b[dep:_cons] ///
  + _b[dep:sleep_bin]*0 ///
  + _b[dep:agemc]*`var' ///            
  + _b[dep:agemc_2]*`var'^2 ///
  + _b[dep:agemc_3]*`var'^3 ///
  + _b[dep:agemcxsleep]*0*`var' ///            
  + _b[dep:agemc_2xsleep]*0*`var'^2 ///
  + _b[dep:agemc_3xsleep]*0*`var'^3 ) ///
  - ///
(_b[dep:_cons] ///
  + _b[dep:sleep_bin]*1 ///
  + _b[dep:agemc]*`var' ///            
  + _b[dep:agemc_2]*`var'^2 ///
  + _b[dep:agemc_3]*`var'^3 ///
  + _b[dep:agemcxsleep]*1*`var' ///            
  + _b[dep:agemc_2xsleep]*1*`var'^2 ///
  + _b[dep:agemc_3xsleep]*1*`var'^3 ) ///		
  )
} // this will calculate the different between those scores

********************************************************************************
*PLOTTING INDIVIDUAL TRAJECTORIES FROM THE MODEL
********************************************************************************

*We can also plot individual level trajectories and explore how specifc individuals
*vary compared to the overall population

*First run the final model as before
mixed dep agemc agemc_2 agemc_3 sleep_bin agemcxsleep agemc_2xsleep agemc_3xsleep ///
|| subject: agemc agemc_2,  cov(uns) residuals(ind, by(sleep_bin)) 

*Then we use the predict command to get the level 2 residuals (which are individuals intercepts, slopes and quadratics)
predict u1 u2 u0, reffects relevel(subject) reses(u1_se u2_se u0_se) // 
 
*Person specific trajectories

*Use the estimates from the final model
estimates use "mixed_cubic_sleep"

*To create the overall trajectories for the whole sample
gen overall_traj = ///
  _b[dep:_cons]*1 ///
  + _b[dep:sleep_bin]*sleep_bin ///
  + _b[dep:agemc]*agemc ///            
  + _b[dep:agemc_2]*agemc_2 ///
  + _b[dep:agemc_3]*agemc_3 ///
  + _b[dep:agemcxsleep]*agemcxsleep ///            
  + _b[dep:agemc_2xsleep]*agemc_2xsleep ///
  + _b[dep:agemc_3xsleep]*agemc_3xsleep

*Then we add the random effects to the overall trajectory to get the persin specific trajectories 
generate individual_traj = ///
  overall_traj ///
  + u0*1///
  + u1*agemc ///
  + u2*agemc_2 

*We can visualise this by plotting a figure with every trajectory plotted, split by the sleep groups
twoway (line individual_traj age, lcolor(gs7) lwidth(vthin) lpattern(solid) connect(ascending) cmissing(n)) ///
(line no_sleep_traj age if sleep_bin==0, sort lcolor(dkorange) lpattern(solid)) ///
(line yes_sleep_traj age if sleep_bin==1, sort lcolor(dknavy) lpattern(solid)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
by(, graphregion(fcolor(white))) by(sleep_bin) subtitle(, size(zero) nobox fcolor() lcolor()) ///
legend(order(2 "No Sleep Problems" 3 "Yes Sleep Problems") size(medium) row(1))

*Or just plot individuals
*Take 4 people from each group
twoway (line individual_traj age if inlist(subject,3,5,6,7), lcolor(gs7) lwidth(vthin) lpattern(dash) connect(ascending) cmissing(n)) ///
(line individual_traj age if inlist(subject,51,104,185,221), lcolor(dkgreen) lwidth(vthin) lpattern(dash) connect(ascending) cmissing(n)) ///
(line no_sleep_traj age, sort lcolor(dkorange) lpattern(solid)) ///
(line yes_sleep_traj age, sort lcolor(dknavy) lpattern(solid)) ///
, ///
ytitle(Depressive Symptoms) ///
xtitle(Age (years)) ///
graphregion(fcolor(white)) ///
legend(order(1 "No Sleep Problem Individuals" 2 "Sleep Problem Individuals" 3 "No Sleep Problems" 4 "Yes Sleep Problems") size(medium) row(4))
