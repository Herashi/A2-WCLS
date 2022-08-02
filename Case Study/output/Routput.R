> testEstimates(mood_agg_results)

Call:
  
  testEstimates(model = mood_agg_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)           1.681     0.056    30.090   819.738     0.000     0.180     0.154 
week_category_new    -0.027     0.013    -2.093    65.868     0.040     1.160     0.551 
MOODprev              0.766     0.008    99.423   539.865     0.000     0.231     0.191 

Unadjusted hypothesis test as appropriate in larger samples.

> testEstimates(mood_agg_results2)

Call:
  
  testEstimates(model = mood_agg_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        1.678     0.056    30.006   781.734     0.000     0.185     0.158 
week_category_new                 -0.028     0.013    -2.110    65.902     0.039     1.160     0.550 
MOODprev                           0.767     0.008    99.215   533.535     0.000     0.233     0.192 
week_category_new_c:MOODprev_c    -0.053     0.012    -4.425    60.007     0.000     1.287     0.577 

Unadjusted hypothesis test as appropriate in larger samples.

> # testEstimates(mood_agg_results3)
  > 
  > saveRDS(mood_agg_results, file = "direct_mood_agg_results.RDS")
> saveRDS(mood_agg_results2, file = "direct_mood_agg_results2.RDS")
> # saveRDS(mood_agg_results3, file = "direct_mood_agg_results3.RDS")
  > 
  > testEstimates(step_results)

Call:
  
  testEstimates(model = step_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)           7.683     0.185    41.593   259.610     0.000     0.371     0.276 
week_category_new     0.012     0.033     0.357   131.428     0.721     0.613     0.389 
STEP_COUNTprev        0.597     0.009    63.246   376.288     0.000     0.290     0.229 

Unadjusted hypothesis test as appropriate in larger samples.

> testEstimates(step_results2)

Call:
  
  testEstimates(model = step_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                              7.680     0.185    41.518   253.726     0.000     0.377     0.279 
week_category_new                        0.012     0.033     0.349   131.528     0.728     0.613     0.389 
STEP_COUNTprev                           0.597     0.009    63.215   363.420     0.000     0.296     0.233 
week_category_new_c:STEP_COUNTprev_c    -0.035     0.015    -2.326    99.160     0.022     0.779     0.449 

Unadjusted hypothesis test as appropriate in larger samples.

> # testEstimates(step_results3)
  > 
  > saveRDS(step_results, file = "direct_step_results.RDS")
> saveRDS(step_results2, file = "direct_step_results2.RDS")
> # saveRDS(step_results3, file = "direct_step_results3.RDS")
  > 
  > testEstimates(sleep_results)

Call:
  
  testEstimates(model = sleep_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)          11.353     0.224    50.761    59.509     0.000     1.299     0.579 
week_category_new    -0.013     0.027    -0.461    41.143     0.647     2.121     0.694 
SLEEP_COUNTprev       0.422     0.011    36.908    54.801     0.000     1.432     0.603 

Unadjusted hypothesis test as appropriate in larger samples.

> testEstimates(sleep_results2)

Call:
  
  testEstimates(model = sleep_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                              11.344     0.226    50.216    57.756     0.000     1.345     0.588 
week_category_new                        -0.013     0.027    -0.481    41.229     0.633     2.114     0.693 
SLEEP_COUNTprev                           0.423     0.012    36.545    53.373     0.000     1.479     0.611 
week_category_new_c:SLEEP_COUNTprev_c    -0.067     0.018    -3.686    51.090     0.001     1.563     0.624 

Unadjusted hypothesis test as appropriate in larger samples.